from ir import IrBinOp, IrMem, IrCondJump, IrConst, IrCall, IrExp, IrExpSequence, IrExpStatement, IrJump, IrLabel, IrName, IrMove, IrSequence, IrStatement, IrTemp, BinOp, RelationalOp
from frame import WORD_SIZE, Label, Level, fp, allocLocal, LevelAccess, externalCall, Frame, returnValue
from syntax import Operator
from typing import Optional
from temp import Label, NumLabel, Temp
from dataclasses import dataclass

def toIrOp(op: Operator) -> BinOp:
    match op:
        case Operator.Plus: return BinOp.Plus
        case Operator.Minus: return BinOp.Minus
        case Operator.Times: return BinOp.Mul
        case Operator.And: return BinOp.And
        case Operator.Divide: return BinOp.Div
        case Operator.Or: return BinOp.Or
        case _:
            raise Exception(str(op) + '不是二元运算符')

def toIrRelOp(op: Operator) -> RelationalOp:
    match op:
        case Operator.Equal: return RelationalOp.Equal
        case Operator.Ge: return RelationalOp.GreaterOrEqual
        case Operator.Gt: return RelationalOp.GreaterThan
        case Operator.Le: return RelationalOp.LesserOrEqual
        case Operator.Lt: return RelationalOp.LesserThan
        case Operator.Neq: return RelationalOp.NotEqual
        case _:
            raise Exception(str(op) + '不是二元关系运算符')

def arraySubscript(var: IrExp, subsciprt: IrExp) -> IrExp:
    return IrMem(IrBinOp(
        BinOp.Plus,
        var,
        IrBinOp(
            BinOp.Mul,
            subsciprt,
            IrConst(WORD_SIZE)
        )
    ))

def binaryOper(op: Operator, left: IrExp, right: IrExp) -> IrExp:
    return IrBinOp(toIrOp(op), left, right)

def fieldAccess(var: IrExp, fieldIndex: int) -> IrExp:
    return IrMem(IrBinOp(
        BinOp.Plus,
        var,
        IrConst(WORD_SIZE * fieldIndex)
    ))

def unit() -> IrExp:
    return IrConst(0)

def goto(label: Label) -> IrExp:
    return IrExpSequence(IrJump(IrName(label), [label]), unit())

def functionCall(label: Label, args: list[IrExp], parentLevel: Level, currentLevel: Level) -> IrExp:
    if currentLevel == parentLevel:
        frame = currentLevel.current
        args.append(frame.exp(frame.formals[-1], IrTemp(fp())))
    elif currentLevel.parent == parentLevel:
        args.append(IrTemp(fp()))
    else:
        functionLevel = parentLevel
        var = IrTemp(fp())
        while True:
            if currentLevel.parent != None:
                if currentLevel.parent == functionLevel:
                    break
            frame = functionLevel.current
            var = frame.exp(frame.frameFormals()[-1], var)
            if functionLevel.parent != None:
                functionLevel = functionLevel.parent
            else:
                break
    return IrCall(IrName(label), args)

def num(number: int) -> IrExp:
    return IrConst(number)

def ifExpression(testExpr: IrExp, ifExpr: IrExp, elseExpr: Optional[IrExp], level: Level) -> IrExp:
    result = allocLocal(level, False)
    trueLabel = NumLabel()
    falseLabel = NumLabel()
    endLabel = NumLabel()
    frame = level.current
    result = frame.exp(result[1], IrTemp(fp()))
    return IrExpSequence(
        IrSequence(
            IrCondJump(RelationalOp.Equal, testExpr, IrConst(1), trueLabel, falseLabel),
            IrSequence(
                IrLabel(trueLabel),
                IrSequence(
                    IrMove(result, ifExpr),
                    IrSequence(
                        IrJump(IrName(endLabel), [endLabel]),
                        IrSequence(
                            IrLabel(falseLabel),
                            IrSequence(
                                IrMove(result, elseExpr if elseExpr != None else unit()),
                                IrLabel(endLabel)
                            )
                        )
                    )
                )
            )
        ),
        result
    )

def stringEquality(oper: Operator, left: IrExp, right: IrExp) -> IrExp:
    exp = externalCall('stringEqual', [left, right])
    match oper:
        case Operator.Equal:
            return exp
        case Operator.Neq:
            return IrBinOp(BinOp.Minus, IrConst(1), exp)
        case _:
            raise Exception('未预期的运算符' + str(oper))

def varDec(access: LevelAccess, value: IrExp) -> IrStatement:
    varLevel = access[0]
    frame = varLevel.current
    return IrMove(frame.exp(access[1], IrTemp(fp())), value)

def varDecs(variables: list[IrStatement], body: IrExp) -> IrExp:
    if len(variables) == 0:
        return body

    if len(variables) == 1:
        return IrExpSequence(variables[0], body)

    statements = IrSequence(variables[0], variables[1])
    for var in variables[2:]:
        statements = IrSequence(statements, var)
    return IrExpSequence(statements, body)

def whileLoop(doneLabel: Label, testExpr: IrExp, body: IrExp) -> IrExp:
    testLabel = NumLabel()
    afterCheckLabel = NumLabel()
    return IrExpSequence(
        IrSequence(
            IrSequence(
                IrLabel(testLabel),
                IrSequence(
                    IrSequence(
                        IrCondJump(RelationalOp.NotEqual, testExpr, IrConst(1), doneLabel, afterCheckLabel),
                        IrLabel(afterCheckLabel)
                    ),
                    IrSequence(
                        IrExpStatement(body),
                        IrJump(IrName(testLabel), [testLabel])
                    )
                )
            ),
            IrLabel(doneLabel)
        ),
        unit()
    )

def simpleVar(access: LevelAccess, level: Level) -> IrExp:
    functionLevel = level
    varLevel = access[0]
    frame = level.current
    var = IrTemp(fp())
    while functionLevel.current != varLevel.current:
        var = frame.exp(functionLevel.current.frameFormals()[-1], var)
        if functionLevel.parent != None:
            functionLevel = functionLevel.parent
        else:
            raise Exception('function level should have a parent')
    var = frame.exp(access[1], var)
    return var

def recordCreate(fields: list[IrExp]) -> IrExp:
    if len(fields) == 0:
        return unit()

    result = IrTemp(Temp())
    sequence = IrSequence(
        IrMove(result, externalCall('malloc', [IrConst(len(fields) * WORD_SIZE)])),
        IrMove(result, fields[0])
    )

    for i in range(1, len(fields)):
        sequence = IrSequence(
            sequence,
            IrMove(
                IrMem(IrBinOp(BinOp.Plus, result, IrConst((i + 1) * WORD_SIZE))),
                fields[i]
            )
        )

    return IrExpSequence(sequence, result)

def relationalOper(op: Operator, left: IrExp, right: IrExp, level: Level) -> IrExp:
    result = allocLocal(level, False)
    trueLabel = NumLabel()
    falseLabel = NumLabel()
    endLabel = NumLabel()
    frame = level.current
    result = frame.exp(result[1], IrTemp(fp()))
    return IrExpSequence(
        IrSequence(
            IrCondJump(toIrRelOp(op), left, right, trueLabel,falseLabel),
            IrSequence(
                IrLabel(trueLabel),
                IrSequence(
                    IrMove(result, IrConst(1)),
                    IrSequence(
                        IrJump(IrName(endLabel), [endLabel]),
                        IrSequence(
                            IrLabel(falseLabel),
                            IrSequence(
                                IrMove(result, IrConst(0)),
                                IrLabel(endLabel)
                            )
                        )
                    )
                )
            )
        ),
        result
    )

@dataclass
class Fragment:
    pass

@dataclass
class FunctionFragment(Fragment):
    body: IrStatement
    frame: Frame

@dataclass
class StrFragment(Fragment):
    label: Label
    value: str

class Gen:
    def __init__(self) -> None:
        self.fragments: list[Fragment] = []

    def getResult(self) -> list[Fragment]:
        return self.fragments

    def procEntryExit(self, level: Level, body: IrExp) -> None:
        body_ = IrMove(IrTemp(returnValue()), body)
        self.fragments.append(FunctionFragment(body_, level.current))

    def stringLiteral(self, string: str) -> IrExp:
        label = NumLabel()
        self.fragments.append(StrFragment(label, string))
        return IrName(label)