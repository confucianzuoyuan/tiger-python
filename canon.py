from ir import IrStatement, IrExp, IrConst, IrSequence, IrExpStatement, IrName, IrCall, IrExpSequence, IrBinOp, IrCondJump, IrMove, IrJump, IrLabel, IrMem, IrTemp, RelationalOp
from temp import Temp, Label, NumLabel
from typing import Callable
from enum import Enum

def commute(stmt: IrStatement, expr: IrExp) -> bool:
    match (stmt, expr):
        case (IrExpStatement(IrConst(_)),_):
            return True
        case (_, IrName(_)):
            return True
        case (_, IrConst(_)):
            return True
        case _:
            return False

def appendTwoStatement(statement1: IrStatement, statement2: IrStatement) -> IrStatement:
    match (statement1, statement2):
        case (IrExpStatement(IrConst(_)), _):
            return statement2
        case (_, IrExpStatement(IrConst(_))):
            return statement1
        case (_, _):
            return IrSequence(statement1, statement2)

def linearize(statement: IrStatement) -> list[IrStatement]:
    statement = doStatement(statement)

    def linear(stmt: IrStatement, result: list[IrStatement]):
        if isinstance(stmt, IrSequence):
            linear(stmt.stmt1, result)
            linear(stmt.stmt2, result)
        else:
            result.append(stmt)

    result: list[IrStatement] = []
    linear(statement, result)
    return result

def doStatement(statement: IrStatement) -> IrStatement:
    match statement:
        case IrSequence(statement1, statement2):
            return appendTwoStatement(doStatement(statement1), doStatement(statement2))
        case IrJump(expr, labels):
            return reorderStatement1(expr, lambda e: IrJump(e, labels))
        case IrCondJump(op, left, right, trueLabel, falseLabel):
            return reorderStatement2(left, right, lambda l, r: IrCondJump(op, l, r, trueLabel, falseLabel))
        case IrMove(IrTemp(temp), IrCall(function, arguments)):
            exprs = [function] + arguments
            return reorderStatement(exprs, lambda es : IrMove(IrTemp(temp), IrCall(es.pop(0), es)))
        case IrMove(IrTemp(temp), expr):
            return reorderStatement1(expr, lambda e: IrMove(IrTemp(temp), e))
        case IrMove(IrMem(mem), expr):
            return reorderStatement2(mem, expr, lambda m, e: IrMove(IrMem(m), e))
        case IrMove(IrExpSequence(statement, expr1), expr2):
            return doStatement(IrSequence(statement, IrMove(expr1, expr2)))
        case IrExpStatement(IrCall(function, arguments)):
            exprs = [function] + arguments
            return reorderStatement(exprs, lambda es : IrExpStatement(IrCall(es.pop(0), es)))
        case IrExpStatement(expr):
            return reorderStatement1(expr, lambda e: IrExpStatement(e))
        case _:
            return statement

def doExpression(expr: IrExp) -> tuple[IrStatement, IrExp]:
    match expr:
        case IrBinOp(op, left, right):
            return reorderExpression2(left, right, lambda l, r: IrBinOp(op, l, r))
        case IrMem(expr):
            return reorderExpression1(expr, lambda e: IrMem(e))
        case IrExpSequence(statement, expr):
            statements1 = doStatement(statement)
            (statements2, expr_) = doExpression(expr)
            return (
                appendTwoStatement(statements1, statements2),
                expr_
            )
        case IrCall(function, arguments):
            exprs = [function] + arguments
            return reorderExpression(exprs, lambda es : IrCall(es.pop(0), es))
        case _:
            return (IrExpStatement(IrConst(0)), expr)

def reorder1(expr: IrExp) -> tuple[IrStatement, IrExp]:
    return doExpression(expr)

def reorder2(expr1: IrExp, expr2: IrExp) -> tuple[IrStatement, IrExp, IrExp]:
    if isinstance(expr1, IrCall):
        temp = Temp()
        return reorder2(
            IrExpSequence(
                IrMove(IrTemp(temp), expr1),
                IrTemp(temp)
            ),
            expr2
        )
    
    (statements1, expr1) = doExpression(expr1)
    (statements2, expr2) = doExpression(expr2)
    if commute(statements2, expr1):
        return (IrSequence(statements1, statements2), expr1, expr2)
    else:
        temp = Temp()
        statements = IrSequence(statements1, IrSequence(IrMove(IrTemp(temp), expr1), statements2))
        return (statements, IrTemp(temp), expr2)

def reorder(exprs: list[IrExp]) -> tuple[IrStatement, list[IrExp]]:
    if len(exprs) == 0:
        return (IrExpStatement(IrConst(0)), [])
    
    if isinstance(exprs[0], IrCall):
        temp = Temp()
        function = exprs.pop(0)
        exprs.insert(0, IrExpSequence(IrMove(IrTemp(temp), function), IrTemp(temp)))
        return reorder(exprs)

    (statements, expr1) = doExpression(exprs.pop(0))
    (statements2, expr2) = reorder(exprs)

    if commute(statements2, expr1):
        expr2.insert(0, expr1)
        return (appendTwoStatement(statements, statements2), expr2)
    else:
        temp = Temp()
        statements_ = appendTwoStatement(
            statements,
            appendTwoStatement(
                IrMove(IrTemp(temp), expr1),
                statements2
            )
        )
        expr2.insert(0, IrTemp(temp))
        return (statements_, expr2)

def reorderExpression1(expr: IrExp, builder: Callable[[IrExp], IrExp]) -> tuple[IrStatement, IrExp]:
    (statements, expr) = reorder1(expr)
    return (statements, builder(expr))

def reorderExpression2(expr1: IrExp, expr2: IrExp, builder: Callable[[IrExp, IrExp], IrExp]) -> tuple[IrStatement, IrExp]:
    (statements, expr1, expr2) = reorder2(expr1, expr2)
    return (statements, builder(expr1, expr2))

def reorderExpression(exprs: list[IrExp], builder: Callable[[list[IrExp]], IrExp]) -> tuple[IrStatement, IrExp]:
    (statements, exprs_) = reorder(exprs)
    return (statements, builder(exprs_))

def reorderStatement1(expr: IrExp, builder: Callable[[IrExp], IrStatement]) -> IrStatement:
    (statements, expr) = reorder1(expr)
    return IrSequence(statements, builder(expr))

def reorderStatement2(expr1: IrExp, expr2: IrExp, builder: Callable[[IrExp, IrExp], IrStatement]) -> IrStatement:
    (statements, expr1_, expr2_) = reorder2(expr1, expr2)
    return IrSequence(statements, builder(expr1_, expr2_))

def reorderStatement(exprs: list[IrExp], builder: Callable[[list[IrExp]], IrStatement]) -> IrStatement:
    (statements, exprs_) = reorder(exprs)
    return IrSequence(statements, builder(exprs_))

# 调整线性化后的IR

def negateCondition(op: RelationalOp) -> RelationalOp:
    match op:
        case RelationalOp.Equal: return RelationalOp.NotEqual
        case RelationalOp.GreaterOrEqual: return RelationalOp.LesserThan
        case RelationalOp.GreaterThan: return RelationalOp.LesserOrEqual
        case RelationalOp.LesserOrEqual: return RelationalOp.GreaterThan
        case RelationalOp.LesserThan: return RelationalOp.GreaterOrEqual
        case RelationalOp.NotEqual: return RelationalOp.Equal
        case RelationalOp.UnsignedGreaterOrEqual: return RelationalOp.UnsignedLesserThan
        case RelationalOp.UnsignedGreaterThan: return RelationalOp.UnsignedLesserOrEqual
        case RelationalOp.UnsignedLesserOrEqual: return RelationalOp.UnsignedGreaterThan
        case RelationalOp.UnsignedLesserThan: return RelationalOp.UnsignedGreaterOrEqual

def basicBlocks(statements: list[IrStatement]) -> tuple[list[list[IrStatement]], Label]:
    '''
    将语句序列转换成基本块组成的列表
    基本块也是语句序列
    '''
    done = NumLabel()

    class State(Enum):
        Label = 1
        InBlock = 2

    state = State.Label
    basicBlocks: list[list[IrStatement]] = []
    for statement in statements:
        if state == State.Label:
            state = State.InBlock
            if isinstance(statement, IrLabel):
                basicBlocks.append([statement])
                continue
            else:
                basicBlocks.append([IrLabel(NumLabel())])
        
        if state == State.InBlock:
            match statement:
                case IrLabel(label):
                    lastBB = basicBlocks[len(basicBlocks) - 1]
                    lastBB.append(IrJump(IrName(label), [label]))
                    basicBlocks.append([statement])
                case IrJump(_) | IrCondJump(_):
                    lastBB = basicBlocks[len(basicBlocks) - 1]
                    lastBB.append(statement)
                    state = State.Label
                case _:
                    lastBB = basicBlocks[len(basicBlocks) - 1]
                    lastBB.append(statement)

    if state == State.InBlock:
        lastBB = basicBlocks[len(basicBlocks) - 1]
        lastBB.append(IrJump(IrName(done), [done]))

    return (basicBlocks, done)

def traceSchedule(basicBlocks: list[list[IrStatement]], doneLabel: Label) -> list[IrStatement]:
    labelMapping: dict[Label, int] = {}
    labelMapping[doneLabel] = 10000000
    for (index, basic_block) in enumerate(basicBlocks):
        if isinstance(basic_block[0], IrLabel):
            labelMapping[basic_block[0].label] = index
        else:
            raise Exception("label as first statement of basic block")

    marks: set[int] = set()
    traces: list[list[int]] = []
    currentTrace: list[int] = []

    for (index, basicBlock) in enumerate(basicBlocks):
        currentTrace = []
        while index not in marks:
            marks.add(index)
            currentTrace.append(index)

            last = basicBlock[len(basicBlock) - 1]
            if isinstance(last, IrCondJump):
                if labelMapping[last.falseLabel] in marks:
                    index = labelMapping[last.falseLabel]
                elif labelMapping[last.trueLabel] in marks:
                    index = labelMapping[last.trueLabel]
            elif isinstance(last, IrJump):
                for label in last.labels:
                    if labelMapping[label] in marks:
                        index = labelMapping[label]
                        break
            else:
                raise Exception('Expected jump as last statement of basic blocks')
        
        if len(currentTrace) != 0:
            traces.append(currentTrace)

    statements: list[IrStatement] = []

    for trace in traces:
        for index in trace:
            traceStatements = basicBlocks[index]
            basicBlocks[index] = []
            for statement in traceStatements:
                statements.append(statement)

    newStatements: list[IrStatement] = []

    try:
        current = statements.pop(0)
    except:
        current = None

    while current != None:
        match current:
            case IrCondJump(op, left, right, trueLabel, falseLabel):
                next = statements.pop(0)
                if next == IrLabel(trueLabel):
                    newStatements.append(IrCondJump(
                        negateCondition(op),
                        left,
                        right,
                        falseLabel,
                        trueLabel
                    ))
                elif next == IrLabel(falseLabel):
                    newStatements.append(IrCondJump(
                        op,
                        left,
                        right,
                        trueLabel,
                        falseLabel
                    ))
                else:
                    newFalse = NumLabel()
                    newStatements.append(IrCondJump(
                        op,
                        left,
                        right,
                        trueLabel,
                        newFalse
                    ))
                    newStatements.append(IrLabel(newFalse))
                    newStatements.append(IrJump(
                        IrName(falseLabel),
                        [falseLabel]
                    ))
                newStatements.append(next)
            case IrJump(expr, labels):
                match expr:
                    case IrName(label):
                        if len(labels) == 1 and labels[0] == label:
                            if len(statements) > 0:
                                front = statements[0]
                                if isinstance(front, IrLabel):
                                    if front.label == label:
                                        current = statements.pop(0)
                                        continue

                        newStatements.append(IrJump(IrName(label), labels))
                    case _:
                        newStatements.append(IrJump(expr, labels))
            case _:
                newStatements.append(current)
        try:
            current = statements.pop(0)
        except:
            current = None

    newStatements.append(IrLabel(doneLabel))

    return newStatements