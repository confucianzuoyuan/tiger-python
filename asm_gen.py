from asm import Instruction, MoveInstruction, OperatorInstruction, LabelInstruction
from ir import IrExp, IrName, IrBinOp, BinOp, IrConst, IrMem, IrTemp, IrExpStatement, IrCall, IrSequence, IrMove, RelationalOp, IrStatement, IrCondJump, IrJump, IrLabel
from temp import Temp, RSP, RAX, RDX
from frame import WORD_SIZE, argRegisters, registers, calldefs

class InstructionGen:
    def __init__(self) -> None:
        self.instructions: list[Instruction] = []

    def emit(self, instruction: Instruction) -> None:
        self.instructions.append(instruction)

    def munchArgs(self, arguments: list[IrExp]) -> list[Temp]:
        temps: list[Temp] = []

        for register in argRegisters():
            if len(arguments) > 0:
                argument = arguments.pop(0)
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(argument)],
                    [register]
                )
                self.emit(instruction)
                temps.append(register)
            else:
                break

        instructions = [OperatorInstruction("push 's0", [self.munchExpression(argument), RSP], [RSP], None) for argument in arguments]
        instructions.reverse()

        for instruction in instructions:
            self.emit(instruction)

        return temps

    def munchExpression(self, expr: IrExp) -> Temp:
        temp = Temp()
        match expr:
            case IrName(label):
                instruction = MoveInstruction(
                    "mov 'd0, {}".format(label),
                    [],
                    [temp]
                )
                self.emit(instruction)
            case IrMem(IrBinOp(BinOp.Plus, expr, IrConst(num))) | IrMem(IrBinOp(BinOp.Plus, IrConst(num), expr)):
                instruction = MoveInstruction(
                    "mov 'd0, ['s0 + {}]".format(num),
                    [self.munchExpression(expr)],
                    [temp]
                )
                self.emit(instruction)
            case IrMem(IrConst(num)):
                instruction = MoveInstruction(
                    "mov 'd0, [{}]".format(num),
                    [],
                    [temp]
                )
                self.emit(instruction)
            case IrMem(expr):
                instruction = MoveInstruction(
                    "mov 'd0, ['s0]",
                    [self.munchExpression(expr)],
                    [temp]
                )
                self.emit(instruction)
            case IrBinOp(BinOp.Plus, expr, IrConst(num)) | IrBinOp(BinOp.Plus, IrConst(num), expr):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(expr)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "add 'd0, {}".format(num),
                    [temp],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.Minus, expr, IrConst(num)):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(expr)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "sub 'd0, {}".format(num),
                    [temp],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.Minus, IrConst(num), expr):
                instruction = MoveInstruction(
                    "mov 'd0, {}".format(num),
                    [],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "sub 'd0, 's0",
                    [self.munchExpression(expr), temp],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.Mul, expr, IrConst(num)) | IrBinOp(BinOp.Mul, IrConst(num), expr):
                instruction = MoveInstruction(
                    "mov 'd0, {}".format(num),
                    [],
                    [temp]
                )
                self.emit(instruction)
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(expr)],
                    [RAX]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "mul 's0",
                    [temp, RAX],
                    [RAX, RDX],
                    None,
                )
                self.emit(instruction)
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [RAX],
                    [temp]
                )
                self.emit(instruction)            
            case IrBinOp(BinOp.Div, left, IrConst(num)):
                instruction = MoveInstruction(
                    "mov 'd0, 0",
                    [],
                    [RDX]
                )
                self.emit(instruction)
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(left)],
                    [RAX]
                )
                self.emit(instruction)
                immediate = Temp()
                instruction = MoveInstruction(
                    "mov 'd0, {}".format(num),
                    [],
                    [immediate]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "idiv 's0",
                    [immediate, RAX, RDX],
                    [RAX, RDX],
                    None
                )
                self.emit(instruction)
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [RAX],
                    [temp]
                )
                self.emit(instruction)
            case IrBinOp(BinOp.Div, IrConst(num), expr):
                instruction = MoveInstruction(
                    "mov 'd0, 0",
                    [],
                    [RDX]
                )
                self.emit(instruction)
                instruction = MoveInstruction(
                    "mov 'd0, {}".format(num),
                    [],
                    [RAX]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "idiv 's0",
                    [self.munchExpression(expr), RAX, RDX],
                    [RAX, RDX],
                    None
                )
                self.emit(instruction)
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [RAX],
                    [temp]
                )
                self.emit(instruction)
            case IrBinOp(BinOp.And, expr, IrConst(num)) | IrBinOp(BinOp.And, IrConst(num), expr):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(expr)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "and 'd0, {}".format(num),
                    [],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.Or, expr, IrConst(num)) | IrBinOp(BinOp.Or, IrConst(num), expr):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(expr)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "or 'd0, {}".format(num),
                    [],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.ShiftLeft, expr, IrConst(num)) | IrBinOp(BinOp.ShiftLeft, IrConst(num), expr):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(expr)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "sal 'd0, {}".format(num),
                    [],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.ArithmeticShiftRight, expr, IrConst(num)) | IrBinOp(BinOp.ArithmeticShiftRight, IrConst(num), expr):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(expr)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "sar 'd0, {}".format(num),
                    [],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.ShiftRight, expr, IrConst(num)) | IrBinOp(BinOp.ShiftRight, IrConst(num), expr):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(expr)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "shr 'd0, {}".format(num),
                    [],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.Xor, expr, IrConst(num)) | IrBinOp(BinOp.Xor, IrConst(num), expr):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(expr)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "xor 'd0, {}".format(num),
                    [],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrConst(num):
                instruction = MoveInstruction(
                    "mov 'd0, {}".format(num),
                    [],
                    [temp]
                )
                self.emit(instruction)
            case IrBinOp(BinOp.Plus, left, right):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(left)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "add 'd0, 's0",
                    [self.munchExpression(right), temp],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.Minus, left, right):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(left)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "sub 'd0, 's0",
                    [self.munchExpression(right), temp],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.Mul, left, right):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(left)],
                    [RAX]
                )
                self.emit(instruction)
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(right)],
                    [RAX]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "mul 's0",
                    [temp, RAX],
                    [RAX, RDX],
                    None
                )
                self.emit(instruction)
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [RAX],
                    [temp]
                )
                self.emit(instruction)
            case IrBinOp(BinOp.Div, left, right):
                instruction = MoveInstruction(
                    "mov 'd0, 0",
                    [],
                    [RDX]
                )
                self.emit(instruction)
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(left)],
                    [RAX]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "idiv 's0",
                    [self.munchExpression(right)],
                    [RAX, RDX],
                    None
                )
                self.emit(instruction)
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [RAX],
                    [temp]
                )
                self.emit(instruction)
            case IrBinOp(BinOp.And, left, right):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(left)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "and 'd0, 's0",
                    [self.munchExpression(right)],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.Or, left, right):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(left)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "or 'd0, 's0",
                    [self.munchExpression(right)],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.ShiftLeft, left, right):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(left)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "sal 'd0, 's0",
                    [self.munchExpression(right)],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.ArithmeticShiftRight, left, right):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(left)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "sar 'd0, 's0",
                    [self.munchExpression(right)],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.ShiftRight, left, right):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(left)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "shr 'd0, 's0",
                    [self.munchExpression(right)],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.Xor, left, right):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(left)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "xor 'd0, 's0",
                    [self.munchExpression(right)],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrTemp(temp):
                return temp
            case IrCall(IrName(label), arguments):
                argumentCount = len(arguments)
                source = self.munchArgs(arguments)
                instruction = OperatorInstruction(
                    "call {}".format(label),
                    source,
                    calldefs(),
                    None
                )
                self.emit(instruction)
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [RAX],
                    [temp]
                )
                self.emit(instruction)
                argRegisterCount = len(argRegisters())
                if argumentCount > argRegisterCount:
                    stackArgCount = argumentCount - len(argRegisters())
                    instruction = OperatorInstruction(
                        "add 'd0, {}".format(stackArgCount * WORD_SIZE),
                        [],
                        [RSP],
                        None
                    )
                    self.emit(instruction)
            case IrCall(function, arguments):
                argumentCount = len(arguments)
                source = [self.munchExpression(function)]
                instruction = OperatorInstruction(
                    "call 's0",
                    source,
                    calldefs(),
                    None
                )
                self.emit(instruction)
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [RAX],
                    [temp]
                )
                self.emit(instruction)
                argRegisterCount = len(argRegisters())
                if argumentCount > argRegisterCount:
                    stackArgCount = argumentCount - len(argRegisters())
                    instruction = OperatorInstruction(
                        "add 'd0, {}".format(stackArgCount * WORD_SIZE),
                        [],
                        [RSP],
                        None
                    )
                    self.emit(instruction)
            case _:
                raise Exception('')
        return temp

    def munchStatement(self, statement: IrStatement) -> None:
        match statement:
            case IrSequence(statement1, statement2):
                self.munchStatement(statement1)
                self.munchStatement(statement2)
            case IrMove(IrMem(IrBinOp(BinOp.Plus, memoryDestination, IrConst(num))), expr) | IrMove(IrMem(IrBinOp(BinOp.Plus, IrConst(num), memoryDestination)), expr):
                instruction = MoveInstruction(
                    "mov ['s0 + {}], 's1".format(num),
                    # FIXME: might be wrong if expr is in memory as Intel might not allow a move from memory to memory.
                    [self.munchExpression(memoryDestination), self.munchExpression(expr)],
                    []
                )
                self.emit(instruction)
            case IrMove(IrMem(IrConst(num)), expr):
                instruction = MoveInstruction(
                    "mov [{}], ['s0]".format(num),
                    # FIXME: not sure move from memory to memory is allowed.
                    [self.munchExpression(expr)],
                    []
                )
                self.emit(instruction)
            case IrMove(IrMem(destination), source):
                instruction = MoveInstruction(
                    "mov ['s0], 's1",
                    [
                        self.munchExpression(destination),
                        self.munchExpression(source)
                    ],
                    []
                )
                self.emit(instruction)
            case IrMove(IrTemp(temp), source):
                match source:
                    case IrMem(IrBinOp(BinOp.Plus, expr, IrConst(num))):
                        if temp in registers():
                            instruction = MoveInstruction(
                                "mov 'd0, ['s0 + {}]".format(num),
                                [self.munchExpression(expr)],
                                [temp]
                            )
                        else:
                            instruction = MoveInstruction(
                                "mov 'd0, 's0",
                                [self.munchExpression(source)],
                                [temp]
                            )
                    case _:
                        instruction = MoveInstruction(
                            "mov 'd0, 's0",
                            [self.munchExpression(source)],
                            [temp]
                        )
                self.emit(instruction)
            case IrExpStatement(IrConst(_)):
                pass
            case IrExpStatement(exp):
                self.munchExpression(exp)
            case IrLabel(label):
                instruction = LabelInstruction(
                    "{}:".format(label),
                    label
                )
                self.emit(instruction)
            case IrJump(exp, labels):
                match exp:
                    case IrName(label):
                        instruction = OperatorInstruction(
                            "jmp {}".format(label),
                            [],
                            [],
                            labels
                        )
                        self.emit(instruction)
                    case _:
                        raise Exception('Unexpected jump expression: {}'.format(exp))
            case IrCondJump(op, left, right, trueLabel, falseLabel):
                instruction = OperatorInstruction(
                    "cmp 's0, 's1",
                    [self.munchExpression(left), self.munchExpression(right)],
                    [],
                    None
                )
                self.emit(instruction)

                opcode = None
                match op:
                    case RelationalOp.Equal: opcode = "je"
                    case RelationalOp.NotEqual: opcode = "jne"
                    case RelationalOp.LesserThan: opcode = "jl"
                    case RelationalOp.GreaterThan: opcode = "jg"
                    case RelationalOp.LesserOrEqual: opcode = "jle"
                    case RelationalOp.GreaterOrEqual: opcode = "jge"
                    case RelationalOp.UnsignedLesserThan: opcode = "jb"
                    case RelationalOp.UnsignedLesserOrEqual: opcode = "jbe"
                    case RelationalOp.UnsignedGreaterThan: opcode = "ja"
                    case RelationalOp.UnsignedGreaterOrEqual: opcode = "jae"

                instruction = OperatorInstruction(
                    "{} {}".format(opcode, trueLabel),
                    [],
                    [],
                    [falseLabel, trueLabel]
                )
                self.emit(instruction)

            case _:
                raise Exception('无法为IrStatement产生汇编代码')

    def getResult(self) -> list[Instruction]:
        return self.instructions