from temp import Temp, Label, NamedLabel, NumLabel, R10, R11, R12, R13, R14, R15, R8, R9, RAX, RBP, RBX, RCX, RDI, RDX, RSI, RSP
from ir import IrBinOp, IrExp, IrCall, IrExpStatement, IrMove, IrName, IrConst, IrMem, IrSequence, IrStatement, IrTemp, BinOp
from dataclasses import dataclass
from typing import Optional, TypeVar
from asm import Instruction, MoveInstruction, OperatorInstruction, LabelInstruction, Subroutine

@dataclass
class Access:
    pass

Level = TypeVar('Level', bound='Level')
LevelAccess = tuple[Level, Access]

POINTER_SIZE = 8

WORD_SIZE = 8

@dataclass
class InFrame(Access):
    offset: int

@dataclass
class InReg(Access):
    temp: Temp

class Frame:
    def __init__(self, name: Label, formals: list[bool]) -> None:
        self.name = name
        self.pointer = 0
        self.formals = [self.allocLocal(i) for i in formals]

    def __str__(self) -> str:
        return 'name=' + str(self.name) + ', pointer=' + str(self.pointer) + ', formals=' + str(self.formals)

    def __repr__(self) -> str:
        return self.__str__()

    def allocLocal(self, escape: bool) -> Access:
        if escape:
            self.pointer -= POINTER_SIZE
            return InFrame(self.pointer)
        else:
            return InReg(Temp())

    def frameFormals(self) -> list[Access]:
        return self.formals

    def frameName(self) -> Label:
        return self.name

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Frame):
            return self.name == other.name
        return False

    def procEntryExit1(self, statement: IrStatement) -> IrStatement:
        startStatements: list[IrStatement] = []
        endStatements: list[IrStatement] = []

        savedRegisterLocations: list[IrExp] = []

        for register in calleeSavedRegisters():
            local_ = Temp()
            memory = IrTemp(local_)
            savedRegisterLocations.append(memory)
            startStatements.append(IrMove(memory, IrTemp(register)))

        arg_registers = argRegisters()
        argRegistersLen = len(arg_registers)
        for (formal, arg_register) in zip(self.formals, arg_registers):
            destination = self.exp(formal, IrTemp(fp()))
            startStatements.append(IrMove(destination, IrTemp(arg_register)))
        for (index, formal) in enumerate(self.formals[argRegistersLen:]):
            destination = self.exp(formal, IrTemp(fp()))
            startStatements.append(IrMove(destination, IrMem(IrBinOp(BinOp.Plus, IrTemp(fp()), IrConst(WORD_SIZE * (index + 2))))))

        for (register, location) in zip(calleeSavedRegisters(), savedRegisterLocations):
            endStatements.append(IrMove(IrTemp(register), location))
        endStatement = IrExpStatement(IrConst(0))
        for statement_ in endStatements:
            endStatement = IrSequence(endStatement, statement_)

        for new_statement in reversed(startStatements):
            statement = IrSequence(new_statement, statement)

        return IrSequence(statement, endStatement)

    def procEntryExit3(self, body: list[Instruction]) -> Subroutine:
        stack_size = -self.pointer
        if stack_size % 16 != 0:
            stack_size = (stack_size & ~0xF) + 0x10

        return Subroutine(
            '''{}:
    push rbp
    mov rbp, rsp
    sub rsp, {}
            '''.format(self.name, stack_size),
            body,
            '''
    leave
    ret
            '''
        )

    def exp(self, access: Access, stackFrame: IrExp) -> IrExp:
        match access:
            case InFrame(offset):
                return IrMem(IrBinOp(BinOp.Plus, stackFrame, IrConst(offset)))
            case InReg(reg):
                return IrTemp(reg)
            case _:
                raise Exception('非法access访问类型')

def argRegisters() -> list[Temp]:
    return [RDI, RSI, RDX, RCX, R8, R9]

def calleeSavedRegisters() -> list[Temp]:
    return [RBX, RBP, R12, R13, R14, R15]

def specialRegisters() -> list[Temp]:
    return [RAX, RSP]

def callerSavedRegisters() -> list[Temp]:
    return [R10, R11]

def returnValue() -> Temp:
    '''
    x64将返回值保存在rax寄存器中
    '''
    return RAX

def registers() -> list[Temp]:
    registers = argRegisters()
    registers.append(returnValue())
    registers = registers + calleeSavedRegisters() + specialRegisters() + callerSavedRegisters()
    return registers

def registerCount() -> int:
    return len(registers()) - len([RSP, RBP])

def fp() -> Temp:
    '''
    返回帧指针，x64的帧指针是在rbp寄存器中保存。
    '''
    return RBP

def calldefs() -> list[Temp]:
    registers = callerSavedRegisters()
    registers += argRegisters()
    registers.append(returnValue())
    return registers

def externalCall(name: str, arguments: list[IrExp]) -> IrExp:
    return IrCall(IrName(NamedLabel(name)), arguments)

@dataclass
class Level:
    current: Frame
    parent: Optional[Level]

    def __eq__(self, __o: object) -> bool:
        if isinstance(__o, Level):
            return self.current == __o.current
        return False

    def formals(self) -> list[LevelAccess]:
        return [(self, access) for access in self.current.frameFormals()]

def outermost() -> Level:
    return Level(Frame(NumLabel(), []), None)

def newLevel(parent: Level, name: Label, formals: list[bool]) -> Level:
    return Level(Frame(name, formals + [True]), parent)

def alloc_local(level: Level, escape: bool) -> LevelAccess:
    frameLocal = level.current.allocLocal(escape)
    return (level, frameLocal)

def procEntryExit2(instructions: list[Instruction]) -> list[Instruction]:
    source = calleeSavedRegisters()
    source += specialRegisters()
    instruction = OperatorInstruction(
        "",
        source,
        [],
        []
    )
    instructions.append(instruction)

    for instruction in instructions:
        match instruction:
            case LabelInstruction(_):
                pass
            case MoveInstruction(_, destination, _) | OperatorInstruction(_, destination, _, _):
                destination.append(RBP)
                destination.append(RSP)
                break
            case _:
                raise Exception('')

    for instruction in reversed(instructions):
        match instruction:
            case LabelInstruction(_):
                pass
            case MoveInstruction(_, _, source) | OperatorInstruction(_, _, source, _):
                source.append(RBP)
                source.append(RSP)
                break
            case _:
                raise Exception('')

    return instructions