from dataclasses import dataclass
from temp import Label, Temp
from enum import Enum

class BinOp(Enum):
    Plus = '+'
    Minus = '-'
    Mul = '*'
    Div = '/'
    And = '&'
    Or = '|'
    ShiftLeft = '逻辑左移'
    ShiftRight = '逻辑右移'
    ArithmeticShiftRight = '算术右移'
    Xor = '异或'

class RelationalOp(Enum):
    Equal = '='
    NotEqual = '<>'
    LesserThan = '<'
    GreaterThan = '>'
    LesserOrEqual = '<='
    GreaterOrEqual = '>='
    UnsignedLesserThan = '无符号小于'
    UnsignedLesserOrEqual = '无符号小于等于'
    UnsignedGreaterThan = '无符号大于'
    UnsignedGreaterOrEqual = '无符号大于等于'

@dataclass
class IrExp:
    pass

@dataclass
class IrStatement:
    pass

@dataclass
class IrConst(IrExp):
    value: int

@dataclass
class IrError(IrExp):
    pass

@dataclass
class IrName(IrExp):
    label: Label

@dataclass
class IrTemp(IrExp):
    temp: Temp

@dataclass
class IrBinOp(IrExp):
    op: BinOp
    left: IrExp
    right: IrExp

@dataclass
class IrMem(IrExp):
    exp: IrExp

@dataclass
class IrCall(IrExp):
    func: IrExp
    args: list[IrExp]

@dataclass
class IrExpSequence(IrExp):
    stmt: IrStatement
    exp: IrExp

@dataclass
class IrMove(IrStatement):
    dst: IrExp
    src: IrExp

@dataclass
class IrExpStatement(IrStatement):
    exp: IrExp

@dataclass
class IrJump(IrStatement):
    exp: IrExp
    labels: list[Label]

@dataclass
class IrCondJump(IrStatement):
    '''
    op: RelationalOp
    left: IrExp
    right: IrExp
    trueLabel: Label
    falseLabel: Label
    '''
    op: RelationalOp
    left: IrExp
    right: IrExp
    trueLabel: Label
    falseLabel: Label

@dataclass
class IrSequence(IrStatement):
    stmt1: IrStatement
    stmt2: IrStatement

@dataclass
class IrLabel(IrStatement):
    label: Label

    def __eq__(self, other: object) -> bool:
        if isinstance(other, IrLabel):
            return self.label == other.label
        return False