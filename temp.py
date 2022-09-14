from dataclasses import dataclass, field

tempCount = 0

tempMap = {}

@dataclass
class Temp:
    num: int = field(init=False)

    def __hash__(self) -> int:
        return self.num

    def __str__(self) -> str:
        if self in tempMap:
            return tempMap[self]
        return 't{}'.format(self.num)

    def __repr__(self) -> str:
        return self.__str__()

    def __post_init__(self) -> None:
        global tempCount
        tempCount += 1
        self.num = tempCount

RBP = Temp()
RSP = Temp()
RAX = Temp()
RBX = Temp()
RCX = Temp()
RDX = Temp()
RDI = Temp()
RSI = Temp()
R8  = Temp()
R9  = Temp()
R10 = Temp()
R11 = Temp()
R12 = Temp()
R13 = Temp()
R14 = Temp()
R15 = Temp()

tempMap = {
    RBP: 'rbp',
    RSP: 'rsp',
    RAX: 'rax',
    RBX: 'rbx',
    RCX: 'rcx',
    RDX: 'rdx',
    RDI: 'rdi',
    RSI: 'rsi',
    R8:  'r8',
    R9:  'r9',
    R10: 'r10',
    R11: 'r11',
    R12: 'r12',
    R13: 'r13',
    R14: 'r14',
    R15: 'r15',
}

class Label:
    pass


class NamedLabel(Label):
    def __init__(self, name : str) -> None:
        self.name = name

    def __str__(self) -> str:
        return self.name

    def __repr__(self) -> str:
        return self.__str__()

    def __eq__(self, __o: object) -> bool:
        if isinstance(__o, NamedLabel):
            return self.name == __o.name
        return False

labelCount = 0

class NumLabel(Label):
    def __init__(self) -> None:
        global labelCount
        labelCount += 1
        self.num = labelCount

    def __str__(self) -> str:
        return 'l{}'.format(self.num)

    def __repr__(self) -> str:
        return self.__str__()

    def __hash__(self) -> int:
        return self.num

    def __eq__(self, other: object) -> bool:
        if isinstance(other, NumLabel):
            return self.num == other.num
        return False