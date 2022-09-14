from dataclasses import dataclass, field
from typing import Optional
from symbol import Symbol

count = 1

@dataclass
class Type:
    pass

@dataclass
class IntType(Type):
    def __str__(self) -> str:
        return 'int'

    def __repr__(self) -> str:
        return self.__str__()

@dataclass
class StringType(Type):
    def __str__(self) -> str:
        return 'string'

    def __repr__(self) -> str:
        return self.__str__()

@dataclass
class RecordType(Type):
    symbol: Symbol
    fields: list[tuple[Symbol, Type]]
    unique: int = field(init=False)

    def __str__(self) -> str:
        return 'struct {}'.format(str(self.symbol))

    def __repr__(self) -> str:
        return self.__str__()
    # 通过判断Unique是否相等来判断是否是相同的Record类型
    # 加快判断速度

    def __eq__(self, other: object) -> bool:
        if isinstance(other, RecordType):
            return self.unique == other.unique
        return False

    def __post_init__(self) -> None:
        global count
        count += 1
        self.unique = count

@dataclass
class ArrayType(Type):
    type: Type
    unique: int = field(init=False)

    def __str__(self) -> str:
        return '[{}]'.format(str(self.type))

    def __repr__(self) -> str:
        return self.__str__()

    def __eq__(self, other: object) -> bool:
        if isinstance(other, ArrayType):
            return self.unique == other.unique
        return False

    def __post_init__(self) -> None:
        global count
        count += 1
        self.unique = count

@dataclass
class NilType(Type):
    def __str__(self) -> str:
        return 'nil'

    def __repr__(self) -> str:
        return self.__str__()

@dataclass
class UnitType(Type):
    def __str__(self) -> str:
        return '()'

    def __repr__(self) -> str:
        return self.__str__()

@dataclass
class NameType(Type):
    symbol: Symbol
    type: Optional[Type]

    def __str__(self) -> str:
        if self.type != None:
            return str(type)
        else:
            return 'unresolved type'
    
    def __repr__(self) -> str:
        return self.__str__()

@dataclass
class ErrorType(Type):
    def __str__(self) -> str:
        return 'type error'

    def __repr__(self) -> str:
        return self.__str__()