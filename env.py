from dataclasses import dataclass
from temp import Label, NamedLabel
from tiger_data_type import Type, IntType, StringType, UnitType
from symbol import Symbol, SymbolTable
from escape import DepthEscape
from typing import Optional
from frame import Level, LevelAccess, outermost

@dataclass
class Entry:
    pass

@dataclass
class FunEntry(Entry):
    external: bool
    label: Label
    level: Level
    parameters: list[Type]
    result: Type

@dataclass
class VarEntry(Entry):
    access: LevelAccess
    typ: Type

@dataclass
class ErrorEntry(Entry):
    pass

class Environment:
    def __init__(self, escapeEnv: SymbolTable[DepthEscape]) -> None:
        self.typeEnv = SymbolTable[Type]([], {})
        self.varEnv = SymbolTable[Entry]([], {})
        self.escapeEnv = escapeEnv
        self.typeEnv.enter(Symbol('int'), IntType())
        self.typeEnv.enter(Symbol('string'), StringType())

        for (k, v) in self.externalFunctions().items():
            self.addFunction(k, v[0], v[1])

    def __str__(self) -> str:
        return '类型环境：' + str(self.typeEnv) + '\n' + '值环境：' + str(self.varEnv)

    def __repr__(self) -> str:
        return self.__str__()

    def addFunction(self, name: str, parameters: list[Type], result: Type) -> None:
        entry = FunEntry(True, NamedLabel(name), outermost(), parameters, result)
        self.varEnv.enter(Symbol(name), entry)

    def beginScope(self) -> None:
        self.typeEnv.beginScope()
        self.varEnv.beginScope()

    def endScope(self) -> None:
        self.typeEnv.endScope()
        self.varEnv.endScope()

    def enterType(self, symbol: Symbol, typ: Type) -> None:
        self.typeEnv.enter(symbol, typ)

    def enterVar(self, symbol: Symbol, data: Entry) -> None:
        self.varEnv.enter(symbol, data)

    def lookEscape(self, symbol: Symbol) -> bool:
        result = self.escapeEnv.look(symbol)
        if isinstance(result, DepthEscape):
            return result.escape
        else:
            raise Exception('expect escape')


    def lookType(self, symbol: Symbol) -> Optional[Type]:
        result = self.typeEnv.look(symbol)
        if isinstance(result, Type):
            return result
        else:
            return None

    def lookVar(self, symbol: Symbol) -> Optional[Entry]:
        result = self.varEnv.look(symbol)
        if isinstance(result, Entry):
            return result
        else:
            return None

    def replaceType(self, symbol: Symbol, typ: Type) -> None:
        self.typeEnv.replace(symbol, typ)

    def typeName(self, symbol: Symbol) -> str:
        return self.typeEnv.name(symbol)

    def varName(self, symbol: Symbol) -> str:
        return self.varEnv.name(symbol)

    def externalFunctions(self) -> dict[str, tuple[list[Type], Type]]:
        functions: dict[str, tuple[list[Type], Type]] = {}
        functions['print'] = ([StringType()], UnitType())
        functions['printi'] = ([IntType()], UnitType())
        functions['flush'] = ([], UnitType())
        functions['getchar'] = ([], StringType())
        functions['ord'] = ([StringType()], IntType())
        functions['chr'] = ([IntType()], StringType())
        functions['size'] = ([StringType()], IntType())
        functions['substring'] = ([StringType(), IntType()], StringType())
        functions['concat'] = ([StringType(), StringType()], StringType())
        functions['not'] = ([IntType()], IntType())
        functions['exit'] = ([IntType()], UnitType())
        functions['stringEqual'] = ([StringType(), StringType()], IntType())
        functions['malloc'] = ([IntType()], IntType())
        functions['initArray'] = ([IntType(), IntType()], IntType())
        return functions