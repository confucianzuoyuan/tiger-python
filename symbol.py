from typing import Generic, Optional, TypeVar

class Symbol:
    def __init__(self, name: str) -> None:
        self.name = name

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Symbol):
            return self.name == other.name
        return False

    def __str__(self) -> str:
        return self.name

    def __repr__(self) -> str:
        return self.__str__()

T = TypeVar('T')

class SymbolTable(Generic[T]):
    def __init__(self, stack: list[list[str]], table: dict[str, list[T]]) -> None:
        self.stack = stack
        self.table = table
        self.beginScope()

    def __str__(self) -> str:
        return str(self.stack) + '\n' + str(self.table)

    def __repr__(self) -> str:
        return self.__str__()

    def beginScope(self) -> None:
        self.stack.append([])

    def endScope(self) -> None:
        for symbol in self.stack[len(self.stack) - 1]:
            bindings = self.table[symbol]
            bindings.pop()
        self.stack.pop()

    def enter(self, symbol: Symbol, data: T) -> None:
        try:
            bindings = self.table[str(symbol)]
            bindings.append(data)
        except:
            self.table[str(symbol)] = [data]
        currentBindings = self.stack[len(self.stack) - 1]
        currentBindings.append(str(symbol))

    def look(self, symbol: Symbol) -> Optional[T]:
        try:
            l = self.table[str(symbol)]
            return l[len(l) - 1]
        except:
            return None

    def name(self, symbol: Symbol) -> str:
        return str(symbol)

    def replace(self, symbol: Symbol, data: T) -> None:
        try:
            bindings = self.table[str(symbol)]
            bindings.pop()
            bindings.append(data)
        except:
            self.table[str(symbol)] = [data]