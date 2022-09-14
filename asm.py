from dataclasses import dataclass
from typing import Optional
from temp import Temp
from temp import Label

@dataclass
class Instruction:
    pass

@dataclass
class OperatorInstruction(Instruction):
    '''
    assembly: str
    source: list[Temp]
    destination: list[Temp]
    jump: Optional[list[Label]]
    '''
    assembly: str
    source: list[Temp]
    destination: list[Temp]
    jump: Optional[list[Label]]

    def __str__(self) -> str:
        result = self.assembly
        for (index, temp) in enumerate(self.destination):
            result = result.replace("'d{}".format(index), str(temp))
        for (index, temp) in enumerate(self.source):
            result = result.replace("'s{}".format(index), str(temp))
        return result

@dataclass
class LabelInstruction(Instruction):
    assembly: str
    label: Label

    def __str__(self) -> str:
        return self.assembly

@dataclass
class MoveInstruction(Instruction):
    '''
    assembly: str
    source: list[Temp]
    destination: list[Temp]
    '''
    assembly: str
    source: list[Temp]
    destination: list[Temp]

    def __str__(self) -> str:
        result = self.assembly
        for (index, temp) in enumerate(self.destination):
            result = result.replace("'d{}".format(index), str(temp))
        for (index, temp) in enumerate(self.source):
            result = result.replace("'s{}".format(index), str(temp))
        return result

@dataclass
class Subroutine:
    prolog: str
    body: list[Instruction]
    epilog: str