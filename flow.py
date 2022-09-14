from dataclasses import dataclass
from graph import Graph, GraphNode, GraphEntry
from temp import Temp, Label
from asm import Instruction, MoveInstruction, OperatorInstruction, LabelInstruction
from typing import Optional

@dataclass
class FlowGraphNode:
    defines: set[Temp]
    uses: set[Temp]
    isMove: bool

@dataclass
class FlowGraph:
    controlFlowGraph: Graph[FlowGraphNode]

    def nodes(self) -> list[GraphNode[FlowGraphNode]]:
        return self.controlFlowGraph.nodes


@dataclass
class GraphBuilder:
    instructions: list[Instruction]
    controlFlowGraph: Graph[FlowGraphNode]
    labelMap: dict[Label, int]
    visited: dict[int, GraphEntry]

    def build(self, currentIndex: int, predecessor: Optional[GraphEntry]) -> None:
        if currentIndex in self.visited:
            if predecessor != None:
                self.controlFlowGraph.link(predecessor, self.visited[currentIndex])
            return None

        instruction = self.instructions[currentIndex]
        isMove = False
        if isinstance(instruction, MoveInstruction):
            isMove = True

        defines: set[Temp] = set()
        if isinstance(instruction, MoveInstruction) or isinstance(instruction, OperatorInstruction):
            defines = set(instruction.destination)

        uses: set[Temp] = set()
        if isinstance(instruction, MoveInstruction) or isinstance(instruction, OperatorInstruction):
            uses = set(instruction.source)

        node = FlowGraphNode(defines, uses, isMove)

        entry = self.controlFlowGraph.insert(node)

        self.visited[currentIndex] = entry

        if predecessor != None:
            self.controlFlowGraph.link(predecessor, entry)

        if isinstance(instruction, OperatorInstruction):
            if instruction.jump != None:
                for j in instruction.jump:
                    self.build(self.labelMap[j], entry)

            if instruction.assembly.startswith('jmp '):
                return None

        if currentIndex + 1 < len(self.instructions):
            self.build(currentIndex + 1, entry)

def instructionsToGraph(instructions: list[Instruction]) -> FlowGraph:
    labelMap: dict[Label, int] = {}

    for (index, instruction) in enumerate(instructions):
        if isinstance(instruction, LabelInstruction):
            labelMap[instruction.label] = index

    controlFlowGraph: Graph[FlowGraphNode] = Graph()
    graphBuilder = GraphBuilder(instructions, controlFlowGraph, labelMap, {})

    graphBuilder.build(0, None)

    return FlowGraph(controlFlowGraph)