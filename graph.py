from dataclasses import dataclass
from typing import Generic, TypeVar

@dataclass
class GraphEntry:
    index: int

R = TypeVar('R')

class GraphNode(Generic[R]):
    def __init__(self, element: R) -> None:
        self.element = element
        self.predecessors: list[GraphEntry] = []
        self.successors: list[GraphEntry] = []

class Graph(Generic[R]):
    def __init__(self) -> None:
        self.nodes: list[GraphNode[R]] = []

    def insert(self, node: R) -> GraphEntry:
        index = len(self.nodes)
        self.nodes.append(GraphNode(node))
        return GraphEntry(index)

    def link(self, node1: GraphEntry, node2: GraphEntry) -> None:
        self.nodes[node1.index].successors.append(node2)
        self.nodes[node2.index].predecessors.append(node1)