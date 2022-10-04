from dataclasses import dataclass
from temp import Temp
from graph import Graph, GraphNode, GraphEntry
from flow import FlowGraph

@dataclass
class InterferenceGraph:
    graph: Graph[Temp]
    tempNodes: dict[Temp, GraphEntry]
    moveList: dict[Temp, set[tuple[Temp, Temp]]]
    workListMoves: set[tuple[Temp, Temp]]

    def nodes(self) -> list[GraphNode[Temp]]:
        return self.graph.nodes

    def show(self) -> None:
        nodes = self.graph.nodes
        for node in self.graph.nodes:
            name = str(node.element)
            print('Node: {}'.format(name))
            for neighbor in node.predecessors:
                name = str(nodes[neighbor.index].element)
                print('<<< {}'.format(name))
            for neighbor in node.successors:
                name = str(nodes[neighbor.index].element)
                print('>>> {}'.format(name))

def interferenceGraph(graph: FlowGraph) -> InterferenceGraph:
    liveIn: dict[int, set[Temp]] = {}
    liveOut: dict[int, set[Temp]] = {}

    workListMoves: set[tuple[Temp, Temp]] = set()

    newLiveIn: dict[int, set[Temp]] = {}
    newLiveOut: dict[int, set[Temp]] = {}

    while True:
        for (index, node) in enumerate(graph.nodes()):
            newLiveIn[index] = liveIn.get(index, set())
            newLiveOut[index] = liveOut.get(index, set())
            
            set_ = node.element.uses
            if index not in liveOut:
                liveOut[index] = set()
            out = liveOut[index]
            set_ = set_ | (out - node.element.defines)
            liveIn[index] = set_

            set_: set[Temp] = set()
            for successor in node.successors:
                if successor.index not in liveIn:
                    liveIn[successor.index] = set()
                inSet = liveIn[successor.index]
                set_ = set_ | inSet
            liveOut[index] = set_

        if newLiveIn == liveIn and newLiveOut == liveOut:
            break

    interferenceGraph_: Graph[Temp] = Graph()
    tempNodes: dict[Temp, GraphEntry] = {}
    moveList: dict[Temp, set[tuple[Temp, Temp]]] = {}

    for (index, node) in enumerate(graph.nodes()):
        for define in node.element.defines:
            defineNode = interferenceGraph_.insert(define)
            tempNodes[define] = defineNode
            for temp in liveOut[index]:
                tempNode = interferenceGraph_.insert(temp)
                tempNodes[temp] = tempNode
                interferenceGraph_.link(defineNode, tempNode)
        
        if node.element.isMove:
            if len(node.element.defines) > 0:
                define = list(node.element.defines)[0]
                if len(node.element.uses) > 0:
                    use = list(node.element.uses)[0]
                    workListMoves.add((define, use))

                    for temp in (list(node.element.defines) + list(node.element.uses)):
                        if temp not in moveList:
                            moveList[temp] = set([(define, use)])
                        else:
                            moveList[temp].add((define, use))

    return InterferenceGraph(interferenceGraph_, tempNodes, moveList, workListMoves)