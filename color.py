from temp import Temp, tempMap
from frame import registerCount, registers, Frame, fp
from ir import IrMove, IrTemp, IrExp
from flow import instructionsToGraph
from graph import GraphNode
from liveness import InterferenceGraph, interferenceGraph
from asm import Instruction, MoveInstruction, LabelInstruction, OperatorInstruction
from asm_gen import InstructionGen

class RegisterAllocator:
    def __init__(self, moveList: dict[Temp, set[tuple[Temp, Temp]]], workListMoves: set[tuple[Temp, Temp]]) -> None:
        self.activeMoves: set[tuple[Temp, Temp]] = set()
        self.adjacencyList: dict[Temp, set[Temp]] = {}
        self.adjacencySet: set[tuple[Temp, Temp]] = set()
        self.alias: dict[Temp, Temp] = {}
        self.coalescedMoves: set[tuple[Temp, Temp]] = set()
        self.coalescedNodes: set[Temp] = set()
        self.coloredNodes: set[Temp] = set()
        self.constrainedMoves: set[tuple[Temp, Temp]] = set()
        self.degree: dict[Temp, int] = {}
        self.freezeWorklist: set[Temp] = set()
        self.frozenMoves: set[tuple[Temp, Temp]] = set()
        self.moveList: dict[Temp, set[tuple[Temp, Temp]]] = moveList
        self.precolored: dict[Temp, str] = tempMap
        self.registerCount: int = registerCount()
        self.selectStatck: list[Temp] = []
        self.simplifyWorkList: set[Temp] = set()
        self.spillNodes: list[Temp] = []
        self.spillWorkList: set[Temp] = set()
        self.worklistMoves: set[tuple[Temp, Temp]] = workListMoves

    def addEdge(self, u: Temp, v: Temp) -> None:
        if (u, v) not in self.adjacencySet and u != v:
            self.adjacencySet.add((u, v))
            self.adjacencySet.add((v, u))
            if u not in self.precolored:
                if u not in self.adjacencyList:
                    self.adjacencyList[u] = set()
                self.adjacencyList[u].add(v)
                if u not in self.degree:
                    self.degree[u] = 0
                self.degree[u] = self.degree[u] + 1
            if v not in self.precolored:
                if v not in self.adjacencyList:
                    self.adjacencyList[v] = set()
                self.adjacencyList[v].add(u)
                if v not in self.degree:
                    self.degree[v] = 0
                self.degree[v] = self.degree[v] + 1

    def addWorkList(self, u: Temp) -> None:
        if u not in self.precolored and not self.moveRelated(u) and self.degree[u] < self.registerCount:
            if u in self.freezeWorklist:
                self.freezeWorklist.remove(u)
            self.simplifyWorkList.add(u)

    def adjacent(self, temp: Temp) -> set[Temp]:
        if temp not in self.adjacencyList:
            self.adjacencyList[temp] = set()
        return self.adjacencyList[temp] - (set(self.selectStatck) | self.coalescedNodes)

    def allocate(self, initial: list[Temp], nodes: list[GraphNode[Temp]]) -> dict[Temp, Temp]:
        self.build(nodes)
        self.makeWorkList(initial)

        while len(self.simplifyWorkList) != 0 or \
              len(self.spillWorkList) != 0 or \
              len(self.worklistMoves) != 0 or \
              len(self.freezeWorklist) != 0:
            if len(self.simplifyWorkList) != 0:
                self.simplify()
            elif len(self.worklistMoves) != 0:
                self.coalesce()
            elif len(self.freezeWorklist) != 0:
                self.freeze()
            elif len(self.spillWorkList) != 0:
                self.selectSpill()

        return self.assignColors()

    def assignColors(self) -> dict[Temp, Temp]:
        colors: dict[Temp, Temp] = {}
        for precolored in self.precolored.keys():
            colors[precolored] = precolored
        while len(self.selectStatck) > 0:
            temp = self.selectStatck.pop()
            okColors = set(registers())

            for neighbor in self.adjacencyList[temp]:
                neighbor = self.getAlias(neighbor)
                if neighbor in self.coloredNodes or neighbor in self.precolored:
                    if neighbor in colors and colors[neighbor] in okColors:
                        okColors.remove(colors[neighbor])

            if len(okColors) > 0:
                color = okColors.pop()
                self.coloredNodes.add(temp)
                colors[temp] = color
            else:
                self.spillNodes.append(temp)

        for node in self.coalescedNodes:
            color = colors.get(self.getAlias(node), None)
            if color != None:
                colors[node] = color
        return colors

    def build(self, nodes: list[GraphNode[Temp]]) -> None:
        for node in nodes:
            temp = node.element
            for predecessor in node.predecessors:
                self.addEdge(temp, nodes[predecessor.index].element)
            for successor in node.successors:
                self.addEdge(temp, nodes[successor.index].element)

        for node in self.precolored:
            if node not in self.degree:
                self.degree[node] = 1000000
            else:
                self.degree[node] = 0

    def coalesce(self) -> None:
        mov = self.worklistMoves.pop()
        (x, y) = mov
        x = self.getAlias(x)
        y = self.getAlias(y)
        if y in self.precolored:
            (u, v) = (y, x)
        else:
            (u, v) = (x, y)

        nodes = self.adjacent(u) | self.adjacent(v)

        if u == v:
            self.coalescedMoves.add(mov)
            self.addWorkList(u)
        elif v in self.precolored or (u, v) in self.adjacencySet:
            self.constrainedMoves.add(mov)
            self.addWorkList(u)
            self.addWorkList(v)
        elif u in self.precolored and \
             all([self.ok(temp, u) for temp in self.adjacent(v)]) or \
             u not in self.precolored and \
             self.conservative(nodes):
            self.coalescedMoves.add(mov)
            self.combine(u, v)
            self.addWorkList(u)
        else:
            self.activeMoves.add(mov)

    def combine(self, u: Temp, v: Temp) -> None:
        if v in self.freezeWorklist:
            self.freezeWorklist.remove(v)
        elif v in self.spillWorkList:
            self.spillWorkList.remove(v)

        self.coalescedNodes.add(v)
        self.alias[v] = u
        if v not in self.moveList:
            self.moveList[v] = set()
        nodes = self.moveList[v]
        if u not in self.moveList:
            self.moveList[u] = set()
        self.moveList[u] |= nodes

        moves: set[Temp] = set()
        moves.add(v)
        self.enableMoves(moves)

        nodes = self.adjacent(v)
        for temp in nodes:
            self.addEdge(temp, u)
            self.decrementDegree(temp)

        if self.degree[u] >= self.registerCount and u in self.freezeWorklist:
            self.freezeWorklist.remove(u)
            self.spillWorkList.add(u)

    def simplify(self) -> None:
        temp = self.simplifyWorkList.pop()
        self.selectStatck.append(temp)

        for neighbor in self.adjacent(temp):
            self.decrementDegree(neighbor)

    def freeze(self) -> None:
        u = self.freezeWorklist.pop()
        self.simplifyWorkList.add(u)
        self.freezeMoves(u)

    def freezeMoves(self, u: Temp) -> None:
        for mov in self.nodeMoves(u):
            (x, y) = mov
            if self.getAlias(y) == self.getAlias(u):
                v = self.getAlias(x)
            else:
                v = self.getAlias(y)
            self.activeMoves.remove(mov)
            self.frozenMoves.add(mov)
            if len(self.nodeMoves(v)) == 0 and self.degree[v] < self.registerCount:
                self.freezeWorklist.remove(v)
                self.simplifyWorkList.add(v)

    def getAlias(self, node: Temp) -> Temp:
        if node in self.coalescedNodes:
            return self.getAlias(self.alias[node])
        else:
            return node

    def nodeMoves(self, temp: Temp) -> set[tuple[Temp, Temp]]:
        if temp not in self.moveList:
            self.moveList[temp] = set()
        return self.moveList[temp] & (self.activeMoves | self.worklistMoves)

    def conservative(self, nodes: set[Temp]) -> bool:
        k = 0
        for node in nodes:
            if self.degree[node] >= self.registerCount:
                k += 1
        return k < self.registerCount

    def decrementDegree(self, temp: Temp) -> None:
        degree = self.degree[temp]
        self.degree[temp] = self.degree[temp] - 1

        if degree == self.registerCount:
            nodes = self.adjacent(temp)
            nodes.add(temp)
            self.enableMoves(nodes)
            self.spillWorkList.remove(temp)
            if self.moveRelated(temp):
                self.freezeWorklist.add(temp)
            else:
                self.simplifyWorkList.add(temp)

    def enableMoves(self, nodes: set[Temp]) -> None:
        for node in nodes:
            for mov in self.nodeMoves(node):
                if mov in self.activeMoves:
                    self.activeMoves.remove(mov)
                    self.worklistMoves.add(mov)

    def moveRelated(self, temp: Temp) -> bool:
        return len(self.nodeMoves(temp)) != 0

    def spillCost(self, node: Temp) -> int:
        return -self.degree[node]

    def ok(self, temp: Temp, u: Temp) -> bool:
        return self.degree[temp] < self.registerCount or \
               temp in self.precolored or \
               (temp, u) in self.adjacencySet

    def makeWorkList(self, initial: list[Temp]) -> None:
        for n in initial:
            if n not in self.degree:
                self.degree[n] = 0
            degree = self.degree[n]
            if degree >= self.registerCount:
                self.spillWorkList.add(n)
            elif self.moveRelated(n):
                self.freezeWorklist.add(n)
            else:
                self.simplifyWorkList.add(n)

    def selectSpill(self) -> None:
        '''
        spill: 溢出
        '''
        temp = min(self.spillWorkList, key=self.spillCost)
        self.spillWorkList.remove(temp)
        self.simplifyWorkList.add(temp)
        self.freezeMoves(temp)

def color(interferenceGraph: InterferenceGraph, initial: list[Temp]) -> tuple[dict[Temp, Temp], list[Temp], set[Temp], set[Temp]]:
    nodes = interferenceGraph.nodes()
    allocator = RegisterAllocator(
        interferenceGraph.moveList,
        interferenceGraph.workListMoves
    )
    allocation = allocator.allocate(initial, nodes)
    return (
        allocation,
        allocator.spillNodes,
        allocator.coloredNodes,
        allocator.coloredNodes
    )

def alloc(instructions: list[Instruction], frame: Frame) -> list[Instruction]:
    precolored = tempMap
    initial: list[Temp] = []
    for instruction in instructions:
        match instruction:
            case LabelInstruction(_):
                pass
            case MoveInstruction(_,source,destination) | OperatorInstruction(_,source,destination,_):
                for d in destination:
                    if d not in precolored:
                        initial.append(d)
                for s in source:
                    if s not in precolored:
                        initial.append(s)
            case _:
                raise Exception('')

    return allocate(instructions, initial, frame)

def allocate(instructions: list[Instruction], initial: list[Temp], frame: Frame) -> list[Instruction]:
    flowGraph = instructionsToGraph(instructions)
    interferenceGraph_ = interferenceGraph(flowGraph)
    (allocation, spills, coloredNodes, coalescedNodes) = color(interferenceGraph_, initial)
    if len(spills) == 0:
        return replaceAllocation(instructions, allocation)
    else:
        (instructions, newTemps) = rewriteProgram(instructions, spills, frame)
        initial = list(coloredNodes | newTemps | coalescedNodes)
        return allocate(instructions, initial, frame)

def replaceAllocation(instructions: list[Instruction], allocation: dict[Temp, Temp]) -> list[Instruction]:
    for instruction in instructions:
        match instruction:
            case LabelInstruction(_):
                pass
            case MoveInstruction(_,source,destination) | OperatorInstruction(_,source,destination,_):
                for (i,d) in enumerate(destination):
                    if d in allocation:
                        destination[i] = allocation[d]
                for (i,s) in enumerate(source):
                    if s in allocation:
                        source[i] = allocation[s]
            case _:
                raise Exception('')

    instructions_: list[Instruction] = []
    for instruction in instructions:
        match instruction:
            case MoveInstruction(assembly,source,destination):
                if not (assembly == "mov 'd0, 's0" and destination[0] == source[0]):
                    instructions_.append(instruction)
            case _:
                instructions_.append(instruction)

    return instructions_

def rewriteProgram(instructions: list[Instruction], spills: list[Temp], frame: Frame) -> tuple[list[Instruction], set[Temp]]:
    memory: dict[Temp, IrExp] = {}
    newTemps: set[Temp] = set()
    for spill in spills:
        local = frame.allocLocal(True)
        exp = frame.exp(local, IrTemp(fp()))
        memory[spill] = exp
        newTemps.add(spill)

    gen = InstructionGen()

    for instruction in instructions:
        match instruction:
            case LabelInstruction(_):
                gen.emit(instruction)
            case MoveInstruction(_,source,destination) | OperatorInstruction(_,source,destination,_):
                if next((d for d in destination if d in spills), None) != None:
                    spill = next((s for s in source if s in spills), None)
                    if spill != None:
                        temp = gen.munchExpression(memory[spill])
                        gen.munchStatement(IrMove(IrTemp(spill), IrTemp(temp)))
                        newTemps.add(temp)
                    else:
                        spill = next(d for d in destination if d in spills)
                    gen.emit(instruction)
                    gen.munchStatement(IrMove(memory[spill], IrTemp(spill)))
                elif next((s for s in source if s in spills), None) != None:
                    spill = next(s for s in source if s in spills)
                    temp = gen.munchExpression(memory[spill])
                    gen.munchStatement(IrMove(IrTemp(spill), IrTemp(temp)))
                    newTemps.add(temp)
                    gen.emit(instruction)
                else:
                    gen.emit(instruction)
            case _:
                raise Exception('')


    return (gen.getResult(), newTemps)