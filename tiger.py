from lexer import Lexer
from parser import Parser
from env import Environment
from semant import SemanticAnalyzer
from symbol import Symbol
from gen import FunctionFragment, StrFragment
from canon import linearize, basicBlocks, traceSchedule
from asm_gen import InstructionGen
from frame import procEntryExit2
from color import alloc
from escape import findEscapes

def to_nasm(string: str) -> str:
    result = "'"
    for c in string:
        if c == '\n':
            result += "', 10, '"
        elif c == '\t':
            result += "', 9, '"
        else:
            result += c
    result += "'"
    return result

source = """
let type tree = {key: string, left: tree, right: tree}
    function prettyprint(tree: tree): string =
        let var output := ""
            function write(s: string) =
                output := concat(output, s)
            function show(n: int, t: tree) =
                let function indent(s: string) =
                    (for i := 0 to n
                        do write(" ");
                        output := concat(output, s);
                        write("\n"))
                in
                    if t = nil then
                        indent(".")
                    else (
                        indent(t.key);
                        show(n + 4, t.left);
                        show(n + 4, t.right)
                    )
                end
        in
            show(0, tree);
            output
        end
in
    print(prettyprint(tree { key = "5", left = tree {
        key = "2", left = tree {
            key = "1", left = nil, right = nil
        }, right = tree {
            key = "3", left = nil, right = nil
        }
    }, right = tree {
        key = "7", left = tree {
            key = "6", left = nil, right = nil
        }, right = tree {
            key = "10", left = nil, right = nil
        }
    }}))
end
"""
lexer = Lexer(source)
tokens = lexer.scanTokens()
parser = Parser(tokens)
ast = parser.parse()
escapeEnv = findEscapes(ast)
env = Environment(escapeEnv)
sematic = SemanticAnalyzer(env)
mainSymbol = Symbol('main')
fragments = sematic.analyze(mainSymbol, ast)
print("global main\n")

for function_name in env.externalFunctions():
    print("extern {}".format(function_name))

print()
print("section .data")
print("    align 2")

for fragment in fragments:
    match fragment:
        case StrFragment(label, string):
            print("    {}: db {}, 0".format(label, to_nasm(string)))
        case _:
            pass

print("\nsection .text")

for fragment in fragments:
    match fragment:
        case FunctionFragment(body,frame):
            body = frame.procEntryExit1(body)
            statements = linearize(body)
            (basicBlocks_, doneLabel) = basicBlocks(statements)
            statements = traceSchedule(basicBlocks_, doneLabel)
            generator = InstructionGen()
            for statement in statements:
                generator.munchStatement(statement)
            instructions = generator.getResult()
            instructions = procEntryExit2(instructions)
            instructions = alloc(instructions, frame)
            subroutine = frame.procEntryExit3(instructions)
            print("    {}".format(subroutine.prolog))
            for instruction in subroutine.body:
                print("    {}".format(instruction))
            print("    {}".format(subroutine.epilog))
        case StrFragment(_, _):
            pass
        case _:
            raise Exception('????????????????????????')