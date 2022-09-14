from syntax import AstArrayExpression, AstAssignExpression, AstBreakExpression, AstCallExpression, AstDeclaration, AstExpression, AstFieldVar, AstFunctionDeclarationList, AstIfExpression, AstIntExpression, AstLetExpression, AstNilExpression, AstOperatorExpression, AstRecordExpression, AstSequenceExpression, AstSimpleVar, AstStringExpression, AstVar, AstSubscriptVar, AstTypeDeclarationList, AstVariableDeclaration, AstVariableExpression, AstWhileExpression, Operator
from dataclasses import dataclass
from symbol import SymbolTable

@dataclass
class DepthEscape:
    depth: int
    escape: bool

class EscapeFinder:
    def __init__(self) -> None:
        self.env = SymbolTable[DepthEscape]([], {})

    def visitBinaryOp(self, left: AstExpression, right: AstExpression, depth: int) -> None:
        self.visitExp(left, depth)
        self.visitExp(right, depth)

    def visitExp(self, expr: AstExpression, depth: int) -> None:
        match expr:
            case AstArrayExpression(init, size):
                self.visitExp(size, depth)
                self.visitExp(init, depth)
            case AstAssignExpression(expr, var):
                self.visitVar(var, depth)
                self.visitExp(expr, depth)
            case AstBreakExpression():
                pass
            case AstCallExpression(args,_):
                for arg in args:
                    self.visitExp(arg, depth)
            case AstIfExpression(else_,test,then):
                self.visitExp(test, depth)
                self.visitExp(then, depth)
                if else_ != None:
                    self.visitExp(else_, depth)
            case AstIntExpression(_):
                pass
            case AstNilExpression():
                pass
            case AstLetExpression(body, declarations):
                for d in declarations:
                    self.visitDec(d, depth)
                self.visitExp(body, depth)
            case AstStringExpression(_):
                pass
            case AstVariableExpression(var):
                self.visitVar(var, depth)
            case AstWhileExpression(body, test):
                self.visitExp(test, depth)
                self.visitExp(body, depth)
            case AstRecordExpression(fields,_):
                for field in fields:
                    self.visitExp(field.expr, depth)
            case AstSequenceExpression(exprs):
                lastExpr = exprs[-1]
                for expr in exprs[:-1]:
                    self.visitExp(expr, depth)
                self.visitExp(lastExpr, depth)
            case AstOperatorExpression(left, oper, right):
                if oper == Operator.Equal or oper == Operator.Neq:
                    self.visitExp(left, depth)
                    self.visitExp(right, depth)
                else:
                    self.visitBinaryOp(left, right, depth)
            case _:
                raise Exception('对表达式进行逃逸分析失败')
    
    def visitVar(self, var: AstVar, depth: int) -> None:
        match var:
            case AstFieldVar(ident) | AstSimpleVar(ident):
                var_ = self.env.look(ident)
                if isinstance(var_, DepthEscape):
                    if depth > var_.depth:
                        var_.escape = True
            case AstSubscriptVar(expr, this):
                self.visitVar(this, depth)
                self.visitExp(expr, depth)
            case _:
                raise Exception('对变量进行逃逸分析失败')

    def visitDec(self, declaration: AstDeclaration, depth: int) -> None:
        match declaration:
            case AstFunctionDeclarationList(declarations):
                for d in declarations:
                    for param in d.params:
                        self.env.enter(param.name, DepthEscape(depth, False))
                    self.visitExp(d.body, depth + 1)
            case AstTypeDeclarationList(_):
                pass
            case AstVariableDeclaration(_, init, name, _):
                self.visitExp(init, depth + 1)
                self.env.enter(name, DepthEscape(depth, False))
            case _:
                raise Exception('对声明进行逃逸分析失败')

def findEscapes(exp: AstExpression) -> SymbolTable[DepthEscape]:
    finder = EscapeFinder()
    finder.visitExp(exp, 0)
    return finder.env