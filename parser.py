from lexer import Token, TokenType
from symbol import Symbol
from syntax import AstArrayExpression, AstArrayType, AstAssignExpression, AstBreakExpression, AstCallExpression, AstDeclaration, AstExpression, AstFieldVar, AstFunctionDeclaration, AstFunctionDeclarationList, AstIfExpression, AstIntExpression, AstLetExpression, AstNameType, AstNilExpression, AstOperatorExpression, AstRecordExpression, AstSequenceExpression, AstSimpleVar, AstStringExpression, AstVar, AstSubscriptVar, AstTypeDeclaration, AstTypeDeclarationList, AstVariableDeclaration, AstVariableExpression, AstWhileExpression, AstRecordType, AstType, AstRecordField, AstField, Operator
from typing import Optional

class Parser:
    def __init__(self, tokens: list[Token]) -> None:
        self.tokens = tokens
        self.current = 0
    
    def isAtEnd(self) -> bool:
        return self.peek().type == TokenType.Eof

    def peek(self) -> Token:
        return self.tokens[self.current]

    def prerious(self) -> Token:
        return self.tokens[self.current-1]

    def check(self, type: TokenType) -> bool:
        if self.isAtEnd():
            return False
        return self.peek().type == type

    def advance(self) -> Token:
        if not self.isAtEnd():
            self.current += 1
        return self.prerious()

    def match(self, type: TokenType) -> bool:
        if self.check(type):
            self.advance()
            return True
        
        return False

    def consume(self, type: TokenType, message: str) -> Token:
        if self.check(type):
            return self.advance()

        raise Exception(str(self.peek()) + message)

    def parse(self) -> AstExpression:
        mainExpression = self.expr()
        if self.isAtEnd():
            return mainExpression
        else:
            raise Exception('文件应该结束了')

    def expr(self) -> AstExpression:
        return self.logicalOrExpr()

    def logicalOrExpr(self) -> AstExpression:
        expr = self.logicalAndExpr()
        while self.match(TokenType.Pipe):
            right = self.logicalAndExpr()
            expr = AstOperatorExpression(expr, Operator.Or, right)
        return expr

    def logicalAndExpr(self) -> AstExpression:
        expr = self.relationalExpr()
        while self.match(TokenType.Ampersand):
            right = self.relationalExpr()
            expr = AstOperatorExpression(expr, Operator.And, right)
        return expr

    def relationalExpr(self) -> AstExpression:
        expr = self.additiveExpr()
        while True:
            oper = None
            match self.peek().type:
                case TokenType.Equal:
                    oper = Operator.Equal
                case TokenType.Greater:
                    oper = Operator.Gt
                case TokenType.GreaterOrEqual:
                    oper = Operator.Ge
                case TokenType.Lesser:
                    oper = Operator.Lt
                case TokenType.LesserOrEqual:
                    oper = Operator.Le
                case TokenType.NotEqual:
                    oper = Operator.Neq
                case _:
                    break
            self.advance()
            right = self.additiveExpr()
            expr = AstOperatorExpression(expr, oper, right)
        return expr

    def additiveExpr(self) -> AstExpression:
        expr = self.multiplicativeExpr()
        while True:
            oper = None
            match self.peek().type:
                case TokenType.Minus:
                    oper = Operator.Minus
                case TokenType.Plus:
                    oper = Operator.Plus
                case _:
                    break
            self.advance()
            right = self.multiplicativeExpr()
            expr = AstOperatorExpression(expr, oper, right)
        return expr

    def multiplicativeExpr(self) -> AstExpression:
        expr = self.unaryExpr()
        while True:
            oper = None
            match self.peek().type:
                case TokenType.Slash:
                    oper = Operator.Divide
                case TokenType.Star:
                    oper = Operator.Times
                case _:
                    break
            self.advance()
            right = self.unaryExpr()
            expr = AstOperatorExpression(expr, oper, right)
        return expr

    def unaryExpr(self) -> AstExpression:
        if self.match(TokenType.Minus):
            expr = self.unaryExpr()
            return AstOperatorExpression(AstIntExpression(1), Operator.Minus, expr)
        else:
            return self.primaryExpr()

    def primaryExpr(self) -> AstExpression:
        match self.peek().type:
            case TokenType.Break: return self.break_()
            case TokenType.For: return self.forLoop()
            case TokenType.If: return self.ifThenElse()
            case TokenType.Ident: return self.callExprOrOther()
            case TokenType.Int: return self.intLit()
            case TokenType.Let: return self.letExpr()
            case TokenType.Nil: return self.nil()
            case TokenType.OpenParen: return self.seqExp()
            case TokenType.Str: return self.stringLit()
            case TokenType.While: return self.whileLoop()
            case _:
                raise Exception("break, for, if, identifier, integer literal, let, nil, (, string literal, while")

    def callExprOrOther(self) -> AstExpression:
        symbol = Symbol(str(self.peek().value))
        self.consume(TokenType.Ident, '缺少标识符')
        if self.match(TokenType.OpenParen):
            args: list[AstExpression] = []
            while True:
                if self.check(TokenType.CloseParen):
                    break
                arg = self.expr()
                args.append(arg)
                if self.match(TokenType.Comma):
                    continue
                else:
                    break
            self.consume(TokenType.CloseParen, '缺少`)`')
            return AstCallExpression(args, symbol)
        elif self.check(TokenType.OpenCurly):
            return self.recCreate(symbol)
        else:
            var = AstSimpleVar(symbol)
            return self.lvalueOrAssign(var)

    def lvalueOrAssign(self, var: AstVar) -> AstExpression:
        var = self.lvalue(var)
        if self.check(TokenType.Of):
            match var:
                case AstSubscriptVar(expr,AstSimpleVar(ident)):
                    self.consume(TokenType.Of, '缺少`of`')
                    init = self.expr()
                    return AstArrayExpression(init, expr, ident)
                case AstSubscriptVar(_,_):
                    raise Exception('neither dot not subscript')
                case _:
                    raise Exception(':= or (nothing)')
        if self.match(TokenType.ColonEqual):
            expr = self.expr()
            return AstAssignExpression(expr, var)
        else:
            return AstVariableExpression(var)

    def whileLoop(self) -> AstExpression:
        self.consume(TokenType.While, '缺少`while`')
        test = self.expr()
        self.consume(TokenType.Do, '缺少`do`')
        body = self.expr()
        return AstWhileExpression(body, test)

    def seqExp(self) -> AstExpression:
        self.consume(TokenType.OpenParen, '缺少`(`')
        exprs = [self.expr()]
        while self.match(TokenType.Semicolon):
            exprs.append(self.expr())
        self.consume(TokenType.CloseParen, '缺少`)`')
        return AstSequenceExpression(exprs)

    def break_(self) -> AstExpression:
        self.consume(TokenType.Break, '缺少`break`')
        return AstBreakExpression()

    def ifThenElse(self) -> AstExpression:
        self.consume(TokenType.If, '缺少`if`')
        test = self.expr()
        self.consume(TokenType.Then, '缺少`then`')
        then = self.expr()
        if self.match(TokenType.Else):
            else_ = self.expr()
        else:
            else_ = None
        return AstIfExpression(else_, test, then)

    def intLit(self) -> AstExpression:
        value = self.peek().value
        self.consume(TokenType.Int, '缺少整数')
        if value != None:
            return AstIntExpression(int(value))
        else:
            raise Exception('None无法强制类型转换成int')

    def dec(self) -> AstDeclaration:
        match self.peek().type:
            case TokenType.Function:
                return self.funDecs()
            case TokenType.Type:
                return self.tyDecs()
            case TokenType.Var:
                return self.varDec()
            case _:
                raise Exception('function, type or var')
                
    def funDecs(self) -> AstDeclaration:
        functions = [self.funDec()]
        while self.check(TokenType.Function):
            functions.append(self.funDec())
        return AstFunctionDeclarationList(functions)

    def lvalue(self, var: AstVar) -> AstVar:
        match self.peek().type:
            case TokenType.OpenSquare:
                return self.subscript(var)
            case TokenType.Dot:
                return self.fieldExp(var)
            case _:
                return var

    def fieldExp(self, var: AstVar) -> AstVar:
        self.consume(TokenType.Dot, '缺少`.`')
        fieldName = str(self.peek().value)
        self.consume(TokenType.Ident, '缺少标识符')
        fieldVar = AstFieldVar(Symbol(fieldName), var)
        return self.lvalue(fieldVar)

    def arrTy(self) -> AstType:
        self.consume(TokenType.Array, '缺少`array`')
        self.consume(TokenType.Of, '缺少`of`')
        symbol = Symbol(str(self.peek().value))
        self.consume(TokenType.Ident, '缺少标识符')
        return AstArrayType(symbol)

    def tyDecs(self) -> AstDeclaration:
        declarations = [self.tyDec()]
        while self.check(TokenType.Type):
            declarations.append(self.tyDec())
        return AstTypeDeclarationList(declarations)

    def tyDec(self) -> AstTypeDeclaration:
        self.consume(TokenType.Type, '缺少`type`')
        symbol = Symbol(str(self.peek().value))
        self.consume(TokenType.Ident, '缺少标识符')
        self.consume(TokenType.Equal, '缺少`=`')
        ty = self.ty()
        return AstTypeDeclaration(symbol, ty)

    def subscript(self, var: AstVar) -> AstVar:
        self.consume(TokenType.OpenSquare, '缺少`[`')
        expr = self.expr()
        self.consume(TokenType.CloseSquare, '缺少`]`')
        return self.lvalue(AstSubscriptVar(expr, var))

    def optionalType(self) -> Optional[Symbol]:
        if self.match(TokenType.Colon):
            symbol = Symbol(str(self.peek().value))
            self.consume(TokenType.Ident, '缺少标识符')
            return symbol
        return None

    def stringLit(self) -> AstExpression:
        value = self.peek().value
        self.consume(TokenType.Str, '缺少字符串')
        return AstStringExpression(str(value))

    def funDec(self) -> AstFunctionDeclaration:
        self.consume(TokenType.Function, '缺少`function`')
        symbol = Symbol(str(self.peek().value))
        self.consume(TokenType.Ident, '缺少函数名称')
        self.consume(TokenType.OpenParen, '缺少`(`')
        params = []
        if not self.check(TokenType.CloseParen):
            params = self.fields()
        self.consume(TokenType.CloseParen, '缺少`)`')
        result = self.optionalType()
        self.consume(TokenType.Equal, '缺少`=`')
        body = self.expr()
        return AstFunctionDeclaration(body, symbol, params, result)

    def recTy(self) -> AstType:
        self.consume(TokenType.OpenCurly, '缺少`{`')
        if self.match(TokenType.CloseCurly):
            fields = []
        else:
            fields = self.fields()
        self.consume(TokenType.CloseCurly, '缺少`}`')
        return AstRecordType(fields)

    def ty(self) -> AstType:
        match self.peek().type:
            case TokenType.Array:
                return self.arrTy()
            case TokenType.OpenCurly:
                return self.recTy()
            case TokenType.Ident:
                typeName = Symbol(str(self.peek().value))
                self.consume(TokenType.Ident, '缺少标识符')
                return AstNameType(typeName)
            case _:
                raise Exception('array, { or identifier')

    def letExpr(self) -> AstExpression:
        self.consume(TokenType.Let, '缺少`let`')
        declarations = [self.dec()]
        while self.check(TokenType.Function) or self.check(TokenType.Type) or self.check(TokenType.Var):
            declarations.append(self.dec())
        self.consume(TokenType.In, 'function, in, type, var')
        exprs = [self.expr()]
        while self.match(TokenType.Semicolon):
            exprs.append(self.expr())
        self.consume(TokenType.End, '缺少`end`')
        return AstLetExpression(AstSequenceExpression(exprs), declarations)

    def fieldDec(self) -> AstField:
        fieldName = Symbol(str(self.peek().value))
        self.consume(TokenType.Ident, '缺少标识符')
        self.consume(TokenType.Colon, '缺少`:`')
        typeName = Symbol(str(self.peek().value))
        self.consume(TokenType.Ident, '缺少标识符')
        return AstField(False, fieldName, typeName)

    def fields(self) -> list[AstField]:
        fields = [self.fieldDec()]
        while self.match(TokenType.Comma):
            fields.append(self.fieldDec())
        return fields

    def recCreate(self, typ: Symbol) -> AstExpression:
        self.consume(TokenType.OpenCurly, '缺少`{`')
        fields = [self.fieldCreate()]
        while self.match(TokenType.Comma):
            fields.append(self.fieldCreate())
        self.consume(TokenType.CloseCurly, '缺少`}`')
        return AstRecordExpression(fields, typ)

    def fieldCreate(self) -> AstRecordField:
        fieldName = Symbol(str(self.peek().value))
        self.consume(TokenType.Ident, '缺少标识符')
        self.consume(TokenType.Equal, '缺少`=`')
        expr = self.expr()
        return AstRecordField(expr, fieldName)

    def nil(self) -> AstExpression:
        self.consume(TokenType.Nil, '缺少`nil`')
        return AstNilExpression()

    def varDec(self) -> AstDeclaration:
        self.consume(TokenType.Var, '缺少`var`')
        varName = Symbol(str(self.peek().value))
        self.consume(TokenType.Ident, '缺少标识符')
        typ = self.optionalType()
        self.consume(TokenType.ColonEqual, '缺少`:=`')
        init = self.expr()
        return AstVariableDeclaration(False, init, varName, typ)

    def forLoop(self) -> AstExpression:
        self.consume(TokenType.For, '缺少`for`')
        var = Symbol(str(self.peek().value))
        self.consume(TokenType.Ident, '缺少标识符')
        iterVariable = AstSimpleVar(var)
        iterVariableExpr: AstExpression = AstVariableExpression(iterVariable)
        self.consume(TokenType.ColonEqual, '缺少`:=`')
        start = self.expr()
        self.consume(TokenType.To, '缺少`to`')
        end = self.expr()
        self.consume(TokenType.Do, '缺少`do`')
        body = self.expr()
        startSymbol = var
        endSymbol = Symbol('__' + str(var) + '_limit')
        declarations: list[AstDeclaration] = [
            AstVariableDeclaration(False, start, startSymbol, None),
            AstVariableDeclaration(False, end, endSymbol, None)
        ]

        body = AstIfExpression(
            None,
            AstOperatorExpression(iterVariableExpr, Operator.Le, AstVariableExpression(AstSimpleVar(endSymbol))),
            AstWhileExpression(
                AstSequenceExpression([
                    body,
                    AstIfExpression(
                        AstBreakExpression(),
                        AstOperatorExpression(
                            iterVariableExpr,
                            Operator.Lt,
                            AstVariableExpression(AstSimpleVar(endSymbol))
                        ),
                        AstAssignExpression(AstOperatorExpression(iterVariableExpr, Operator.Plus, AstIntExpression(1)), iterVariable)
                    )
                ]),
                AstIntExpression(1)
            )
        )
        return AstLetExpression(body, declarations)