from dataclasses import dataclass, field
from typing import Callable, Optional, TypeVar
from enum import Enum

# 词法分析模块

class TokenType(Enum):
    Ampersand      = '&'
    Array          = 'array'
    Break          = 'break'
    CloseCurly     = '}'
    CloseParen     = ')'
    CloseSquare    = ']'
    Colon          = ':'
    ColonEqual     = ':='
    Comma          = ','
    Do             = 'do'
    Dot            = '.'
    Else           = 'else'
    End            = 'end'
    Equal          = '='
    For            = 'for'
    Function       = 'function'
    Greater        = '>'
    GreaterOrEqual = '>='
    Ident          = '标识符'
    If             = 'if'
    In             = 'in'
    Int            = '整型'
    Lesser         = '<'
    LesserOrEqual  = '<='
    Let            = 'let'
    Minus          = '-'
    Nil            = 'nil'
    NotEqual       = '<>'
    Of             = 'of'
    OpenCurly      = '{'
    OpenParen      = '('
    OpenSquare     = '['
    Pipe           = '|'
    Plus           = '+'
    Semicolon      = ';'
    Slash          = '/'
    Star           = '*'
    Str            = '字符串'
    Then           = 'then'
    To             = 'to'
    Type           = 'type'
    Var            = 'var'
    While          = 'while'
    Eof            = '文件结束符'

@dataclass
class Token:
    type: TokenType
    value: int|str|None
    line: int

    def __str__(self) -> str:
        return 'Token({type}, {value}, 行号={line})\n'.format(
            type=self.type,
            value=repr(self.value),
            line=self.line,
        )

    def __repr__(self) -> str:
        return self.__str__()

keywords: dict[str, TokenType] = {
    'nil'     : TokenType.Nil,
    'if'      : TokenType.If,
    'then'    : TokenType.Then,
    'else'    : TokenType.Else,
    'function': TokenType.Function,
    'do'      : TokenType.Do,
    'let'     : TokenType.Let,
    'in'      : TokenType.In,
    'to'      : TokenType.To,
    'for'     : TokenType.For,
    'while'   : TokenType.While,
    'of'      : TokenType.Of,
    'var'     : TokenType.Var,
    'type'    : TokenType.Type,
    'array'   : TokenType.Array,
    'break'   : TokenType.Break,
    'end'     : TokenType.End,
}

class Lexer:
    def __init__(self, source: str) -> None:
        self.source: str = source
        self.tokens: list[Token] = []
        self.start: int = 0
        self.current: int = 0
        self.line: int = 0

    def advance(self) -> str:
        self.current += 1
        return self.source[self.current - 1]

    def isAtEnd(self) -> bool:
        return self.current >= len(self.source)

    def isDigit(self, c: str) -> bool:
        return c >= '0' and c <= '9'

    def isAlpha(self, c: str) -> bool:
        return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_'

    def isAlphaNumeric(self, c: str) -> bool:
        return self.isAlpha(c) or self.isDigit(c)

    def peek(self) -> str:
        if self.isAtEnd():
            return '\0'
        return self.source[self.current]

    def match(self, expected: str) -> bool:
        if self.isAtEnd():
            return False
        if self.source[self.current] != expected:
            return False

        self.current += 1
        return True

    def identifier(self) -> None:
        while self.isAlphaNumeric(self.peek()):
            self.advance()

        text = self.source[self.start:self.current]
        try:
            type = keywords[text]
        except:
            type = TokenType.Ident
        self.tokens.append(Token(type, text, self.line))

    def number(self) -> None:
        while self.isDigit(self.peek()):
            self.advance()
        
        self.tokens.append(Token(
            TokenType.Int,
            int(self.source[self.start:self.current]),
            self.line))

    def string(self) -> None:
        while self.peek() != '"' and not self.isAtEnd():
            self.advance()

        if self.isAtEnd():
            raise Exception('第' + str(self.line) + '行的字符串没有闭合')

        self.advance()

        value = self.source[self.start+1:self.current-1]
        self.tokens.append(Token(TokenType.Str, value, self.line))

    def addToken(self, type: TokenType) -> None:
        self.tokens.append(Token(type, None, self.line))

    def scanToken(self) -> None:
        c = self.advance()

        match c:
            case '(': self.addToken(TokenType.OpenParen)
            case ')': self.addToken(TokenType.CloseParen)
            case '{': self.addToken(TokenType.OpenCurly)
            case '}': self.addToken(TokenType.CloseCurly)
            case '.': self.addToken(TokenType.Dot)
            case ',': self.addToken(TokenType.Comma)
            case '+': self.addToken(TokenType.Plus)
            case '-': self.addToken(TokenType.Minus)
            case '*': self.addToken(TokenType.Star)
            case ';': self.addToken(TokenType.Semicolon)
            case '&': self.addToken(TokenType.Ampersand)
            case '|': self.addToken(TokenType.Pipe)
            case '[': self.addToken(TokenType.OpenSquare)
            case ']': self.addToken(TokenType.CloseSquare)
            case '=': self.addToken(TokenType.Equal)
            case ':':
                if self.match('='):
                    self.addToken(TokenType.ColonEqual)
                else:
                    self.addToken(TokenType.Colon)
            case '<':
                if self.match('='):
                    self.addToken(TokenType.LesserOrEqual)
                elif self.match('>'):
                    self.addToken(TokenType.NotEqual)
                else:
                    self.addToken(TokenType.Lesser)
            case '>':
                if self.match('='):
                    self.addToken(TokenType.GreaterOrEqual)
                else:
                    self.addToken(TokenType.Greater)
            case '/':
                if self.match('*'):
                    depth = 1
                    while True:
                        self.advance()
                        ch = self.peek()
                        if ch == '/':
                            self.advance()
                            ch = self.peek()
                            if ch == '*':
                                depth += 1
                        if ch == '*':
                            self.advance()
                            ch = self.peek()
                            if ch == '/':
                                depth -= 1
                                if depth == 0:
                                    self.advance()
                                    break
                else:
                    self.addToken(TokenType.Slash)
            case ' ': ...
            case '\t': ...
            case '\r': ...
            case '\n': self.line += 1
            case '"': self.string()
            case _:
                if self.isDigit(c):
                    self.number()
                elif self.isAlpha(c):
                    self.identifier()
                else:
                    raise Exception('未识别字符')

    def scanTokens(self) -> list[Token]:
        while not self.isAtEnd():
            self.start = self.current
            self.scanToken()

        self.tokens.append(Token(TokenType.Eof, None, self.line))
        return self.tokens

# 符号模块
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

# 抽象语法树模块
class Operator(Enum):
    And    = '&'
    Divide = '/'
    Equal  = '='
    Ge     = '>='
    Gt     = '>'
    Le     = '<='
    Lt     = '<'
    Minus  = '-'
    Neq    = '<>'
    Or     = '|'
    Plus   = '+'
    Times  = '*'

@dataclass
class AstNode:
    pass

@dataclass
class AstDeclaration(AstNode):
    pass

@dataclass
class AstExpression(AstNode):
    pass

@dataclass
class AstType(AstNode):
    pass

@dataclass
class AstVar(AstNode):
    pass


@dataclass
class AstSimpleVar(AstVar):
    ident: Symbol

@dataclass
class AstFieldVar(AstVar):
    ident: Symbol
    this: AstVar

@dataclass
class AstSubscriptVar(AstVar):
    expr: AstExpression
    this: AstVar

@dataclass
class AstField(AstNode):
    escape: bool
    name: Symbol
    type: Symbol

@dataclass
class AstArrayType(AstType):
    ident: Symbol

@dataclass
class AstNameType(AstType):
    ident: Symbol

@dataclass
class AstRecordType(AstType):
    fields: list[AstField]

@dataclass
class AstTypeDeclaration(AstNode):
    name: Symbol
    type: AstType

@dataclass
class AstFunctionDeclaration(AstNode):
    body: AstExpression
    name: Symbol
    params: list[AstField]
    result: Optional[Symbol]

@dataclass
class AstFunctionDeclarationList(AstDeclaration):
    fundecs: list[AstFunctionDeclaration]

@dataclass
class AstTypeDeclarationList(AstDeclaration):
    typedecs: list[AstTypeDeclaration]

@dataclass
class AstRecordField(AstNode):
    expr: AstExpression
    ident: Symbol

@dataclass
class AstVariableDeclaration(AstDeclaration):
    escape: bool
    init: AstExpression
    name: Symbol
    typ: Optional[Symbol]

@dataclass
class AstArrayExpression(AstExpression):
    init: AstExpression
    size: AstExpression
    type: Symbol

@dataclass
class AstAssignExpression(AstExpression):
    expr: AstExpression
    var: AstVar

@dataclass
class AstBreakExpression(AstExpression):
    pass

@dataclass
class AstCallExpression(AstExpression):
    args: list[AstExpression]
    function: Symbol

@dataclass
class AstIfExpression(AstExpression):
    else_: Optional[AstExpression]
    test: AstExpression
    then: AstExpression

@dataclass
class AstIntExpression(AstExpression):
    value: int

@dataclass
class AstNilExpression(AstExpression):
    pass

@dataclass
class AstLetExpression(AstExpression):
    body: AstExpression
    declarations: list[AstDeclaration]

@dataclass
class AstOperatorExpression(AstExpression):
    left: AstExpression
    oper: Operator
    right: AstExpression

@dataclass
class AstRecordExpression(AstExpression):
    fields: list[AstRecordField]
    type: Symbol

@dataclass
class AstSequenceExpression(AstExpression):
    sequence: list[AstExpression]

@dataclass
class AstStringExpression(AstExpression):
    value: str

@dataclass
class AstVariableExpression(AstExpression):
    var: AstVar

@dataclass
class AstWhileExpression(AstExpression):
    body: AstExpression
    test: AstExpression

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


# 类型模块

count = 1

@dataclass
class Type:
    pass

@dataclass
class IntType(Type):
    def __str__(self) -> str:
        return 'int'

    def __repr__(self) -> str:
        return self.__str__()

@dataclass
class StringType(Type):
    def __str__(self) -> str:
        return 'string'

    def __repr__(self) -> str:
        return self.__str__()

@dataclass
class RecordType(Type):
    symbol: Symbol
    fields: list[tuple[Symbol, Type]]
    unique: int = field(init=False)

    def __str__(self) -> str:
        return 'struct {}'.format(str(self.symbol))

    def __repr__(self) -> str:
        return self.__str__()
    # 通过判断Unique是否相等来判断是否是相同的Record类型
    # 加快判断速度

    def __eq__(self, other: object) -> bool:
        if isinstance(other, RecordType):
            return self.unique == other.unique
        return False

    def __post_init__(self) -> None:
        global count
        count += 1
        self.unique = count

@dataclass
class ArrayType(Type):
    type: Type
    unique: int = field(init=False)

    def __str__(self) -> str:
        return '[{}]'.format(str(self.type))

    def __repr__(self) -> str:
        return self.__str__()

    def __eq__(self, other: object) -> bool:
        if isinstance(other, ArrayType):
            return self.unique == other.unique
        return False

    def __post_init__(self) -> None:
        global count
        count += 1
        self.unique = count

@dataclass
class NilType(Type):
    def __str__(self) -> str:
        return 'nil'

    def __repr__(self) -> str:
        return self.__str__()

@dataclass
class UnitType(Type):
    def __str__(self) -> str:
        return '()'

    def __repr__(self) -> str:
        return self.__str__()

@dataclass
class NameType(Type):
    symbol: Symbol
    type: Optional[Type]

    def __str__(self) -> str:
        if self.type != None:
            return str(type)
        else:
            return 'unresolved type'
    
    def __repr__(self) -> str:
        return self.__str__()

@dataclass
class ErrorType(Type):
    def __str__(self) -> str:
        return 'type error'

    def __repr__(self) -> str:
        return self.__str__()

# 符号表(环境)模块

class Label:
    pass


@dataclass
class Entry:
    pass

@dataclass
class Access:
    pass

Level = TypeVar('Level', bound='Level')
LevelAccess = tuple[Level, Access]

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

@dataclass
class DepthEscape:
    depth: int
    escape: bool

T = Type|Entry|DepthEscape

class SymbolTable:
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
        
    

class Environment:
    def __init__(self, escapeEnv: SymbolTable) -> None:
        self.typeEnv = SymbolTable([], {})
        self.varEnv = SymbolTable([], {})
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

# 帧结构模块

@dataclass
class IrExp:
    pass

@dataclass
class IrStatement:
    pass

tempCount = 0

tempMap = {}

@dataclass
class Temp:
    num: int = field(init=False)

    def __hash__(self) -> int:
        return self.num

    def __str__(self) -> str:
        if self in tempMap:
            return tempMap[self]
        return 't{}'.format(self.num)

    def __repr__(self) -> str:
        return self.__str__()

    def __post_init__(self) -> None:
        global tempCount
        tempCount += 1
        self.num = tempCount

RBP = Temp()
RSP = Temp()
RAX = Temp()
RBX = Temp()
RCX = Temp()
RDX = Temp()
RDI = Temp()
RSI = Temp()
R8  = Temp()
R9  = Temp()
R10 = Temp()
R11 = Temp()
R12 = Temp()
R13 = Temp()
R14 = Temp()
R15 = Temp()

tempMap = {
    RBP: 'rbp',
    RSP: 'rsp',
    RAX: 'rax',
    RBX: 'rbx',
    RCX: 'rcx',
    RDX: 'rdx',
    RDI: 'rdi',
    RSI: 'rsi',
    R8:  'r8',
    R9:  'r9',
    R10: 'r10',
    R11: 'r11',
    R12: 'r12',
    R13: 'r13',
    R14: 'r14',
    R15: 'r15',
}

def argRegisters() -> list[Temp]:
    return [RDI, RSI, RDX, RCX, R8, R9]

def calleeSavedRegisters() -> list[Temp]:
    return [RBX, RBP, R12, R13, R14, R15]

def specialRegisters() -> list[Temp]:
    return [RAX, RSP]

def callerSavedRegisters() -> list[Temp]:
    return [R10, R11]

def returnValue() -> Temp:
    return RAX

def registers() -> list[Temp]:
    registers = argRegisters()
    registers.append(returnValue())
    registers = registers + calleeSavedRegisters() + specialRegisters() + callerSavedRegisters()
    return registers

def registerCount() -> int:
    return len(registers()) - len([RSP, RBP])

def fp() -> Temp:
    return RBP

class BinOp(Enum):
    Plus = '+'
    Minus = '-'
    Mul = '*'
    Div = '/'
    And = '&'
    Or = '|'
    ShiftLeft = '逻辑左移'
    ShiftRight = '逻辑右移'
    ArithmeticShiftRight = '算术右移'
    Xor = '异或'

class RelationalOp(Enum):
    Equal = '='
    NotEqual = '<>'
    LesserThan = '<'
    GreaterThan = '>'
    LesserOrEqual = '<='
    GreaterOrEqual = '>='
    UnsignedLesserThan = '无符号小于'
    UnsignedLesserOrEqual = '无符号小于等于'
    UnsignedGreaterThan = '无符号大于'
    UnsignedGreaterOrEqual = '无符号大于等于'



@dataclass
class IrConst(IrExp):
    value: int

@dataclass
class IrError(IrExp):
    pass

@dataclass
class IrName(IrExp):
    label: Label

@dataclass
class IrTemp(IrExp):
    temp: Temp

@dataclass
class IrBinOp(IrExp):
    op: BinOp
    left: IrExp
    right: IrExp

@dataclass
class IrMem(IrExp):
    exp: IrExp

@dataclass
class IrCall(IrExp):
    func: IrExp
    args: list[IrExp]

@dataclass
class IrExpSequence(IrExp):
    stmt: IrStatement
    exp: IrExp

@dataclass
class IrMove(IrStatement):
    dst: IrExp
    src: IrExp

@dataclass
class IrExpStatement(IrStatement):
    exp: IrExp

@dataclass
class IrJump(IrStatement):
    exp: IrExp
    labels: list[Label]

@dataclass
class IrCondJump(IrStatement):
    op: RelationalOp
    left: IrExp
    right: IrExp
    true_label: Label
    false_label: Label

@dataclass
class IrSequence(IrStatement):
    stmt1: IrStatement
    stmt2: IrStatement

@dataclass
class IrLabel(IrStatement):
    label: Label

    def __eq__(self, other: object) -> bool:
        if isinstance(other, NumLabel) or isinstance(other, NamedLabel):
            return self == other
        return False


class NamedLabel(Label):
    def __init__(self, name : str) -> None:
        self.name = name

    def __str__(self) -> str:
        return self.name

    def __repr__(self) -> str:
        return self.__str__()

    def __eq__(self, __o: object) -> bool:
        if isinstance(__o, NamedLabel):
            return self.name == __o.name
        return False

labelCount = 0

class NumLabel(Label):
    def __init__(self) -> None:
        global labelCount
        labelCount += 1
        self.num = labelCount

    def __str__(self) -> str:
        return 'l{}'.format(self.num)

    def __repr__(self) -> str:
        return self.__str__()

    def __hash__(self) -> int:
        return self.num

    def __eq__(self, other: object) -> bool:
        if isinstance(other, NumLabel):
            return self.num == other.num
        return False

POINTER_SIZE = 8

WORD_SIZE = 8



@dataclass
class InFrame(Access):
    offset: int

@dataclass
class InReg(Access):
    temp: Temp

class Frame:
    def __init__(self, name: Label, formals: list[bool]) -> None:
        self.name = name
        self.pointer = 0
        self.formals = [self.allocLocal(i) for i in formals]

    def allocLocal(self, escape: bool) -> Access:
        if escape:
            self.pointer -= POINTER_SIZE
            return InFrame(self.pointer)
        else:
            return InReg(Temp())

    def frameFormals(self) -> list[Access]:
        return self.formals

    def frameName(self) -> Label:
        return self.name

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Frame):
            return self.name == other.name
        return False

    def exp(self, access: Access, stackFrame: IrExp) -> IrExp:
        match access:
            case InFrame(offset):
                return IrMem(IrBinOp(BinOp.Plus, stackFrame, IrConst(offset)))
            case InReg(reg):
                return IrTemp(reg)
            case _:
                raise Exception('非法access访问类型')

def externalCall(name: str, arguments: list[IrExp]) -> IrExp:
    return IrCall(IrName(NamedLabel(name)), arguments)



@dataclass
class Level:
    current: Frame
    parent: Optional[Level]

    def formals(self) -> list[LevelAccess]:
        return [(self, access) for access in self.current.frameFormals()]

def outermost() -> Level:
    return Level(Frame(NumLabel(), []), None)

def newLevel(parent: Level, name: Label, formals: list[bool]) -> Level:
    return Level(Frame(name, formals + [True]), parent)

def allocLocal(level: Level, escape: bool) -> LevelAccess:
    frameLocal = level.current.allocLocal(escape)
    return (level, frameLocal)

# 逃逸分析模块

class EscapeFinder:
    def __init__(self) -> None:
        self.env = SymbolTable([], {})

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

def findEscapes(exp: AstExpression) -> SymbolTable:
    finder = EscapeFinder()
    finder.visitExp(exp, 0)
    return finder.env

# 中间表示模块

# 生成中间代码的一些功能函数

def toIrOp(op: Operator) -> BinOp:
    match op:
        case Operator.Plus: return BinOp.Plus
        case Operator.Minus: return BinOp.Minus
        case Operator.Times: return BinOp.Mul
        case Operator.And: return BinOp.And
        case Operator.Divide: return BinOp.Div
        case Operator.Or: return BinOp.Or
        case _:
            raise Exception(str(op) + '不是二元运算符')

def toIrRelOp(op: Operator) -> RelationalOp:
    match op:
        case Operator.Equal: return RelationalOp.Equal
        case Operator.Ge: return RelationalOp.GreaterOrEqual
        case Operator.Gt: return RelationalOp.GreaterThan
        case Operator.Le: return RelationalOp.LesserOrEqual
        case Operator.Lt: return RelationalOp.LesserThan
        case Operator.Neq: return RelationalOp.NotEqual
        case _:
            raise Exception(str(op) + '不是二元关系运算符')

def arraySubscript(var: IrExp, subsciprt: IrExp) -> IrExp:
    return IrMem(IrBinOp(
        BinOp.Plus,
        var,
        IrBinOp(
            BinOp.Mul,
            subsciprt,
            IrConst(WORD_SIZE)
        )
    ))

def binaryOper(op: Operator, left: IrExp, right: IrExp) -> IrExp:
    return IrBinOp(toIrOp(op), left, right)

def fieldAccess(var: IrExp, fieldIndex: int) -> IrExp:
    return IrMem(IrBinOp(
        BinOp.Plus,
        var,
        IrConst(WORD_SIZE * fieldIndex)
    ))

def unit() -> IrExp:
    return IrConst(0)

def goto(label: Label) -> IrExp:
    return IrExpSequence(IrJump(IrName(label), [label]), unit())

def functionCall(label: Label, args: list[IrExp], parentLevel: Level, currentLevel: Level) -> IrExp:
    if currentLevel == parentLevel:
        frame = currentLevel.current
        args.append(frame.exp(frame.formals[-1], IrTemp(fp())))
    elif currentLevel.parent == parentLevel:
        args.append(IrTemp(fp()))
    else:
        functionLevel = parentLevel
        var = IrTemp(fp())
        while True:
            if currentLevel.parent != None:
                if currentLevel.parent == functionLevel:
                    break
            frame = functionLevel.current
            var = frame.exp(frame.frameFormals()[-1], var)
            if functionLevel.parent != None:
                functionLevel = functionLevel.parent
            else:
                break
    return IrCall(IrName(label), args)

def num(number: int) -> IrExp:
    return IrConst(number)

def ifExpression(testExpr: IrExp, ifExpr: IrExp, elseExpr: Optional[IrExp], level: Level) -> IrExp:
    result = allocLocal(level, False)
    trueLabel = NumLabel()
    falseLabel = NumLabel()
    endLabel = NumLabel()
    frame = level.current
    result = frame.exp(result[1], IrTemp(fp()))
    return IrExpSequence(
        IrSequence(
            IrCondJump(RelationalOp.Equal, testExpr, IrConst(1), trueLabel, falseLabel),
            IrSequence(
                IrLabel(trueLabel),
                IrSequence(
                    IrMove(result, ifExpr),
                    IrSequence(
                        IrJump(IrName(endLabel), [endLabel]),
                        IrSequence(
                            IrLabel(falseLabel),
                            IrSequence(
                                IrMove(result, elseExpr if elseExpr != None else unit()),
                                IrLabel(endLabel)
                            )
                        )
                    )
                )
            )
        ),
        result
    )

def stringEquality(oper: Operator, left: IrExp, right: IrExp) -> IrExp:
    exp = externalCall('stringEqual', [left, right])
    match oper:
        case Operator.Equal:
            return exp
        case Operator.Neq:
            return IrBinOp(BinOp.Minus, IrConst(1), exp)
        case _:
            raise Exception('未预期的运算符' + str(oper))

def varDec(access: LevelAccess, value: IrExp) -> IrStatement:
    varLevel = access[0]
    frame = varLevel.current
    return IrMove(frame.exp(access[1], IrTemp(fp())), value)

def varDecs(variables: list[IrStatement], body: IrExp) -> IrExp:
    if len(variables) == 0:
        return body

    if len(variables) == 1:
        return IrExpSequence(variables[0], body)

    statements = IrSequence(variables[0], variables[1])
    for var in variables[2:]:
        statements = IrSequence(statements, var)
    return IrExpSequence(statements, body)

def whileLoop(doneLabel: Label, testExpr: IrExp, body: IrExp) -> IrExp:
    testLabel = NumLabel()
    afterCheckLabel = NumLabel()
    return IrExpSequence(
        IrSequence(
            IrSequence(
                IrLabel(testLabel),
                IrSequence(
                    IrSequence(
                        IrCondJump(RelationalOp.NotEqual, testExpr, IrConst(1), doneLabel, afterCheckLabel),
                        IrLabel(afterCheckLabel)
                    ),
                    IrSequence(
                        IrExpStatement(body),
                        IrJump(IrName(testLabel), [testLabel])
                    )
                )
            ),
            IrLabel(doneLabel)
        ),
        unit()
    )

def simpleVar(access: LevelAccess, level: Level) -> IrExp:
    functionLevel = level
    varLevel = access[0]
    frame = level.current
    var = IrTemp(fp())
    while functionLevel.current != varLevel.current:
        var = frame.exp(functionLevel.current.frameFormals()[-1], var)
        if functionLevel.parent != None:
            functionLevel = functionLevel.parent
        else:
            raise Exception('function level should have a parent')
    var = frame.exp(access[1], var)
    return var

def recordCreate(fields: list[IrExp]) -> IrExp:
    if len(fields) == 0:
        return unit()

    result = IrTemp(Temp())
    sequence = IrSequence(
        IrMove(result, externalCall('malloc', [IrConst(len(fields) * WORD_SIZE)])),
        IrMove(result, fields[0])
    )

    for i in range(1, len(fields)):
        sequence = IrSequence(
            sequence,
            IrMove(
                IrMem(IrBinOp(BinOp.Plus, result, IrConst((i + 1) * WORD_SIZE))),
                fields[i]
            )
        )

    return IrExpSequence(sequence, result)

def relationalOper(op: Operator, left: IrExp, right: IrExp, level: Level) -> IrExp:
    result = allocLocal(level, False)
    trueLabel = NumLabel()
    falseLabel = NumLabel()
    endLabel = NumLabel()
    frame = level.current
    result = frame.exp(result[1], IrTemp(fp()))
    return IrExpSequence(
        IrSequence(
            IrCondJump(toIrRelOp(op), left, right, trueLabel,falseLabel),
            IrSequence(
                IrLabel(trueLabel),
                IrSequence(
                    IrMove(result, IrConst(1)),
                    IrSequence(
                        IrJump(IrName(endLabel), [endLabel]),
                        IrSequence(
                            IrLabel(falseLabel),
                            IrSequence(
                                IrMove(result, IrConst(0)),
                                IrLabel(endLabel)
                            )
                        )
                    )
                )
            )
        ),
        result
    )

@dataclass
class Fragment:
    pass

@dataclass
class FunctionFragment(Fragment):
    body: IrStatement
    frame: Frame

@dataclass
class StrFragment(Fragment):
    label: Label
    value: str

class Gen:
    def __init__(self) -> None:
        self.fragments: list[Fragment] = []

    def getResult(self) -> list[Fragment]:
        return self.fragments

    def procEntryExit(self, level: Level, body: IrExp) -> None:
        body_ = IrMove(IrTemp(returnValue()), body)
        self.fragments.append(FunctionFragment(body_, level.current))

    def stringLiteral(self, string: str) -> IrExp:
        label = NumLabel()
        self.fragments.append(StrFragment(label, string))
        return IrName(label)

# 语义分析模块

@dataclass
class AddError:
    pass

@dataclass
class DontAddError:
    pass

@dataclass
class ExpTy:
    exp: IrExp
    ty: Type

ExpTypeError = ExpTy(IrError(), ErrorType())

class SemanticAnalyzer:
    def __init__(self, env: Environment) -> None:
        self.env = env
        self.errors: list[str] = []
        self.inLoop = False
        self.gen = Gen()

    def analyze(self, mainSymbol: Symbol, expr: AstExpression) -> list[Fragment]|list[str]:
        body = AstSequenceExpression([expr, AstIntExpression(0)])
        result = Symbol('int')
        self.transDec(AstFunctionDeclarationList([AstFunctionDeclaration(body, mainSymbol, [], result)]), outermost(), None)
        if len(self.errors) == 0:
            return self.gen.getResult()
        else:
            return self.errors

    def checkBinaryOp(self, oper: Operator, left: AstExpression, right: AstExpression, level: Level, doneLabel: Optional[Label]) -> ExpTy:
        leftExpTy = self.transExp(left, level, doneLabel)
        self.checkInt(leftExpTy)
        rightExpTy = self.transExp(right, level, doneLabel)
        self.checkInt(rightExpTy)
        return ExpTy(binaryOper(oper, leftExpTy.exp, rightExpTy.exp), IntType())

    def checkDuplicateTypes(self, types: list[AstTypeDeclaration]) -> None:
        names: set[str] = set()
        for ty in types:
            names.add(str(ty.name))
            if isinstance(ty.type, AstNameType):
                if str(ty.type.ident) in names:
                    self.errors.append('类型出现循环定义')


    def checkInt(self, expr: ExpTy) -> None:
        if not isinstance(expr.ty, IntType) and not isinstance(expr.ty, ErrorType):
            self.errors.append('期望整型，实际类型却是' + str(expr.ty))

    def getType(self, symbol: Symbol, add: AddError|DontAddError) -> Type:
        typ = self.env.lookType(symbol)
        if typ != None:
            return typ
        if isinstance(add, AddError):
            self.errors.append('未定义类型' + str(symbol))
        return ErrorType()

    def getVar(self, symbol: Symbol) -> Entry:
        entry = self.env.lookVar(symbol)
        if entry != None:
            return entry
        self.errors.append('未定义标识符' + str(symbol))
        return ErrorEntry()


    def actualTy(self, typ: Type) -> Type:
        match typ:
            case NameType(symbol, ty):
                if ty == None:
                    return self.getType(symbol, DontAddError())
                else:
                    return typ
            case _:
                return typ

    def actualTyVar(self, typ: Type) -> Type:
        match typ:
            case NameType(symbol, ty):
                if ty == None:
                    if isinstance(self.getVar(symbol), VarEntry):
                        return typ
                    else:
                        raise Exception('type should be a variable, not a function')
                else:
                    return ty
            case _:
                return typ

    def checkTypes(self, expected: Type, unexpected: Type) -> None:
        expected = self.actualTy(expected)
        unexpected = self.actualTy(unexpected)
        if expected != unexpected and not isinstance(expected, ErrorType) and not isinstance(unexpected, ErrorType):
            if isinstance(expected, RecordType):
                if isinstance(unexpected, NilType):
                    return None
            self.errors.append('期望的类型是' + str(expected) + "，实际类型却是" + str(unexpected)) 

    def transExp(self, expr: AstExpression, level: Level, doneLabel: Optional[Label]) -> ExpTy:
        match expr:
            case AstArrayExpression(init, size, typ):
                sizeExpr = self.transExp(size, level, doneLabel)
                self.checkInt(sizeExpr)
                ty = self.getType(typ, AddError())
                initExpr = self.transExp(init, level, doneLabel)
                match ty:
                    case ArrayType(typ):
                        self.checkTypes(typ, initExpr.ty)
                    case ErrorType():
                        pass
                    case _:
                        self.errors.append('期望array类型')
                return ExpTy(externalCall('initArray', [sizeExpr.exp, initExpr.exp]), ty)
            case AstStringExpression(value):
                return ExpTy(self.gen.stringLiteral(value), StringType())
            case AstIntExpression(value):
                return ExpTy(num(value), IntType())
            case AstNilExpression():
                return ExpTy(num(0), NilType())
            case AstBreakExpression():
                if not self.inLoop:
                    self.errors.append('break不能在循环外使用')
                    return ExpTypeError
                if doneLabel != None:
                    return ExpTy(goto(doneLabel), UnitType())
                else:
                    raise Exception('break should be in while loop')
            case AstAssignExpression(expr, var):
                var_ = self.transVar(var, level, doneLabel)
                exprTy = self.transExp(expr, level, doneLabel)
                self.checkTypes(var_.ty, exprTy.ty)
                return ExpTy(IrExpSequence(IrMove(var_.exp, exprTy.exp), unit()), UnitType())
            case AstOperatorExpression(left, oper, right):
                if oper == Operator.Plus or \
                   oper == Operator.Minus or \
                   oper == Operator.Times or \
                   oper == Operator.Divide or \
                   oper == Operator.And or \
                   oper == Operator.Or:
                    return self.checkBinaryOp(oper, left, right, level, doneLabel)
                elif oper == Operator.Equal or \
                     oper == Operator.Neq or \
                     oper == Operator.Lt or \
                     oper == Operator.Le or \
                     oper == Operator.Gt or \
                     oper == Operator.Ge:
                    left = self.transExp(left, level, doneLabel)
                    right = self.transExp(right, level, doneLabel)
                    self.checkTypes(left.ty, right.ty)
                    if isinstance(left.ty, StringType) and isinstance(right.ty, StringType):
                        exp = stringEquality(oper, left.exp, right.exp)
                    else:
                        exp = relationalOper(oper, left.exp, right.exp, level)
                    return ExpTy(exp, IntType())
                else:
                    raise Exception('对二元表达式进行语义分析失败')
            case AstIfExpression(else_,test,then):
                testExpr = self.transExp(test, level, doneLabel)
                self.checkInt(testExpr)
                ifExpr = self.transExp(then, level, doneLabel)
                if else_ == None:
                    self.checkTypes(UnitType(), ifExpr.ty)
                    elseExpr = None
                else:
                    elseExpr = self.transExp(else_, level, doneLabel)
                    self.checkTypes(ifExpr.ty, elseExpr.ty)
                return ExpTy(ifExpression(testExpr.exp, ifExpr.exp, elseExpr.exp if elseExpr != None else None, level), ifExpr.ty)
            case AstLetExpression(body, declarations):
                oldInLoop = self.inLoop
                self.inLoop = False
                self.env.beginScope()
                vars: list[IrStatement] = []
                for d in declarations:
                    statement = self.transDec(d, level, doneLabel)
                    if statement != None:
                        vars.append(statement)
                self.inLoop = oldInLoop
                result = self.transExp(body, level, doneLabel)
                self.env.endScope()
                return ExpTy(varDecs(vars, result.exp), result.ty)
            case AstCallExpression(args, function):
                entry = self.env.lookVar(function)
                if isinstance(entry, FunEntry):
                    exprArgs: list[IrExp] = []
                    for (arg, param) in zip(args, entry.parameters):
                        exp = self.transExp(arg, level, doneLabel)
                        self.checkTypes(param, exp.ty)
                        exprArgs.append(exp.exp)
                    if entry.external == True:
                        exp = externalCall(str(entry.label), exprArgs)
                    else:
                        exp = functionCall(entry.label, exprArgs, level, entry.level)
                    return ExpTy(exp, self.actualTyVar(entry.result))
                self.errors.append('未定义函数' + str(function))
                return ExpTypeError
            case AstSequenceExpression(exprs):
                try:
                    lastExpr = exprs[-1]
                    newExprs: list[ExpTy] = []
                    for expr in exprs[:-1]:
                        newExprs.append(self.transExp(expr, level, doneLabel))
                    lastExprExpTy = self.transExp(lastExpr, level, doneLabel)
                    if len(newExprs) == 0:
                        return lastExprExpTy
                    else:
                        exprs_ = IrExpStatement(newExprs.pop().exp)
                        newExprs.reverse()
                        for e in newExprs:
                            exprs_ = IrSequence(IrExpStatement(e.exp), exprs_)
                        return ExpTy(IrExpSequence(exprs_, lastExprExpTy.exp), lastExprExpTy.ty)
                except:
                    raise Exception('序列表达式不能为空')
            case AstVariableExpression(var):
                return self.transVar(var, level, doneLabel)
            case AstWhileExpression(body, test):
                testExpr = self.transExp(test, level, doneLabel)
                self.checkInt(testExpr)
                oldInLoop = self.inLoop
                self.inLoop = True
                whileDoneLabel = NumLabel()
                result = self.transExp(body, level, whileDoneLabel)
                self.inLoop = oldInLoop
                return ExpTy(whileLoop(whileDoneLabel, testExpr.exp, result.exp), result.ty)
            case AstRecordExpression(fields, typ):
                ty = self.getType(typ, AddError())
                fieldExprs: list[IrExp] = []
                if isinstance(ty, RecordType):
                    for (typeFieldName, typeField) in ty.fields:
                        found = False
                        for field in fields:
                            if typeFieldName == field.ident:
                                found = True
                                fieldExpr = self.transExp(field.expr, level, doneLabel)
                                self.checkTypes(typeField, fieldExpr.ty)
                                fieldExprs.append(fieldExpr.exp)
                        if not found:
                            self.errors.append('缺少类型' + str(typeFieldName))
                            return ExpTypeError
                    for field in fields:
                        found = any([i[0] == field.ident for i in ty.fields])
                        if not found:
                            self.errors.append(str(ty.symbol) + '记录类型出现多余的字段')
                            return ExpTypeError
                elif isinstance(ty, ErrorType):
                    pass
                else:
                    self.errors.append('未预期的记录类型')
                    return ExpTypeError
                return ExpTy(recordCreate(fieldExprs), ty)
            case _:
                raise Exception('对表达式语义分析失败')

    def transVar(self, var: AstVar, level: Level, doneLabel: Optional[Label]) -> ExpTy:
        match var:
            case AstFieldVar(ident, this):
                varExpTy = self.transVar(this, level, doneLabel)
                match varExpTy.ty:
                    case RecordType(_, fields):
                        index = 0
                        for (name, typ) in fields:
                            if name == ident:
                                return ExpTy(fieldAccess(varExpTy.exp, index), typ)
                            index += 1
                        self.errors.append('未预期的记录字段' + str(ident))
                        return ExpTypeError
                    case ty:
                        self.errors.append(str(ty) + '不是记录类型')
                        return ExpTypeError
            case AstSimpleVar(ident):
                entry = self.env.lookVar(ident)
                if isinstance(entry, VarEntry):
                    return ExpTy(simpleVar(entry.access, level), self.actualTyVar(entry.typ))
                else:
                    self.errors.append('未定义变量' + str(ident))
                    return ExpTypeError
            case AstSubscriptVar(expr, this):
                varExpTy = self.transVar(this, level, doneLabel)
                subscriptExpr = self.transExp(expr, level, doneLabel)
                self.checkInt(subscriptExpr)
                match varExpTy.ty:
                    case ArrayType(typ):
                        return ExpTy(arraySubscript(varExpTy.exp, subscriptExpr.exp), self.actualTyVar(typ))
                    case ErrorType():
                        return ExpTypeError
                    case _:
                        self.errors.append('下标无法索引')
                        return ExpTypeError
            case _:
                raise Exception('对变量进行语义分析失败')

    def transTy(self, symbol: Symbol, ty: AstType) -> Type:
        match ty:
            case AstArrayType(ident):
                ty_ = self.getType(ident, AddError())
                return ArrayType(ty_)
            case AstNameType(ident):
                return self.getType(ident, AddError())
            case AstRecordType(fields):
                recordFields: list[tuple[Symbol, Type]] = []
                for field in fields:
                    typ = self.getType(field.type, AddError())
                    recordFields.append((field.name, typ))
                return RecordType(symbol, recordFields)
            case _:
                raise Exception('对类型进行语义分析失败')

    def transDec(self, declaration: AstDeclaration, parentLevel: Level, doneLabel: Optional[Label]) -> Optional[IrStatement]:
        match declaration:
            case AstFunctionDeclarationList(declarations):
                levels: list[Level] = []
                for d in declarations:
                    formals = [self.env.lookEscape(i.name) for i in d.params]
                    level = newLevel(parentLevel, NamedLabel(str(d.name)), formals)
                    if d.result == None:
                        resultType = UnitType()
                    else:
                        resultType = self.getType(d.result, AddError())
                    paramNames: list[str] = []
                    parameters: list[Type] = []
                    paramSet: set[str] = set()

                    for param in d.params:
                        parameters.append(self.getType(param.type, AddError()))
                        paramNames.append(str(param.name))
                        if str(param.name) in paramSet:
                            self.errors.append('形式参数名重复' + str(param.name))
                        else:
                            paramSet.add(str(param.name))
                    levels.append(level)
                    self.env.enterVar(d.name, FunEntry(False, NamedLabel(str(d.name)), level, parameters, resultType))

                for (node, level) in zip(declarations, levels):
                    if node.result == None:
                        resultType = UnitType()
                    else:
                        resultType = self.getType(node.result, DontAddError())
                    paramNames: list[str] = []
                    parameters: list[Type] = []
                    for param in node.params:
                        parameters.append(self.getType(param.type, DontAddError()))
                        paramNames.append(str(param.name))
                    self.env.beginScope()
                    for ((param, name), access) in zip(zip(parameters, paramNames), level.formals()):
                        self.env.enterVar(Symbol(name), VarEntry(access, param))
                    exp = self.transExp(node.body, level, doneLabel)
                    self.checkTypes(resultType, exp.ty)
                    self.gen.procEntryExit(level, exp.exp)
                    self.env.endScope()
                return None
            case AstTypeDeclarationList(typeDeclarations):
                self.checkDuplicateTypes(typeDeclarations)
                for t in typeDeclarations:
                    self.env.enterType(t.name, NameType(t.name, None))
                for t in typeDeclarations:
                    newType = self.transTy(t.name, t.type)
                    self.env.replaceType(t.name, newType)
                return None
            case AstVariableDeclaration(_, init, name, typ):
                escape = self.env.lookEscape(name)
                access = allocLocal(parentLevel, escape)
                exp = self.transExp(init, parentLevel, doneLabel)
                if isinstance(typ, Symbol):
                    typ_ = self.getType(typ, AddError())
                    self.checkTypes(typ_, exp.ty)
                elif isinstance(exp.ty, NilType):
                    self.errors.append('错误的记录类型')
                var = varDec(access, exp.exp)
                self.env.enterVar(name, VarEntry(access, exp.ty))
                return var
            case _:
                raise Exception('对声明进行语义分析失败')

#######################################
##       漂亮打印模块                  ##
##                                   ##
#######################################

E = TypeVar('E')
C = Callable[[E, int], None]

class PrettyPrintAst:
    def __init__(self, ast: AstExpression) -> None:
        self.ast = ast

    def opname(self, op: Operator) -> str:
        match op:
            case Operator.Plus:
                return '+'
            case Operator.Minus:
                return '-'
            case Operator.Times:
                return '*'
            case Operator.Divide:
                return '/'
            case Operator.Equal:
                return '='
            case Operator.Neq:
                return '<>'
            case Operator.Lt:
                return '<'
            case Operator.Le:
                return '<='
            case Operator.Gt:
                return '>'
            case Operator.Ge:
                return '>='
            case _:
                raise Exception('')

    def say(self, s: str) -> None:
        print(s, end='')

    def sayLn(self, s: str) -> None:
        print(s)

    def indent(self, depth: int) -> None:
        if depth == 0:
            return None
        else:
            self.say('  ')
            self.indent(depth - 1)

    def dolist(self, depth: int, f: C[E], l: list[E]) -> None:
        if len(l) == 0:
            return None
        elif len(l) == 1:
            self.sayLn('')
            f(l[0], depth + 1)
        else:
            self.sayLn('')
            f(l[0], depth + 1)
            self.say(',')
            self.dolist(depth, f , l[1:])

    def ppVar(self, var: AstVar, depth: int) -> None:
        match var:
            case AstSimpleVar(s):
                self.indent(depth)
                self.say('SimpleVar({})'.format(s))
            case AstFieldVar(s,v):
                self.indent(depth)
                self.sayLn('FieldVar(')
                self.ppVar(v, depth+1)
                self.say(str(s))
                self.say(')')
            case AstSubscriptVar(e,v):
                self.indent(depth)
                self.sayLn('SubscriptVar(')
                self.ppVar(v, depth + 1)
                self.sayLn(',')
                self.ppExp(e, depth + 1)
                self.say(')')
            case _:
                raise Exception('')

    def ppExp(self, expr: AstExpression, depth: int) -> None:
        match expr:
            case AstVariableExpression(v):
                self.indent(depth)
                self.sayLn('VarExp(')
                self.ppVar(v, depth + 1)
                self.say(')')
            case AstNilExpression():
                self.indent(depth)
                self.say('NilExp')
            case AstIntExpression(i):
                self.indent(depth)
                self.say('IntExp({})'.format(i))
            case AstStringExpression(s):
                self.indent(depth)
                self.say('StringExp("{}")'.format(s))
            case AstCallExpression(args, func):
                self.indent(depth)
                self.say('CallExp(')
                self.say(str(func))
                self.say(',[')
                self.dolist(depth, self.ppExp, args)
                self.say('])')
            case AstOperatorExpression(left,oper,right):
                self.indent(depth)
                self.say('OpExp(')
                self.say(self.opname(oper))
                self.sayLn(',')
                self.ppExp(left, depth + 1)
                self.sayLn(',')
                self.ppExp(right, depth + 1)
                self.say(')')
            case AstRecordExpression(fields,typ):
                def f(rf: AstRecordField, d: int) -> None:
                    self.indent(d)
                    self.say('(')
                    self.say(str(rf.ident))
                    self.sayLn(',')
                    self.ppExp(rf.expr, d+1)
                    self.say(')')
                self.indent(depth)
                self.say('RecordExp(')
                self.say(str(typ))
                self.sayLn(',[')
                self.dolist(depth, f, fields)
                self.say('])')
            case AstSequenceExpression(l):
                self.indent(depth)
                self.say('SeqExp[')
                self.dolist(depth, self.ppExp, l)
                self.say(']')
            case AstIfExpression(else_,test,then):
                self.indent(depth)
                self.sayLn('IfExp(')
                self.ppExp(test,depth+1)
                self.sayLn(',')
                self.ppExp(then,depth+1)
                if else_ == None:
                    pass
                else:
                    self.sayLn(',')
                    self.ppExp(else_,depth+1)
                self.say(')')
            case AstWhileExpression(body,test):
                self.indent(depth)
                self.sayLn('WhileExp(')
                self.ppExp(test,depth+1)
                self.sayLn(',')
                self.ppExp(body,depth+1)
                self.say(')')
            case AstBreakExpression():
                self.indent(depth)
                self.say('BreakExp')
            case AstLetExpression(body,declarations):
                self.indent(depth)
                self.say('LetExp[')
                self.dolist(depth, self.ppDec, declarations)
                self.sayLn('],')
                self.ppExp(body, depth+1)
                self.say(')')
            case AstArrayExpression(init,size,typ):
                self.indent(depth)
                self.sayLn('ArrayExp({},'.format(typ))
                self.ppExp(size, depth+1)
                self.sayLn(',')
                self.ppExp(init, depth+1)
                self.say(')')
            case AstAssignExpression(e,v):
                self.indent(depth)
                self.sayLn('AssignExp(')
                self.ppVar(v, depth+1)
                self.sayLn(',')
                self.ppExp(e, depth+1)
                self.say(')')
            case _:
                raise Exception('')

    def ppTy(self, ty: AstType, depth: int) -> None:
        match ty:
            case AstNameType(s):
                self.indent(depth)
                self.say('NameTy({})'.format(s))
            case AstRecordType(l):
                def f(field: AstField, d: int):
                    self.indent(d)
                    self.say('({},{},{})'.format(field.name,field.escape,field.type))
                self.indent(depth)
                self.say('RecordTy[')
                self.dolist(depth, f, l)
                self.say(']')
            case AstArrayType(s):
                self.indent(depth)
                self.say('ArrayTy({})'.format(s))
            case _:
                raise Exception('')

    def ppDec(self, dec: AstDeclaration, depth: int) -> None:
        match dec:
            case AstFunctionDeclarationList(l):
                def field(fd: AstField, d: int) -> None:
                    self.indent(d)
                    self.say('({},{},{})'.format(fd.name,fd.escape,fd.type))
                def f(fundec: AstFunctionDeclaration, d: int) -> None:
                    self.indent(d)
                    self.say('(')
                    self.say(str(fundec.name))
                    self.say(',[')
                    self.dolist(d, field, fundec.params)
                    self.sayLn('],')
                    if fundec.result == None:
                        self.say('None')
                    else:
                        self.say('Some({})'.format(fundec.result))
                    self.say(',')
                    self.ppExp(fundec.body, d + 1)
                    self.say(')')
                self.indent(depth)
                self.say('FunctionDec[')
                self.dolist(depth, f, l)
                self.say(']')
            case AstVariableDeclaration(escape,init,name,typ):
                self.indent(depth)
                self.say('VarDec({},{},'.format(name,escape))
                if typ == None:
                    self.say('None')
                else:
                    self.say('Some({})'.format(typ))
                self.sayLn(',')
                self.ppExp(init, depth+1)
                self.say(')')
            case AstTypeDeclarationList(l):
                def tdec(tydec: AstTypeDeclaration, d: int) -> None:
                    self.indent(d)
                    self.say('(')
                    self.say(str(tydec.name))
                    self.sayLn(',')
                    self.ppTy(tydec.type, d + 1)
                    self.say(')')
                self.indent(depth)
                self.say('TypeDec[')
                self.dolist(depth,tdec,l)
                self.say(']')
            case _:
                raise Exception('')



class PrettyPrintIr:
    def __init__(self) -> None:
        pass

    def say(self, s: str) -> None:
        print(s, end='')

    def sayLn(self, s: str) -> None:
        print(s)

    def indent(self, depth: int) -> None:
        if depth == 0:
            return None
        else:
            self.say('  ')
            self.indent(depth - 1)
    
    def binop(self, op: BinOp) -> None:
        match op:
            case BinOp.Plus: self.say('+')
            case BinOp.Minus: self.say('-')
            case BinOp.Mul: self.say('*')
            case BinOp.Div: self.say('/')
            case BinOp.And: self.say('&')
            case BinOp.Or: self.say('|')
            case BinOp.ShiftLeft: self.say('shl')
            case BinOp.ShiftRight: self.say('shr')
            case BinOp.ArithmeticShiftRight: self.say('sar')
            case BinOp.Xor: self.say('xor')
            case _: raise Exception('')

    def relop(self, op: RelationalOp) -> None:
        match op:
            case RelationalOp.Equal: self.say('=')
            case RelationalOp.NotEqual: self.say('<>')
            case RelationalOp.LesserThan: self.say('<')
            case RelationalOp.LesserOrEqual: self.say('<=')
            case RelationalOp.GreaterThan: self.say('>')
            case RelationalOp.GreaterOrEqual: self.say('>=')
            case RelationalOp.UnsignedLesserThan: self.say('ult')
            case RelationalOp.UnsignedLesserOrEqual: self.say('ule')
            case RelationalOp.UnsignedGreaterThan: self.say('ugt')
            case RelationalOp.UnsignedGreaterOrEqual: self.say('uge')
            case _: raise Exception('')

    def ppIrExp(self, exp: IrExp, depth: int) -> None:
        match exp:
            case IrBinOp(p,a,b):
                self.indent(depth)
                self.say('BINOP(')
                self.binop(p)
                self.sayLn(',')
                self.ppIrExp(a, depth+1)
                self.sayLn(',')
                self.ppIrExp(b, depth+1)
                self.say(')')
            case IrMem(e):
                self.indent(depth)
                self.sayLn('MEM(')
                self.ppIrExp(e, depth+1)
                self.say(')')
            case IrTemp(t):
                self.indent(depth)
                self.say('TEMP {}'.format(t))
            case IrExpSequence(s,e):
                self.indent(depth)
                self.sayLn('ESEQ(')
                self.ppIrStm(s, depth+1)
                self.sayLn(',')
                self.ppIrExp(e, depth+1)
                self.say(')')
            case IrName(label):
                self.indent(depth)
                self.say('NAME ')
                self.say(str(label))
            case IrConst(i):
                self.indent(depth)
                self.say('CONST ')
                self.say(str(i))
            case IrCall(e,el):
                self.indent(depth)
                self.sayLn('CALL(')
                self.ppIrExp(e, depth+1)
                for a in el:
                    self.sayLn(',')
                    self.ppIrExp(a, depth+2)
                self.say(')')
            case _:
                raise Exception('')

    def ppIrStm(self, stm: IrStatement, depth: int) -> None:
        match stm:
            case IrSequence(a, b):
                self.indent(depth)
                self.sayLn('SEQ(')
                self.ppIrStm(a, depth+1)
                self.sayLn(',')
                self.ppIrStm(b, depth+1)
                self.say(')')
            case IrLabel(lab):
                self.indent(depth)
                self.say('LABEL ')
                self.say(str(lab))
            case IrJump(e,_):
                self.indent(depth)
                self.sayLn('JUMP(')
                self.ppIrExp(e, depth+1)
                self.say(')')
            case IrCondJump(r,a,b,t,f):
                self.indent(depth)
                self.say('CJUMP(')
                self.relop(r)
                self.sayLn(',')
                self.ppIrExp(a, depth+1)
                self.sayLn(',')
                self.ppIrExp(b, depth+1)
                self.sayLn(',')
                self.indent(depth+1)
                self.say(str(t))
                self.say(',')
                self.say(str(f))
                self.say(')')
            case IrMove(a,b):
                self.indent(depth)
                self.sayLn('MOVE(')
                self.ppIrExp(a, depth+1)
                self.sayLn(',')
                self.ppIrExp(b, depth+1)
                self.say(')')
            case IrExpStatement(e):
                self.indent(depth)
                self.sayLn('EXP(')
                self.ppIrExp(e, depth+1)
                self.say(')')
            case _:
                raise Exception('')

# 将树形IR线性化

def commute(stmt: IrStatement, expr: IrExp) -> bool:
    match (stmt, expr):
        case (IrExpStatement(IrConst(_)),_):
            return True
        case (_, IrName(_)):
            return True
        case _:
            return False

def appendTwoStatement(statement1: IrStatement, statement2: IrStatement) -> IrStatement:
    match (statement1, statement2):
        case (IrExpStatement(IrConst(_)), statement2_):
            return statement2_
        case (statement1_, IrExpStatement(IrConst(_))):
            return statement1_
        case (statement1_, statement2_):
            return IrSequence(statement1_, statement2_)

def linearize(statement: IrStatement) -> list[IrStatement]:
    statement = doStatement(statement)

    def linear(stmt: IrStatement, result: list[IrStatement]):
        if isinstance(stmt, IrSequence):
            linear(stmt.stmt1, result)
            linear(stmt.stmt2, result)
        else:
            result.append(stmt)

    result: list[IrStatement] = []
    linear(statement, result)
    return result

def doStatement(statement: IrStatement) -> IrStatement:
    match statement:
        case IrSequence(statement1, statement2):
            return appendTwoStatement(doStatement(statement1), doStatement(statement2))
        case IrJump(expr, labels):
            return reorderStatement1(expr, lambda e: IrJump(e, labels))
        case IrCondJump(op, left, right, trueLabel, falseLabel):
            return reorderStatement2(left, right, lambda l, r: IrCondJump(op, l, r, trueLabel, falseLabel))
        case IrMove(IrTemp(temp), IrCall(function, arguments)):
            exprs = [function] + arguments
            def builder(exprs: list[IrExp]) -> IrStatement:
                function = exprs.pop(0)
                return IrMove(IrTemp(temp), IrCall(function, exprs))
            return reorderStatement(exprs, builder)
        case IrMove(IrTemp(temp), expr):
            return reorderStatement1(expr, lambda e: IrMove(IrTemp(temp), e))
        case IrMove(IrMem(mem), expr):
            return reorderStatement2(mem, expr, lambda m, e: IrMove(IrMem(m), e))
        case IrMove(IrExpSequence(statement, expr1), expr2):
            return IrSequence(statement, IrMove(expr1, expr2))
        case IrExpStatement(IrCall(function, arguments)):
            exprs = [function] + arguments
            def builder(exprs: list[IrExp]) -> IrStatement:
                function = exprs.pop(0)
                return IrExpStatement(IrCall(function, exprs))
            return reorderStatement(exprs, builder)
        case IrExpStatement(expr):
            return reorderStatement1(expr, lambda e: IrExpStatement(e))
        case _:
            return statement

def doExpression(expr: IrExp) -> tuple[IrStatement, IrExp]:
    match expr:
        case IrBinOp(op, left, right):
            return reorderExpression2(left, right, lambda l, r: IrBinOp(op, l, r))
        case IrMem(expr):
            return reorderExpression1(expr, lambda e: IrMem(e))
        case IrExpSequence(statement, expr):
            statements1 = doStatement(statement)
            (statements2, expr_) = doExpression(expr)
            return (
                appendTwoStatement(statements1, statements2),
                expr_
            )
        case IrCall(function, arguments):
            exprs = [function] + arguments
            def builder(exprs: list[IrExp]) -> IrExp:
                function = exprs.pop(0)
                return IrCall(function, exprs)
            return reorderExpression(exprs, builder)
        case _:
            return (IrExpStatement(IrConst(0)), expr)

def reorder1(expr: IrExp) -> tuple[IrStatement, IrExp]:
    return doExpression(expr)

def reorder2(expr1: IrExp, expr2: IrExp) -> tuple[IrStatement, IrExp, IrExp]:
    if isinstance(expr1, IrCall):
        temp = Temp()
        return reorder2(
            IrExpSequence(
                IrMove(IrTemp(temp), expr1),
                IrTemp(temp)
            ),
            expr2
        )
    
    (statements1, expr1) = doExpression(expr1)
    (statements2, expr2) = doExpression(expr2)
    if commute(statements2, expr1):
        return (IrSequence(statements1, statements2), expr1, expr2)
    else:
        temp = Temp()
        statements = IrSequence(statements1, IrSequence(IrMove(IrTemp(temp), expr1), statements2))
        return (statements, IrTemp(temp), expr2)

def reorder(exprs: list[IrExp]) -> tuple[IrStatement, list[IrExp]]:
    if len(exprs) == 0:
        return (IrExpStatement(IrConst(0)), [])
    
    if isinstance(exprs[0], IrCall):
        temp = Temp()
        function = exprs.pop(0)
        exprs.insert(0, IrExpSequence(IrMove(IrTemp(temp), function), IrTemp(temp)))
        return reorder(exprs)

    (statements, expr1) = doExpression(exprs.pop(0))
    (statements2, expr2) = reorder(exprs)

    if commute(statements2, expr1):
        expr2.insert(0, expr1)
        return (appendTwoStatement(statements, statements2), expr2)
    else:
        temp = Temp()
        statements_ = appendTwoStatement(
            statements,
            appendTwoStatement(
                IrMove(IrTemp(temp), expr1),
                statements2
            )
        )
        expr2.insert(0, IrTemp(temp))
        return (statements_, expr2)

def reorderExpression1(expr: IrExp, builder: Callable[[IrExp], IrExp]) -> tuple[IrStatement, IrExp]:
    (statements, expr) = reorder1(expr)
    return (statements, builder(expr))

def reorderExpression2(expr1: IrExp, expr2: IrExp, builder: Callable[[IrExp, IrExp], IrExp]) -> tuple[IrStatement, IrExp]:
    (statements, expr1, expr2) = reorder2(expr1, expr2)
    return (statements, builder(expr1, expr2))

def reorderExpression(exprs: list[IrExp], builder: Callable[[list[IrExp]], IrExp]) -> tuple[IrStatement, IrExp]:
    (statements, exprs_) = reorder(exprs)
    return (statements, builder(exprs_))

def reorderStatement1(expr: IrExp, builder: Callable[[IrExp], IrStatement]) -> IrStatement:
    (statements, expr) = reorder1(expr)
    return IrSequence(statements, builder(expr))

def reorderStatement2(expr1: IrExp, expr2: IrExp, builder: Callable[[IrExp, IrExp], IrStatement]) -> IrStatement:
    (statements, expr1_, expr2_) = reorder2(expr1, expr2)
    return IrSequence(statements, builder(expr1_, expr2_))

def reorderStatement(exprs: list[IrExp], builder: Callable[[list[IrExp]], IrStatement]) -> IrStatement:
    (statements, exprs_) = reorder(exprs)
    return IrSequence(statements, builder(exprs_))

# 调整线性化后的IR

def negateCondition(op: RelationalOp) -> RelationalOp:
    match op:
        case RelationalOp.Equal: return RelationalOp.NotEqual
        case RelationalOp.GreaterOrEqual: return RelationalOp.LesserThan
        case RelationalOp.GreaterThan: return RelationalOp.LesserOrEqual
        case RelationalOp.LesserOrEqual: return RelationalOp.GreaterThan
        case RelationalOp.LesserThan: return RelationalOp.GreaterOrEqual
        case RelationalOp.NotEqual: return RelationalOp.Equal
        case RelationalOp.UnsignedGreaterOrEqual: return RelationalOp.UnsignedLesserThan
        case RelationalOp.UnsignedGreaterThan: return RelationalOp.UnsignedLesserOrEqual
        case RelationalOp.UnsignedLesserOrEqual: return RelationalOp.UnsignedGreaterThan
        case RelationalOp.UnsignedLesserThan: return RelationalOp.UnsignedGreaterOrEqual

def basicBlocks(statements: list[IrStatement]) -> tuple[list[list[IrStatement]], Label]:
    '''
    将语句序列转换成基本块组成的列表
    基本块也是语句序列
    '''
    done = NumLabel()

    class State(Enum):
        Label = 1
        InBlock = 2

    state = State.Label
    basicBlocks: list[list[IrStatement]] = []
    for statement in statements:
        if state == State.Label:
            state = State.InBlock
            if isinstance(statement, IrLabel):
                basicBlocks.append([statement])
            else:
                basicBlocks.append([IrLabel(NumLabel())])
        
        if state == State.InBlock:
            match statement:
                case IrLabel(label):
                    lastBB = basicBlocks[len(basicBlocks) - 1]
                    lastBB.append(IrJump(IrName(label), [label]))
                    basicBlocks.append([statement])
                case IrJump(_) | IrCondJump(_):
                    lastBB = basicBlocks[len(basicBlocks) - 1]
                    lastBB.append(statement)
                    state = State.Label
                case _:
                    lastBB = basicBlocks[len(basicBlocks) - 1]
                    lastBB.append(statement)
        
    if state == State.InBlock:
        lastBB = basicBlocks[len(basicBlocks) - 1]
        lastBB.append(IrJump(IrName(done), [done]))

    return (basicBlocks, done)

def traceSchedule(basicBlocks: list[list[IrStatement]], doneLabel: Label) -> list[IrStatement]:
    labelMapping: dict[Label, int] = {}
    labelMapping[doneLabel] = 1000000000
    for i in range(len(basicBlocks)):
        first = basicBlocks[i][0]
        if isinstance(first, IrLabel):
            labelMapping[first.label] = i
        else:
            raise Exception('label as first statement of basic block')

    marks: set[int] = set()
    traces: list[list[int]] = []
    currentTrace: list[int] = []

    for (index, basicBlock) in enumerate(basicBlocks):
        currentTrace = []
        while index not in marks:
            marks.add(index)
            currentTrace.append(index)

            last = basicBlock[len(basicBlock) - 1]
            if isinstance(last, IrCondJump):
                if labelMapping[last.false_label] in marks:
                    index = labelMapping[last.false_label]
                elif labelMapping[last.true_label] in marks:
                    index = labelMapping[last.true_label]
            elif isinstance(last, IrJump):
                for label in last.labels:
                    if labelMapping[label] in marks:
                        index = labelMapping[label]
                        break
            else:
                raise Exception('Expected jump as last statement of basic blocks')
        
        if len(currentTrace) != 0:
            traces.append(currentTrace)

    statements: list[IrStatement] = []

    for trace in traces:
        for index in trace:
            traceStatements = basicBlocks[index]
            basicBlocks[index] = []
            for statement in traceStatements:
                statements.append(statement)

    newStatements: list[IrStatement] = []

    try:
        current = statements.pop(0)
    except:
        current = None

    while current != None:
        match current:
            case IrCondJump(op, left, right, falseLabel, trueLabel):
                next = statements.pop(0)
                if next == IrLabel(trueLabel):
                    newStatements.append(IrCondJump(
                        negateCondition(op),
                        left,
                        right,
                        trueLabel,
                        falseLabel
                    ))
                elif next == IrLabel(falseLabel):
                    newStatements.append(IrCondJump(
                        op,
                        left,
                        right,
                        falseLabel,
                        trueLabel
                    ))
                else:
                    newFalse = NumLabel()
                    newStatements.append(IrCondJump(
                        op,
                        left,
                        right,
                        newFalse,
                        trueLabel
                    ))
                    newStatements.append(IrLabel(newFalse))
                    newStatements.append(IrJump(
                        IrName(falseLabel),
                        [falseLabel]
                    ))
                newStatements.append(next)
            case IrJump(expr, labels):
                match expr:
                    case IrName(label):
                        if len(labels) == 1 and labels[0] == label:
                            if len(statements) > 0:
                                front = statements[0]
                                if isinstance(front, IrLabel):
                                    if front.label == label:
                                        current = statements.pop(0)
                                        continue

                        newStatements.append(IrJump(IrName(label), labels))
                    case _:
                        newStatements.append(IrJump(expr, labels))
            case _:
                newStatements.append(current)
        try:
            current = statements.pop(0)
        except:
            current = None

    newStatements.append(IrLabel(doneLabel))

    return newStatements

# 指令选择

def calldefs() -> list[Temp]:
    registers = callerSavedRegisters()
    registers += argRegisters()
    registers.append(returnValue())
    return registers

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

class InstructionGen:
    def __init__(self) -> None:
        self.instructions: list[Instruction] = []

    def emit(self, instruction: Instruction) -> None:
        self.instructions.append(instruction)

    def munchArgs(self, arguments: list[IrExp]) -> list[Temp]:
        temps: list[Temp] = []

        for register in argRegisters():
            if len(arguments) > 0:
                argument = arguments.pop(0)
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(argument)],
                    [register]
                )
                self.emit(instruction)
                temps.append(register)
            else:
                break

        instructions = [OperatorInstruction("push 's0", [self.munchExpression(argument), RSP], [RSP], None) for argument in arguments]
        instructions.reverse()

        for instruction in instructions:
            self.emit(instruction)

        return temps

    def munchExpression(self, expr: IrExp) -> Temp:
        temp = Temp()
        match expr:
            case IrName(label):
                instruction = MoveInstruction(
                    "mov 'd0, {}".format(label),
                    [],
                    [temp]
                )
                self.emit(instruction)
            case IrMem(IrBinOp(BinOp.Plus, expr, IrConst(num))) | IrMem(IrBinOp(BinOp.Plus, IrConst(num), expr)):
                instruction = MoveInstruction(
                    "mov 'd0, ['s0 + {}]".format(num),
                    [self.munchExpression(expr)],
                    [temp]
                )
                self.emit(instruction)
            case IrMem(IrConst(num)):
                instruction = MoveInstruction(
                    "mov 'd0, [{}]".format(num),
                    [],
                    [temp]
                )
                self.emit(instruction)
            case IrMem(expr):
                instruction = MoveInstruction(
                    "mov 'd0, ['s0]",
                    [self.munchExpression(expr)],
                    [temp]
                )
                self.emit(instruction)
            case IrBinOp(BinOp.Plus, expr, IrConst(num)) | IrBinOp(BinOp.Plus, IrConst(num), expr):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(expr)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "add 'd0, {}".format(num),
                    [temp],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.Minus, expr, IrConst(num)):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(expr)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "sub 'd0, {}".format(num),
                    [temp],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.Minus, IrConst(num), expr):
                instruction = MoveInstruction(
                    "mov 'd0, {}".format(num),
                    [],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "sub 'd0, 's0",
                    [self.munchExpression(expr), temp],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.Mul, expr, IrConst(num)) | IrBinOp(BinOp.Mul, IrConst(num), expr):
                instruction = MoveInstruction(
                    "mov 'd0, {}".format(num),
                    [],
                    [temp]
                )
                self.emit(instruction)
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(expr)],
                    [RAX]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "mul 's0",
                    [temp, RAX],
                    [RAX, RDX],
                    None,
                )
                self.emit(instruction)
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [RAX],
                    [temp]
                )
                self.emit(instruction)            
            case IrBinOp(BinOp.Div, left, IrConst(num)):
                instruction = MoveInstruction(
                    "mov 'd0, 0",
                    [],
                    [RDX]
                )
                self.emit(instruction)
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(left)],
                    [RAX]
                )
                self.emit(instruction)
                immediate = Temp()
                instruction = MoveInstruction(
                    "mov 'd0, {}".format(num),
                    [],
                    [immediate]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "idiv 's0",
                    [immediate, RAX, RDX],
                    [RAX, RDX],
                    None
                )
                self.emit(instruction)
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [RAX],
                    [temp]
                )
                self.emit(instruction)
            case IrBinOp(BinOp.Div, IrConst(num), expr):
                instruction = MoveInstruction(
                    "mov 'd0, 0",
                    [],
                    [RDX]
                )
                self.emit(instruction)
                instruction = MoveInstruction(
                    "mov 'd0, {}".format(num),
                    [],
                    [RAX]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "idiv 's0",
                    [self.munchExpression(expr), RAX, RDX],
                    [RAX, RDX],
                    None
                )
                self.emit(instruction)
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [RAX],
                    [temp]
                )
                self.emit(instruction)
            case IrBinOp(BinOp.And, expr, IrConst(num)) | IrBinOp(BinOp.And, IrConst(num), expr):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(expr)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "and 'd0, {}".format(num),
                    [],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.Or, expr, IrConst(num)) | IrBinOp(BinOp.Or, IrConst(num), expr):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(expr)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "or 'd0, {}".format(num),
                    [],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.ShiftLeft, expr, IrConst(num)) | IrBinOp(BinOp.ShiftLeft, IrConst(num), expr):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(expr)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "sal 'd0, {}".format(num),
                    [],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.ArithmeticShiftRight, expr, IrConst(num)) | IrBinOp(BinOp.ArithmeticShiftRight, IrConst(num), expr):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(expr)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "sar 'd0, {}".format(num),
                    [],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.ShiftRight, expr, IrConst(num)) | IrBinOp(BinOp.ShiftRight, IrConst(num), expr):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(expr)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "shr 'd0, {}".format(num),
                    [],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.Xor, expr, IrConst(num)) | IrBinOp(BinOp.Xor, IrConst(num), expr):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(expr)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "xor 'd0, {}".format(num),
                    [],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrConst(num):
                instruction = MoveInstruction(
                    "mov 'd0, {}".format(num),
                    [],
                    [temp]
                )
                self.emit(instruction)
            case IrBinOp(BinOp.Plus, left, right):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(left)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "add 'd0, 's0",
                    [self.munchExpression(right), temp],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.Minus, left, right):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(left)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "sub 'd0, 's0",
                    [self.munchExpression(right), temp],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.Mul, left, right):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(left)],
                    [RAX]
                )
                self.emit(instruction)
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(right)],
                    [RAX]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "mul 's0",
                    [temp, RAX],
                    [RAX, RDI],
                    None
                )
                self.emit(instruction)
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [RAX],
                    [temp]
                )
                self.emit(instruction)
            case IrBinOp(BinOp.Div, left, right):
                instruction = MoveInstruction(
                    "mov 'd0, 0",
                    [],
                    [RDX]
                )
                self.emit(instruction)
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(left)],
                    [RAX]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "idiv 's0",
                    [self.munchExpression(right)],
                    [RAX, RDX],
                    None
                )
                self.emit(instruction)
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [RAX],
                    [temp]
                )
                self.emit(instruction)
            case IrBinOp(BinOp.And, left, right):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(left)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "and 'd0, 's0",
                    [self.munchExpression(right)],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.Or, left, right):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(left)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "or 'd0, 's0",
                    [self.munchExpression(right)],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.ShiftLeft, left, right):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(left)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "sal 'd0, 's0",
                    [self.munchExpression(right)],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.ArithmeticShiftRight, left, right):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(left)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "sar 'd0, 's0",
                    [self.munchExpression(right)],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.ShiftRight, left, right):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(left)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "shr 'd0, 's0",
                    [self.munchExpression(right)],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrBinOp(BinOp.Xor, left, right):
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [self.munchExpression(left)],
                    [temp]
                )
                self.emit(instruction)
                instruction = OperatorInstruction(
                    "xor 'd0, 's0",
                    [self.munchExpression(right)],
                    [temp],
                    None
                )
                self.emit(instruction)
            case IrTemp(temp):
                return temp
            case IrCall(IrName(label), arguments):
                argumentCount = len(arguments)
                source = self.munchArgs(arguments)
                instruction = OperatorInstruction(
                    "call {}".format(label),
                    source,
                    calldefs(),
                    None
                )
                self.emit(instruction)
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [RAX],
                    [temp]
                )
                self.emit(instruction)
                argRegisterCount = len(argRegisters())
                if argumentCount > argRegisterCount:
                    stackArgCount = argumentCount - len(argRegisters())
                    instruction = OperatorInstruction(
                        "add 'd0, {}".format(stackArgCount * WORD_SIZE),
                        [],
                        [RSP],
                        None
                    )
                    self.emit(instruction)
            case IrCall(function, arguments):
                argumentCount = len(arguments)
                source = [self.munchExpression(function)]
                instruction = OperatorInstruction(
                    "call 's0",
                    source,
                    calldefs(),
                    None
                )
                self.emit(instruction)
                instruction = MoveInstruction(
                    "mov 'd0, 's0",
                    [RAX],
                    [temp]
                )
                self.emit(instruction)
                argRegisterCount = len(argRegisters())
                if argumentCount > argRegisterCount:
                    stackArgCount = argumentCount - len(argRegisters())
                    instruction = OperatorInstruction(
                        "add 'd0, {}".format(stackArgCount * WORD_SIZE),
                        [],
                        [RSP],
                        None
                    )
                    self.emit(instruction)
            case _:
                raise Exception('')
        return temp

    def munchStatement(self, statement: IrStatement) -> None:
        match statement:
            case IrSequence(statement1, statement2):
                self.munchStatement(statement1)
                self.munchStatement(statement2)
            case IrMove(IrMem(IrBinOp(BinOp.Plus, memoryDestination, IrConst(num))), expr) | IrMove(IrMem(IrBinOp(BinOp.Plus, IrConst(num), memoryDestination)), expr):
                instruction = MoveInstruction(
                    "mov ['s0 + {}], 's1".format(num),
                    # FIXME: might be wrong if expr is in memory as Intel might not allow a move from memory to memory.
                    [self.munchExpression(memoryDestination), self.munchExpression(expr)],
                    []
                )
                self.emit(instruction)
            case IrMove(IrMem(IrConst(num)), expr):
                instruction = MoveInstruction(
                    "mov [{}], ['s0]".format(num),
                    # FIXME: not sure move from memory to memory is allowed.
                    [self.munchExpression(expr)],
                    []
                )
                self.emit(instruction)
            case IrMove(IrMem(destination), source):
                instruction = MoveInstruction(
                    "mov ['s0], 's1",
                    [
                        self.munchExpression(destination),
                        self.munchExpression(source)
                    ],
                    []
                )
                self.emit(instruction)
            case IrMove(IrTemp(temp), source):
                match source:
                    case IrMem(IrBinOp(BinOp.Plus, expr, IrConst(num))):
                        if temp in registers():
                            instruction = MoveInstruction(
                                "mov 'd0, ['s0 + {}]".format(num),
                                [self.munchExpression(expr)],
                                [temp]
                            )
                        else:
                            instruction = MoveInstruction(
                                "mov 'd0, 's0",
                                [self.munchExpression(source)],
                                [temp]
                            )
                    case _:
                        instruction = MoveInstruction(
                            "mov 'd0, 's0",
                            [self.munchExpression(source)],
                            [temp]
                        )
                self.emit(instruction)
            case IrExpStatement(IrConst(_)):
                pass
            case IrExpStatement(exp):
                self.munchExpression(exp)
            case IrLabel(label):
                instruction = LabelInstruction(
                    "{}:".format(label),
                    label
                )
                self.emit(instruction)
            case IrJump(exp, labels):
                match exp:
                    case IrName(label):
                        instruction = OperatorInstruction(
                            "jmp {}".format(label),
                            [],
                            [],
                            labels
                        )
                        self.emit(instruction)
                    case _:
                        raise Exception('Unexpected jump expression: {}'.format(exp))
            case IrCondJump(op, left, right, falseLabel, trueLabel):
                instruction = OperatorInstruction(
                    "cmp 's0, 's1",
                    [self.munchExpression(left), self.munchExpression(right)],
                    [],
                    None
                )
                self.emit(instruction)

                opcode = None
                match op:
                    case RelationalOp.Equal: opcode = "je"
                    case RelationalOp.NotEqual: opcode = "jne"
                    case RelationalOp.LesserThan: opcode = "jl"
                    case RelationalOp.GreaterThan: opcode = "jg"
                    case RelationalOp.LesserOrEqual: opcode = "jle"
                    case RelationalOp.GreaterOrEqual: opcode = "jge"
                    case RelationalOp.UnsignedLesserThan: opcode = "jb"
                    case RelationalOp.UnsignedLesserOrEqual: opcode = "jbe"
                    case RelationalOp.UnsignedGreaterThan: opcode = "jbe"
                    case RelationalOp.UnsignedGreaterOrEqual: opcode = "jbe"

                instruction = OperatorInstruction(
                    "{} {}".format(opcode, trueLabel),
                    [],
                    [],
                    [falseLabel, trueLabel]
                )
                self.emit(instruction)

            case _:
                raise Exception('无法为IrStatement产生汇编代码')

    def getResult(self) -> list[Instruction]:
        return self.instructions

def procEntryExit2(instructions: list[Instruction]) -> list[Instruction]:
    source = calleeSavedRegisters()
    source += specialRegisters()
    instruction = OperatorInstruction(
        "",
        source,
        [],
        []
    )
    instructions.append(instruction)

    for instruction in instructions:
        match instruction:
            case LabelInstruction(_):
                pass
            case MoveInstruction(_, destination, _) | OperatorInstruction(_, destination, _, _):
                destination.append(RBP)
                destination.append(RSP)
                break
            case _:
                raise Exception('')

    for instruction in reversed(instructions):
        match instruction:
            case LabelInstruction(_):
                pass
            case MoveInstruction(_, _, source) | OperatorInstruction(_, _, source, _):
                source.append(RBP)
                source.append(RSP)
                break
            case _:
                raise Exception('')

    return instructions



# 测试
source = """
let function add(a:int,b:int):int = a + b in add(1,2) end
"""
lexer = Lexer(source)
tokens = lexer.scanTokens()
parser = Parser(tokens)
ast = parser.parse()
pp = PrettyPrintAst(ast)
print('================== 抽象语法树 ====================')
pp.ppExp(ast, 0)
print('\n================== 抽象语法树 ====================')
escapeEnv = findEscapes(ast)
env = Environment(escapeEnv)
sematic = SemanticAnalyzer(env)
mainSymbol = Symbol('main')
fragments = sematic.analyze(mainSymbol, ast)
print('================== 中间表示 ====================')
for fragment in fragments:
    match fragment:
        case FunctionFragment(body,_):
            ppir = PrettyPrintIr()
            ppir.ppIrStm(body, 0)
            print()
        case StrFragment(_, string):
            print('String {}'.format(string))
        case _:
            raise Exception('打印代码片段异常')
print('\n================== 中间表示 ====================')
print()
print('================== 线性化后的中间表示 ====================')
for fragment in fragments:
    match fragment:
        case FunctionFragment(body,_):
            ppir = PrettyPrintIr()
            statements = linearize(body)
            (basicBlocks_, doneLabel) = basicBlocks(statements)
            statements = traceSchedule(basicBlocks_, doneLabel)

            generator = InstructionGen()
            for statement in statements:
                ppir.ppIrStm(statement, 0)
                generator.munchStatement(statement)
                print()
            instructions = generator.getResult()
            instructions = procEntryExit2(instructions)
            for instruction in instructions:
                print(instruction)
            print()
            print()
        case StrFragment(_, string):
            print('String {}'.format(string))
        case _:
            raise Exception('打印代码片段异常')
print('\n================== 线性化的中间表示 ====================')