from enum import Enum
from dataclasses import dataclass

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