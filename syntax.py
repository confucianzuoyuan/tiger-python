from enum import Enum
from dataclasses import dataclass
from symbol import Symbol
from typing import Optional

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
    '''
    else_: Optional[AstExpression]
    test: AstExpression
    then: AstExpression
    '''
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