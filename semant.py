from dataclasses import dataclass
from syntax import AstArrayExpression, AstArrayType, AstAssignExpression, AstBreakExpression, AstCallExpression, AstDeclaration, AstExpression, AstFieldVar, AstFunctionDeclaration, AstFunctionDeclarationList, AstIfExpression, AstIntExpression, AstLetExpression, AstNameType, AstNilExpression, AstOperatorExpression, AstRecordExpression, AstSequenceExpression, AstSimpleVar, AstStringExpression, AstVar, AstSubscriptVar, AstTypeDeclaration, AstTypeDeclarationList, AstVariableDeclaration, AstVariableExpression, AstWhileExpression, AstRecordType, AstType, Operator
from tiger_data_type import Type, ErrorType, IntType, UnitType, StringType, ArrayType, RecordType, NameType, NilType
from ir import IrExp, IrError, IrStatement, IrSequence, IrExpStatement, IrExpSequence, IrMove
from env import Entry, Environment, ErrorEntry, FunEntry, VarEntry
from gen import Fragment, Gen, binaryOper, goto, ifExpression, num, stringEquality, varDec, relationalOper, recordCreate, varDecs, simpleVar, arraySubscript, unit, functionCall, whileLoop, fieldAccess
from temp import Label, NamedLabel, NumLabel
from frame import outermost, Label, Level, alloc_local, externalCall, newLevel
from symbol import Symbol
from typing import Optional

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
                    return ExpTy(ifExpression(testExpr.exp, ifExpr.exp, None, level), UnitType())
                else:
                    elseExpr = self.transExp(else_, level, doneLabel)
                    self.checkTypes(ifExpr.ty, elseExpr.ty)
                    return ExpTy(ifExpression(testExpr.exp, ifExpr.exp, elseExpr.exp, level), elseExpr.ty)
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
                access = alloc_local(parentLevel, escape)
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