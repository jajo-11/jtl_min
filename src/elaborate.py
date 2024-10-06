from typing import Set, Dict, Tuple

from ast_types import *
from elaborate_types import *
from errors import ElaborationError, ElaborationErrorType, JTLTypeError, JTLTypeErrorType


@dataclass(slots=True)
class TemporaryProvider:
    count: int

    def next(self, location: CodeLocation, type: Type) -> Temporary:
        self.count += 1
        return Temporary(self.count, location, type)


GlobalTemporaryProvider = TemporaryProvider()


def elaborate_scope(parent: Scope, nodes: List[ASTNode]):
    for node in nodes:
        elaborate_statement(parent, node)


def elaborate_statement(parent: Scope, node: ASTNode):
    match node:
        case ASTNodeStatement():
            match node.token:
                case TokenKeyword(keyword=Keyword.VARIABLE):
                    elaborate_declaration(parent, node.value, False)
                case TokenKeyword(keyword=Keyword.LET):
                    elaborate_declaration(parent, node.value, False)
                case TokenKeyword(keyword=Keyword.CONSTANT):
                    elaborate_declaration(parent, node.value, True)
                case _:
                    elaborate_expression(parent, node, False)


def elaborate_declaration(parent: Scope, node: ASTNode, constant: bool):
    if not (isinstance(node, ASTNodeBinary) and isinstance(node.token, TokenOperator)
            and node.token.op == Operator.ASSIGNMENT):
        raise ElaborationError.from_type(ElaborationErrorType.NO_ASSIGNMENT_DECLARATION,
                                         node.token.location.whole_line())
    n = elaborate_name_and_type(parent, node.left)
    e = elaborate_expression(parent, node.right, constant)

    if e is None:
        raise ElaborationError.from_type(ElaborationErrorType.EXPECTED_VALUE, node.token.location.whole_line())

    e_type = e.type()
    if n.type is not None and n.type != e_type:
        raise JTLTypeError.from_type(JTLTypeErrorType.TYPE_MISSMATCH_DECLARATION, node.token.location.whole_line(),
                                     n.type, e)
    n.type = e_type
    parent.instructions.append(InstructionUnary(PseudoUnaryOps.STORE, Operand(n, n.declaration_location), e))
    parent.names[n.name] = n


def is_name(node: ASTNode) -> bool:
    return isinstance(node, ASTNodeValue) and isinstance(node.token, TokenName)


def elaborate_name_and_type(parent: Scope, node: ASTNode) -> Name:
    if isinstance(node, ASTNodeBinary) and node.token.op == Operator.COLON:
        if not is_name(node.left):
            raise ElaborationError.from_type(ElaborationErrorType.EXPECTED_NAME, node.token.location.whole_line())
        return Name(node.left.token.name, node.token.location.whole_line(), elaborate_type(parent, node.right))
    elif is_name(node):
        return Name(node.token.name, node.token.location.whole_line())
    else:
        raise ElaborationError.from_type(ElaborationErrorType.EXPECTED_NAME, node.token.location.whole_line())


def elaborate_type(parent: Scope, node: ASTNode) -> Type:
    modifiers: List[TypeModifiers] = []
    while True:
        if isinstance(node, ASTNodeUnary) and node.token.op == Operator.POINTER:
            modifiers.append(TypeModifiers.Pointer)
            node = node.child
        elif isinstance(node, ASTNodeValue) and isinstance(node.token, TokenBuildInType):
            return Type(modifiers, node.token.type)
        elif isinstance(node, ASTNodeValue) and isinstance(node.token, TokenName):
            name_obj = parent.lookup(node.token.name)
            return Type(modifiers + name_obj.type.modifiers, name_obj.type.type)
        else:
            raise ElaborationError.from_type(ElaborationErrorType.EXPECTED_TYPE, node.token.location.whole_line())


def promote_type(parent: Scope, a: Operand, b: Operand) -> (Operand, Operand):
    t_a = a.get_type()
    t_b = b.get_type()
    assert t_a is not None
    assert t_b is not None
    if t_a == t_b:
        return t_a, t_b


def elaborate_expression(parent: Scope, node: ASTNode, constant: bool) -> Operand:
    match node:
        case ASTNodeBinary():
            op_a = elaborate_expression(parent, node.left, constant)
            op_b = elaborate_expression(parent, node.right, constant)
            op_a, op_b = promote_type(parent, op_a, op_b)
            dest = TemporaryProvider.next(node.token.location, )
        case ASTNodeUnary():
            pass
        case ASTNodeUnaryRight():
            pass
        case ASTNodeStatement():
            raise NotImplementedError()
        case ASTNodeValue():
            match node.token:
                case TokenNumberLiteral():
                    return Operand(node.token.value, node.token.location)
                case TokenStringLiteral():
                    return Operand(node.token.content, node.token.location)
                case TokenBoolLiteral():
                    return Operand(node.token.value, node.token.location)
                case TokenName():
                    if (name_obj := parent.lookup(node.token.name)) is None:
                        raise ElaborationError.from_type(ElaborationErrorType.UNDECLARED_NAME,
                                                         node.token.location.whole_line())
                    if constant and name_obj.mut != Mutability.CONSTANT:
                        raise ElaborationError.from_type(ElaborationErrorType.NON_CONSTANT_IN_CONSTANT_EXPRESSION,
                                                         node.token.location)
                    return Operand(name_obj, node.token.location)
        case ASTNodeIf():
            raise NotImplementedError()
        case ASTNodeWhile():
            raise NotImplementedError()
        case ASTNodeProcedure():
            raise NotImplementedError()
        case ASTNodeScope():
            raise NotImplementedError()
        case ASTNodeTupleLike():
            raise NotImplementedError()
