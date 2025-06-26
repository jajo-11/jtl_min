import itertools
from dataclasses import field
from typing import List, Optional, Union, TYPE_CHECKING

from lexer_types import *

if TYPE_CHECKING:
    from elaboration_types import Scope, Name, Record

@dataclass
class ASTNode:
    token: Token
    type: int = field(default=0, init=False)
    # True if you could assign to this or take a mutable pointer
    mutable: bool = field(default=False, init=False)

    def __post_init__(self):
        self.type = 0

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self):
        return f"ASTNode<{self.token}>"

    def get_location(self) -> CodeLocation:
        return self.token.location


@dataclass(slots=True)
class ASTNodeValue(ASTNode):
    token: Token

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self) -> str:
        match self.token:
            case TokenNumberLiteral():
                return f"{self.token.value}"
            case TokenName():
                return f"{self.token.name}"
            case TokenStringLiteral():
                if self.token.zero_terminated:
                    return f"c\"{self.token.content}\""
                else:
                    return f"\"{self.token.content}\""
            case TokenBoolLiteral():
                return f"{self.token.value}"
            case TokenBuildInType():
                return f"{self.token.type.value}"
            case _:
                raise RuntimeError(f"Bad Value {self.token}")

@dataclass(slots=True)
class ASTNodeVarArgs(ASTNode):
    token: Token

    def __str__(self) -> str:
        return "varargs"

@dataclass(slots=True)
class ASTNodeBinary(ASTNode):
    token: TokenOperator
    left: ASTNode
    right: ASTNode

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self) -> str:
        return f"({self.token.op.value} {self.left} {self.right})"

    def get_location(self) -> CodeLocation:
        return self.left.get_location().span(self.right.get_location())


@dataclass(slots=True)
class ASTNodeUnary(ASTNode):
    token: TokenOperator
    child: ASTNode

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self) -> str:
        return f"(<- {self.token.op.value} {self.child})"

    def get_location(self) -> CodeLocation:
        return self.token.location.span(self.child.get_location())


@dataclass(slots=True)
class ASTNodeUnaryRight(ASTNode):
    token: TokenOperator
    child: ASTNode

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self) -> str:
        return f"(-> {self.token.op.value} {self.child})"

    def get_location(self) -> CodeLocation:
        return self.token.location.span(self.child.get_location())


@dataclass(slots=True)
class ASTNodeTupleLike(ASTNode):
    token: TokenBracket | TokenKeyword
    location: CodeLocation
    children: List[ASTNode]
    parent: Optional[ASTNode] = None

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self) -> str:
        match self.token:
            case TokenKeyword():
                return f"({self.token.keyword.value} {self.parent} [{" ".join(map(str, self.children))}])"
            case TokenBracket():
                return f"({self.token.type} {self.parent} [{" ".join(map(str, self.children))}])"

    def get_location(self) -> CodeLocation:
        return self.location

@dataclass(slots=True)
class ASTNodeArrayAccess(ASTNode):
    token: Token
    location: CodeLocation
    index: ASTNode
    array: ASTNode

    def __str__(self) -> str:
        return f"([access] {self.index} {self.array})"

    def get_location(self) -> CodeLocation:
        return self.location


@dataclass(slots=True)
class ASTNodeArrayType(ASTNode):
    """Might also include an array literal"""
    token: TokenBracket
    location: CodeLocation
    array_children: List[ASTNode]
    type_expression: ASTNode

    def __str__(self) -> str:
        return f"([ArrayType] [{", ".join(map(str, self.array_children))}] {str(self.type_expression)})"

    def get_location(self) -> CodeLocation:
        return self.location


@dataclass(slots=True)
class ASTNodeScope(ASTNode):
    body: List[ASTNode]
    location: CodeLocation
    elaborated_body: Optional["Scope"] = None

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self) -> str:
        lines = itertools.chain.from_iterable(map(lambda x: str(x).splitlines(), self.body))
        return "Scope:\n  " + "\n  ".join(lines)

    def get_location(self) -> CodeLocation:
        return self.location

@dataclass(slots=True)
class ASTNodeRecord(ASTNode):
    body: List[ASTNode]
    location: CodeLocation

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self) -> str:
        lines = itertools.chain.from_iterable(map(lambda x: str(x).splitlines(), self.body))
        return "(Record\n  " + "\n  ".join(lines) + ")"

    def get_location(self) -> CodeLocation:
        return self.location


@dataclass(slots=True)
class ASTNodeIf(ASTNode):
    token: TokenKeyword
    location: CodeLocation
    condition: ASTNode
    body: ASTNode
    else_location: Optional[CodeLocation]
    else_body: Optional[ASTNode]

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self) -> str:
        body = "\n   ".join(str(self.body).splitlines())
        else_body = "\n   ".join(str(self.else_body).splitlines())
        return f"(if {self.condition}\n body:\n   {body}\n else:\n   {else_body}\n)"

    def get_location(self) -> CodeLocation:
        return self.location


@dataclass(slots=True)
class ASTNodeWhile(ASTNode):
    token: TokenKeyword
    location: CodeLocation
    condition: ASTNode
    body: List[ASTNode]
    elaborated_body: Optional["Scope"] = None

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self) -> str:
        body = itertools.chain.from_iterable(map(lambda x: str(x).splitlines(), self.body))
        return f"(while {self.condition}\n body:\n   {'\n   '.join(body)}\n)"

    def get_location(self) -> CodeLocation:
        return self.location


@dataclass(slots=True)
class ASTNodeProcedure(ASTNode):
    token: TokenKeyword
    location: CodeLocation # just the header
    arguments: List[ASTNode]
    return_type_expr: Optional[ASTNode]
    body: List[ASTNode]
    var_args: bool = False
    argument_names: List["Name"] = field(default_factory=list)
    elaborated_body: Optional["Scope"] = None

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self) -> str:
        body = itertools.chain.from_iterable(map(lambda x: str(x).splitlines(), self.body))
        return f"(proc [{', '.join(map(str, self.arguments))}] {self.return_type_expr}\n body:\n   {'\n  '.join(body)}\n)"

    def get_location(self) -> CodeLocation:
        return self.location



@dataclass(slots=True)
class ASTNodeProcedureStub(ASTNode):
    token: TokenKeyword
    location: CodeLocation
    arguments: List[ASTNode]
    return_type_expr: Optional[ASTNode]
    argument_names: List["Name"] = field(default_factory=list)
    var_args: bool = False

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self) -> str:
        return f"(proc_stub [{', '.join(map(str, self.arguments))}] {self.return_type_expr})\n"

    def get_location(self) -> CodeLocation:
        return self.location


@dataclass(slots=True)
class ASTNodeStatement(ASTNode):
    token: TokenKeyword
    child: Optional[ASTNode]

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self) -> str:
        return f"({self.token.keyword.value} {self.child})"

    def get_location(self) -> CodeLocation:
        if self.child is None:
            return self.token.location
        return self.token.location.span(self.child.get_location())


@dataclass(slots=True)
class ASTNodeCast(ASTNode):
    child: ASTNode

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self) -> str:
        return f"(cast {self.type} {self.child})"

    def get_location(self) -> CodeLocation:
        return self.token.location.span(self.child.get_location())


@dataclass(slots=True)
class ASTNodeTransmute(ASTNode):
    child: ASTNode

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self) -> str:
        return f"(transmute {self.type} {self.child})"

    def get_location(self) -> CodeLocation:
        return self.token.location.span(self.child.get_location())


@dataclass(slots=True)
class ASTNodeAssignment(ASTNode):
    node: ASTNodeBinary
    target: Union[ASTNode, "Name"]
    expression: ASTNode

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self):
        return f"(ASSIGN {self.target} = {self.expression})"

    def get_location(self) -> CodeLocation:
        return self.node.get_location()


@dataclass(slots=True)
class ASTNodeCall(ASTNode):
    procedure: "Name"
    arguments: List[ASTNode]

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self):
        return f"(call {self.procedure.name} ({", ".join(map(str, self.arguments))})"

    def get_location(self) -> CodeLocation:
        if len(self.arguments) == 0:
            return self.token.location
        return self.token.location.span(self.arguments[-1].get_location())


# associativity is encoded by biasing the binding power
OperatorBindingPower = {
    Operator.ASSIGNMENT:   (1.5, 1.0),
    Operator.COLON:        (2.0, 2.5),
    Operator.AND:          (2.0, 2.5),
    Operator.OR:           (2.0, 2.5),
    Operator.BITWISE_XOR:  (3.0, 3.5),
    Operator.BITWISE_AND:  (3.0, 3.5),
    Operator.BITWISE_OR:   (3.0, 3.5),
    Operator.EQUAL:        (4.0, 4.5),
    Operator.NOTEQUAL:     (4.0, 4.5),
    Operator.LESSEQUAL:    (4.0, 4.5),
    Operator.GREATEREQUAL: (4.0, 4.5),
    Operator.GREATER:      (4.0, 4.5),
    Operator.LESS:         (4.0, 4.5),
    Operator.SHIFT_RIGHT:  (5.0, 5.5),
    Operator.SHIFT_LEFT:   (5.0, 5.5),
    Operator.PLUS:         (6.0, 6.5),
    Operator.MINUS:        (6.0, 6.5),
    Operator.TIMES:        (7.0, 7.5),
    Operator.DIVIDE:       (7.0, 7.5),
    Operator.MODULO:       (7.0, 7.5),
    Operator.DOT:          (11.0, 11.5),
}

UnaryBindingPower = {
    Operator.PLUS: 8.0,
    Operator.MINUS: 8.0,
    Operator.NOT: 8.0,
    Operator.BITWISE_NOT: 8.0,
    Operator.POINTER: 9.0,
    Operator.MUTABLE_POINTER: 9.0,
    Operator.ADDRESS_OFF: 9.0,
}

CallBindingPower = 10.0
