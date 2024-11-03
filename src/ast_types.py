from dataclasses import field
from typing import List, Optional

from typing import TYPE_CHECKING

from lexer_types import *
from typing_types import Type, TypeType, Name

if TYPE_CHECKING:
    from elaboration_types import Scope

@dataclass
class ASTNode:
    token: Token
    type: int = field(init=False)

    def __post_init__(self):
        self.type = 0

    def __repr__(self):
        return str(self.token)


@dataclass(slots=True)
class ASTNodeValue(ASTNode):
    token: Token

    def __repr__(self) -> str:
        match self.token:
            case TokenNumberLiteral():
                return str(self.token.value)
            case TokenName():
                return self.token.name
            case TokenStringLiteral():
                return f"\"{self.token.content}\""
            case TokenBoolLiteral():
                return str(self.token.value)
            case TokenBuildInType():
                return self.token.type.value
            case _:
                raise RuntimeError(f"Bad Value {self.token}")


@dataclass(slots=True)
class ASTNodeBinary(ASTNode):
    token: TokenOperator
    left: ASTNode
    right: ASTNode

    def __repr__(self) -> str:
        return f"({self.token.op.value} {self.left} {self.right})"


@dataclass(slots=True)
class ASTNodeUnary(ASTNode):
    token: TokenOperator
    child: ASTNode

    def __repr__(self) -> str:
        return f"(<- {self.token.op.value} {self.child})"


@dataclass(slots=True)
class ASTNodeUnaryRight(ASTNode):
    token: TokenOperator
    child: ASTNode

    def __repr__(self) -> str:
        return f"(-> {self.token.op.value} {self.child})"


@dataclass(slots=True)
class ASTNodeTupleLike(ASTNode):
    token: TokenBracket | TokenKeyword
    children: List[ASTNode]
    parent: Optional[ASTNode] = None

    def __repr__(self) -> str:
        match self.token:
            case TokenKeyword():
                return f"({self.token.keyword.value} {self.parent} [{" ".join(map(str, self.children))}])"
            case TokenBracket():
                return f"({self.token.type} {self.parent} [{" ".join(map(str, self.children))}])"


@dataclass(slots=True)
class ASTNodeScope(ASTNode):
    nodes: List[ASTNode]
    elaborated_body: Optional["Scope"] = None

    def __repr__(self) -> str:
        return "\n".join(map(lambda x: "#" + str(x), self.nodes))


@dataclass(slots=True)
class ASTNodeIf(ASTNode):
    token: TokenKeyword
    condition: ASTNode
    body: ASTNode
    else_body: Optional[ASTNode]

    def __repr__(self) -> str:
        body = "\n".join(map(lambda x: "   " + x, str(self.body).splitlines()))
        else_body = "\n".join(map(lambda x: "   " + x, str(self.else_body).splitlines()))
        return f"(if {self.condition}\n body:\n{body}\n else:\n{else_body}\n)"


@dataclass(slots=True)
class ASTNodeWhile(ASTNode):
    token: TokenKeyword
    condition: ASTNode
    body: List[ASTNode]
    elaborated_body: Optional["Scope"] = None

    def __repr__(self) -> str:
        body = "\n".join(map(lambda x: "   #" + str(x), self.body))
        return f"(while {self.condition}\n body:\n{body}\n)"


@dataclass(slots=True)
class ASTNodeProcedure(ASTNode):
    token: TokenKeyword
    arguments: List[ASTNode]
    return_type_expr: Optional[ASTNode]
    body: List[ASTNode]
    argument_names: List[Name] = field(default_factory=list)
    elaborated_body: Optional["Scope"] = None

    def __repr__(self) -> str:
        body = "\n".join(map(lambda x: "   #" + str(x), self.body))
        return f"(proc {self.arguments} {self.return_type_expr}\n body:\n{body}\n)"


@dataclass(slots=True)
class ASTNodeStatement(ASTNode):
    token: TokenKeyword
    value: Optional[ASTNode]

    def __repr__(self) -> str:
        return f"({self.token.keyword.value} {self.value})"


@dataclass(slots=True)
class ASTNodeCast(ASTNode):
    node: ASTNode

    def __repr__(self) -> str:
        return f"(cast {self.type} {self.node})"

@dataclass(slots=True)
class ASTNodeAssignment(ASTNode):
    node: ASTNodeBinary
    name: Name
    expression: ASTNode

@dataclass(slots=True)
class ASTNodeCall(ASTNode):
    procedure: Name
    arguments: List[ASTNode]


# associativity is encoded by biasing the binding power
OperatorBindingPower = {
    Operator.ASSIGNMENT:   (1.5, 1.0),
    Operator.COLON:        (2.0, 2.5),
    Operator.AND:          (2.0, 2.5),
    Operator.OR:           (2.0, 2.5),
    Operator.EQUAL:        (3.0, 3.5),
    Operator.NOTEQUAL:     (3.0, 3.5),
    Operator.LESSEQUAL:    (3.0, 3.5),
    Operator.GREATEREQUAL: (3.0, 3.5),
    Operator.GREATER:      (3.0, 3.5),
    Operator.LESS:         (3.0, 3.5),
    Operator.PLUS:         (4.0, 4.5),
    Operator.MINUS:        (4.0, 4.5),
    Operator.TIMES:        (5.0, 5.5),
    Operator.DIVIDE:       (5.0, 5.5),
    Operator.MODULO:       (5.0, 5.5),
    Operator.DOT:          (9.0, 9.5),
}

UnaryBindingPower = {
    Operator.PLUS: 6.0,
    Operator.MINUS: 6.0,
    Operator.NOT: 6.0,
    Operator.POINTER: 7.0,
    Operator.ADDRESS_OFF: 7.0,
}

CallBindingPower = 8.0
