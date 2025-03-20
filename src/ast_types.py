import itertools
from dataclasses import field
from typing import List, Optional, Union, TYPE_CHECKING

from lexer_types import *

if TYPE_CHECKING:
    from elaboration_types import Scope, Name, Record

@dataclass
class ASTNode:
    token: Token
    type: int = field(init=False)

    def __post_init__(self):
        self.type = 0

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self):
        return f"ASTNode<{self.token}>"


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
                return f"\"{self.token.content}\""
            case TokenBoolLiteral():
                return f"{self.token.value}"
            case TokenBuildInType():
                return f"{self.token.type.value}"
            case _:
                raise RuntimeError(f"Bad Value {self.token}")

@dataclass(slots=True)
class ASTNodeBinary(ASTNode):
    token: TokenOperator
    left: ASTNode
    right: ASTNode

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self) -> str:
        return f"({self.token.op.value} {self.left} {self.right})"


@dataclass(slots=True)
class ASTNodeUnary(ASTNode):
    token: TokenOperator
    child: ASTNode

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self) -> str:
        return f"(<- {self.token.op.value} {self.child})"


@dataclass(slots=True)
class ASTNodeUnaryRight(ASTNode):
    token: TokenOperator
    child: ASTNode

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self) -> str:
        return f"(-> {self.token.op.value} {self.child})"


@dataclass(slots=True)
class ASTNodeTupleLike(ASTNode):
    token: TokenBracket | TokenKeyword
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


@dataclass(slots=True)
class ASTNodeScope(ASTNode):
    body: List[ASTNode]
    elaborated_body: Optional["Scope"] = None

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self) -> str:
        lines = itertools.chain.from_iterable(map(lambda x: str(x).splitlines(), self.body))
        return "Scope:\n  " + "\n  ".join(lines)


@dataclass(slots=True)
class ASTNodeRecord(ASTNode):
    body: List[ASTNode]

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self) -> str:
        lines = itertools.chain.from_iterable(map(lambda x: str(x).splitlines(), self.body))
        return "(Record\n  " + "\n  ".join(lines) + ")"


@dataclass(slots=True)
class ASTNodeIf(ASTNode):
    token: TokenKeyword
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


@dataclass(slots=True)
class ASTNodeWhile(ASTNode):
    token: TokenKeyword
    condition: ASTNode
    body: List[ASTNode]
    elaborated_body: Optional["Scope"] = None

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self) -> str:
        body = itertools.chain.from_iterable(map(lambda x: str(x).splitlines(), self.body))
        return f"(while {self.condition}\n body:\n   {'\n   '.join(body)}\n)"


@dataclass(slots=True)
class ASTNodeProcedure(ASTNode):
    token: TokenKeyword
    arguments: List[ASTNode]
    return_type_expr: Optional[ASTNode]
    body: List[ASTNode]
    argument_names: List["Name"] = field(default_factory=list)
    elaborated_body: Optional["Scope"] = None

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self) -> str:
        body = itertools.chain.from_iterable(map(lambda x: str(x).splitlines(), self.body))
        return f"(proc [{', '.join(map(str, self.arguments))}] {self.return_type_expr}\n body:\n   {'\n  '.join(body)}\n)"


@dataclass(slots=True)
class ASTNodeStatement(ASTNode):
    token: TokenKeyword
    child: Optional[ASTNode]

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self) -> str:
        return f"({self.token.keyword.value} {self.child})"


@dataclass(slots=True)
class ASTNodeCast(ASTNode):
    child: ASTNode

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self) -> str:
        return f"(cast {self.type} {self.child})"


@dataclass(slots=True)
class ASTNodeTransmute(ASTNode):
    child: ASTNode

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self) -> str:
        return f"(transmute {self.type} {self.child})"


@dataclass(slots=True)
class ASTNodeAssignment(ASTNode):
    node: ASTNodeBinary
    target: Union[ASTNode, "Name"]
    expression: ASTNode

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self):
        return f"(ASSIGN {self.target} = {self.expression})"


@dataclass(slots=True)
class ASTNodeCall(ASTNode):
    procedure: "Name"
    arguments: List[ASTNode]

    def __repr__(self):
        return f"{type(self).__name__}<{str(self)}>"

    def __str__(self):
        return f"(call {self.procedure.name} ({", ".join(map(str, self.arguments))})"


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
    Operator.CONSTANT_POINTER: 9.0,
    Operator.ADDRESS_OFF: 9.0,
}

CallBindingPower = 10.0
