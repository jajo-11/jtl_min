from dataclasses import dataclass
from typing import List, Optional

from lexer_types import Token, TokenLiteral, TokenName, TokenOperator, TokenBracket, Operator


@dataclass
class Type:
    pass


@dataclass
class ASTNode:
    token: Token
    type: Type


@dataclass(slots=True)
class ASTNodeValue(ASTNode):
    token: TokenLiteral | TokenName

    def __repr__(self) -> str:
        match self.token:
            case TokenLiteral():
                return str(self.token.value)
            case TokenName():
                return self.token.name


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
        return f"({self.token.op.value} {self.child})"


@dataclass(slots=True)
class ASTNodeTupleLike(ASTNode):
    token: TokenBracket
    children: List[ASTNode]
    parent: Optional[ASTNode] = None

    def __repr__(self) -> str:
        return f"({self.token.type} {self.parent} [{" ".join(map(str, self.children))}])"


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
    Operator.DOT:          (8.0, 8.5),
}

UnaryBindingPower = {
    Operator.PLUS:  6.0,
    Operator.MINUS: 6.0,
    Operator.NOT:   6.0,
}

CallBindingPower = 7.0
