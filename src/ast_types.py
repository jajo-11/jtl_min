from dataclasses import dataclass

from lexer_types import Token

@dataclass
class Type:
    pass

@dataclass
class ASTNode:
    token: Token
    type: Type

@dataclass(slots=True)
class ASTNodeValue:
    pass

@dataclass(slots=True)
class BinaryASTNode(ASTNode):
    left: ASTNode
    right: ASTNode

@dataclass(slots=True)
class UnaryASTNode(ASTNode):
    child: ASTNode