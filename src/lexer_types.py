from dataclasses import dataclass
from enum import Enum, auto
from typing import List


@dataclass(slots=True)
class CodeLocation:
    file_name: str
    line: int
    col: int
    length: int
    line_str: str

    def __repr__(self):
        return f"{self.file_name}:{self.line + 1}:{self.col + 1}"


@dataclass
class Token:
    location: CodeLocation


@dataclass(slots=True)
class TokenNewLine(Token):
    pass


@dataclass(slots=True)
class TokenLiteral(Token):
    value: int


@dataclass(slots=True)
class TokenName(Token):
    name: str


# Comma is not an operator it is only valid in Tuple like structures (works more like a NewLineToken)
@dataclass(slots=True)
class TokenComma(Token):
    pass


class Operator(Enum):
    PLUS = "+"
    MINUS = "-"
    TIMES = "*"
    DIVIDE = "/"
    MODULO = "%"
    ASSIGNMENT = "="
    DOT = "."
    COLON = ":"
    EQUAL = "=="
    NOTEQUAL = "!="
    LESSEQUAL = "<="
    GREATEREQUAL = ">="
    LESS = "<"
    GREATER = ">"
    AND = "and"
    OR = "or"
    NOT = "not"

    def __repr__(self):
        return self.value


@dataclass(slots=True)
class TokenOperator(Token):
    op: Operator


class BracketType(Enum):
    ROUND = "()"
    SQUARE = "[]"
    CURLY = "{}"

    def __str__(self) -> str:
        return self.__repr__()

    def __repr__(self) -> str:
        return self.value


@dataclass(slots=True)
class TokenBracket(Token):
    open: bool
    type: BracketType
