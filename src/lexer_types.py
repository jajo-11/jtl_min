from dataclasses import dataclass
from enum import Enum, auto


@dataclass(slots=True)
class CodeLocation:
    file_name: str
    line: int
    col: int
    length: int

    def __repr__(self):
        return f"{self.file_name}:{self.line+1}:{self.col+1}"

@dataclass
class Token:
    location: CodeLocation

@dataclass(slots=True)
class NewLineToken(Token):
    pass

@dataclass(slots=True)
class LiteralToken(Token):
    value: int

@dataclass(slots=True)
class NameToken(Token):
    name: str

class Operator(Enum):
    PLUS = auto()
    MINUS = auto()
    TIMES = auto()
    DIVIDE = auto()
    MODULO = auto()
    ASSIGNMENT = auto()

OperatorBindingPower = {
    Operator.PLUS: (2.5, 2),
    Operator.MINUS: (2.5, 2),
    Operator.TIMES: (3.5, 3),
    Operator.DIVIDE: (3.5, 3),
    Operator.MODULO: (3.5, 3),
    Operator.ASSIGNMENT: (1, 1.5),
}

@dataclass(slots=True)
class OperatorToken(Token):
    op: Operator

@dataclass(slots=True)
class BracketToken(Token):
    open: bool