from dataclasses import dataclass
from enum import Enum

@dataclass(slots=True)
class CodeLocation:
    file_name: str
    line_start: int
    line_stop: int
    col_start: int
    col_stop: int

    def span(self, other: "CodeLocation") -> "CodeLocation":
        assert self.file_name == other.file_name
        start, stop = self, other
        if start.line_start > stop.line_start:
            start, stop = stop, start
        elif start.line_start == stop.line_start and start.col_start > stop.col_start:
            start, stop = stop, start
        col_stop = stop.col_stop
        if start.line_stop > stop.line_stop:
            return start
        elif start.line_stop == stop.line_stop and start.col_stop > stop.col_stop:
            return start
        return CodeLocation(self.file_name,
                            start.line_start,
                            stop.line_stop,
                            start.col_start,
                            stop.col_stop)

    def __str__(self):
        return f"{self.file_name}:{self.line_start + 1}:{self.col_start + 1}"


@dataclass
class Token:
    location: CodeLocation


@dataclass(slots=True)
class TokenNewLine(Token):
    pass


@dataclass(slots=True)
class TokenNumberLiteral(Token):
    value: int | float


@dataclass(slots=True)
class TokenBoolLiteral(Token):
    value: bool


@dataclass(slots=True)
class TokenName(Token):
    name: str


@dataclass(slots=True)
class TokenStringLiteral(Token):
    content: str
    zero_terminated: bool = False


class Keyword(Enum):
    VARIABLE = "var"
    CONSTANT = "const"
    LET = "let"
    IF = "if"
    ELSE = "else"
    THEN = "then"
    PROCEDURE = "proc"
    RETURN = "return"
    RECORD = "record"
    FOR = "for"
    WHILE = "while"
    INOUT = "inout"
    OWNED = "owned"
    REFERENCE = "ref"
    IN = "in"
    CAST = "cast"
    TRANSMUTE = "transmute"
    DISTINCT = "distinct"
    DEFER = "defer"
    EXTERNAL = "external"
    VARARGS = "varargs"

    def __str__(self) -> str:
        return self.value


@dataclass(slots=True)
class TokenKeyword(Token):
    keyword: Keyword


class BuildInType(Enum):
    INT = "int"
    UINT = "uint"
    BOOL = "bool"
    STRING = "str"
    CHAR = "char"
    I8 = "i8"
    I16 = "i16"
    I32 = "i32"
    I64 = "i64"
    ISIZE = "isize"
    USIZE = "usize"
    U8 = "u8"
    U16 = "u16"
    U32 = "u32"
    U64 = "u64"
    F32 = "f32"
    F64 = "f64"
    TYPE = "type"

    def __str__(self) -> str:
        return self.value


@dataclass(slots=True)
class TokenBuildInType(Token):
    type: BuildInType


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
    BITWISE_NOT = "~"
    ADDRESS_OFF = "@"
    ADDRESS_OFF_MUTABLE = "@var"
    POINTER = "^"
    BITWISE_OR = "|"
    BITWISE_AND = "&"
    BITWISE_XOR = "xor"
    SHIFT_LEFT = "<<"
    SHIFT_RIGHT = ">>"
    ARROW = "->"
    MUTABLE_POINTER = "^var"

    def __str__(self) -> str:
        return self.value


@dataclass(slots=True)
class TokenOperator(Token):
    op: Operator


class BracketType(Enum):
    ROUND = "()"
    SQUARE = "[]"
    CURLY = "{}"

    def __str__(self) -> str:
        return self.value


@dataclass(slots=True)
class TokenBracket(Token):
    open: bool
    type: BracketType
