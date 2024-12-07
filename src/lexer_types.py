from dataclasses import dataclass
from enum import Enum


@dataclass(slots=True)
class CodeLocation:
    file_name: str
    line: int
    col: int
    length: int
    line_str: str

    def whole_line(self) -> "CodeLocation":
        # FIXME: assume all uses as a bug and implement proper CodeLocationRanges
        # This should include multi-line locations and a recursive function for all
        # ast nodes to determine the full extend of a node
        return CodeLocation(self.file_name, self.line, 0, len(self.line_str), self.line_str)

    def __str__(self):
        return f"{self.file_name}:{self.line + 1}:{self.col + 1}"


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


class Keyword(Enum):
    VARIABLE = "var"
    LET = "let"
    CONSTANT = "const"
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
    POINTER = "^"
    BITWISE_OR = "|"
    BITWISE_AND = "&"
    BITWISE_XOR = "xor"
    SHIFT_LEFT = "<<"
    SHIFT_RIGHT = ">>"

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
