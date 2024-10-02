from dataclasses import dataclass
from enum import Enum

from lexer_types import CodeLocation


def simple_error(msg: str):
    """
    Prints a simple error message

    used where not file information is available
    Args:
        msg: message to print
    """
    print(f"Error: {msg}")


@dataclass
class JTLError(Exception):
    title: str
    message: str


class LexerErrorType(Enum):
    UNIMPLEMENTED = ("Unimplemented operator", "This is a reserved but unused operator")
    INVALID = ("Invalid", "This char is not a valid (part of) operator or name")
    UNTERMINATED_STRING = ("Unterminated string", "Missing terminating \"")


@dataclass
class LexerError(JTLError):
    location: CodeLocation

    def __str__(self) -> str:
        return f"[Lexer Error] {self.title} [{self.location}]:\n{self.message}\n"

    @classmethod
    def from_type(cls, type: LexerErrorType, location: CodeLocation) -> "LexerError":
        return cls(type.value[0], type.value[1], location)


class ParserErrorType(Enum):
    UNEXPECTED_TOKEN = ("Unexpected token", "Parser did not expect this (compiler error)")
    UNOPENED_BRACKET = ("Unopened bracket", "closed here")
    UNCLOSED_BRACKET = ("Unclosed bracket", "opened here")
    EXPECTED_ATOM = ("Expected atom", "expected a value here")
    EXPECTED_OPERATOR = ("Expected operator", "expected some form of operator (compiler error)")
    UNEXPECTED_TUPLE_TERMINATOR =\
        ("Unexpected token", "Parser did not expect this at the end of tuple (compiler error)")
    UNEXPECTED_THEN = ("Unexpected token", "must follow condition in if expression")
    EXPECTED_CONDITION = ("Expected condition", "must be followed by condition")
    EXPECTED_THEN = ("Unexpected token", "if condition must be terminated by 'then'")
    EXPECTED_EXPRESSION = ("Expected expression", "must be followed by expression")
    EXPECTED_END = ("Unexpected token", "expected end of expression")
    UNEXPECTED_COMMA = ("Unexpected token", "did not expect this here")
    UNEXPECTED_ELSE = ("Unexpected token", "no matching if")
    EXPECTED_BRACKET = ("Unexpected token", "expected '(' here")
    EXPECTED_RETURN_TYPE = ("Unexpected token", "expected return type here")
    EXPECTED_BODY = ("Unexpected token", "expected procedure body here")
    EXPECTED_BODY_WHILE = ("Unexpected token", "expected loop body here")


@dataclass
class ParserError(JTLError):
    location: CodeLocation

    def __str__(self) -> str:
        return f"[Parser Error] {self.title} [{self.location}]:\n{self.message}\n"

    @classmethod
    def from_type(cls, type: ParserErrorType, location: CodeLocation) -> "ParserError":
        return cls(type.value[0], type.value[1], location)
