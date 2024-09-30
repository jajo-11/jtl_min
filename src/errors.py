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


@dataclass
class ParserError(JTLError):
    location: CodeLocation

    def __str__(self) -> str:
        return f"[Parser Error] {self.title} [{self.location}]:\n{self.message}\n"

    @classmethod
    def from_type(cls, type: ParserErrorType, location: CodeLocation) -> "ParserError":
        return cls(type.value[0], type.value[1], location)
