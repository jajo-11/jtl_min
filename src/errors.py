from dataclasses import dataclass
from enum import Enum
from typing import Any, Tuple

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
    EXPECTED_ATOM = ("Expected value", "expected a value here")
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
    EXPECTED_BODY_RECORD = ("Unexpected token", "expected record body here")
    EXPECTED_BODY_WHILE = ("Unexpected token", "expected loop body here")
    EMPTY_TUPLE = ("Expected value", "() is not a valid value")


@dataclass
class ParserError(JTLError):
    location: CodeLocation

    def __str__(self) -> str:
        return f"[Parser Error] {self.title} [{self.location}]:\n{self.message}\n"

    @classmethod
    def from_type(cls, type: ParserErrorType, location: CodeLocation) -> "ParserError":
        return cls(type.value[0], type.value[1], location)


class ElaborationErrorType(Enum):
    NO_ASSIGNMENT_DECLARATION = ("Invalid Declaration",
                                 "A declaration started by 'const', 'let' or 'var' must contain an assignment")
    EXPECTED_NAME_DECLARATION = ("Invalid Declaration", "Expected a name here")
    UN_ASSIGNABLE = ("Syntax error", "Can not assign a value to this")
    EXPECTED_TYPE = ("Invalid Type", "Expected a type or type modifier here")
    EXPECTED_VALUE = ("Missing Value", "no value to assign to name")
    UNDECLARED_NAME = ("Undeclared Name", "this name has not been declared (yet)")
    UNDECLARED_PROC = ("Undeclared Name", "this procedure has not been declared (yet)")
    NOT_CALLABLE = ("Unexpected Call", "this object is not callable")
    NON_CONSTANT_IN_CONSTANT_EXPRESSION = ("Non-constant expression", "value only valid at runtime")
    UNEXPECTED_TYPE_MODIFIER = ("Unexpected Operator", "This operator is only valid in types")
    ADDRESS_OF_UNASSIGNED = ("No Address", "Can only take address of assigned variables")
    UNEXPECTED_STATEMENT = ("Unexpected Statement", "expected an expression got a statement")
    NOT_MUTABLE = ("Assignment Error", "Can not reassign a {} valued name")
    MISSING_TYPE = ("Missing Type", "Procedure arguments need type information")
    CAN_NOT_RETURN_FROM_HERE = ("Unexpected Statement", "Can only return from procedures")
    UNREACHABLE = ("Unreachable Expression", "Procedure returns before this can be executed")
    MISSING_RETURN = ("Missing Return", "Procedure {} does not return a value")
    WRONG_NUMBER_OF_ARGUMENTS = ("Procedure signature missmatch", "Expected {} arguments, expected {}")
    WRONG_NUMBER_OF_ARGUMENTS_CAST = ("Procedure signature missmatch", "cast expects exactly 2 arguments, got {}")
    WRONG_NUMBER_OF_ARGUMENTS_TRANSMUTE = ("Procedure signature missmatch",
                                           "transmute expects exactly 2 arguments, got {}")
    WRONG_ARGUMENT_TYPE = ("Procedure signature missmatch", "Expected argument {} to have type {} got {}")
    GLOBAL_CALL = ("Unexpected call", "Can not call functions in the global scope")
    CASTING_ERROR = ("Casting error", "Can not cast objecto of type {} to type {}")
    UNEXPECTED_POINTER_MODIFIER = ("Unexpected Operator",
                                   "When specifying a type const must always follow a pointer operator")
    WRITE_TO_CONST_POINTER = ("Assignment Error", "Can not write to const pointer")
    NOT_A_DECLARATION = ("Syntax error", "All statements in a record must be a declaration")
    CYCLIC_RECORD = ("Cyclic Record", "This record contains itself")
    EXPECTED_FIELD_NAME = ("Expected Name", "Expected a field name here")
    EXPECTED_RECORD = ("Expected Record", "Expected a record here")
    FIELD_DOES_NOT_EXIST = ("Undeclared Field", "This field does not exist")

@dataclass
class ElaborationError(JTLError):
    location: CodeLocation
    args: Tuple[Any]

    def __str__(self) -> str:
        return f"[Elaboration Error] {self.title} [{self.location}]:\n{self.message.format(*self.args)}\n"

    @classmethod
    def from_type(cls, type: ElaborationErrorType, location: CodeLocation, *args) -> "ElaborationError":
        return cls(type.value[0], type.value[1], location, args)


class JTLTypeErrorType(Enum):
    TYPE_MISSMATCH_DECLARATION = ("Type Error", "Declared type {} does not match expression type {}")
    NO_VALUE_TYPE = ("Missing Value", "Expression returns no value for {} to operate on")
    POINTER_ARITHMETIC = ("Invalid Operation", "arithmetic operations on pointers is forbidden")
    NOT_NUMERIC = ("Not a numeric type", "cannot do arithmetic on non-numeric type")
    NOT_NUMERIC_COMPARE = ("Not a numeric type", "cannot compare non-numeric type")
    INCOMPATIBLE_TYPE_GROUP = ("Incompatible type", "{} is incompatible with type {}")
    UNSAFE_AUTOMATIC_CAST = ("Incompatible type", "{} and {} can not be promoted to an int that can hold all values")
    NOT_COMPARABLE = ("Incompatible type", "Can not compare {} and {}")
    INCOMPATIBLE_POINTER = ("Incompatible type", "to compare pointers they must point to a value of the same type."
                                                 " But they point to {} and {}")
    EXPECTED_BOOLEAN_2x = ("Incompatible type", "Can not use '{}' on non booleans ({}, {})")
    EXPECTED_BOOLEAN = ("Incompatible type", "Can not use '{}' on non booleans of type {}")
    UNSAFE_UINT_TO_INT = ("Incompatible type", "Can not convert {} to an int as a 64bit int can not hold all values")
    DEREFERENCE_VALUE = ("Incompatible type", "Can not dereference value of type {} as it is not a pointer")
    EXPECTED_BOOLEAN_CONDITION = ("Unexpected type", "Condition must return/be a boolean value not {}")
    TYPE_MISSMATCH_RETURN = ("Type Error", "Returned value of type {}, expected a value of {}")
    BIT_OPERATOR_ON_NON_INTEGER = ("Type Error", "Cannot use {} on non-integer value,"
                                                 " transmute floats if necessary")
    BIT_OPERATOR_SIZE_MISSMATCH = ("Type Error", "Both operands must have the same size in bits")
    BIT_OPERATOR_SIGNED_UNSIGNED = ("Type Error", "Both operands must be either signed or unsigned")
    SHIFT_ON_NON_INTEGER = ("Type Error", "Cannot use {} on non-integer value,"
                                                 " transmute floats if necessary")
    TRANSMUTE_UNRESOLVED_SIZE = ("Missing Type", "To transmute the size of the source type must be known."
                                       " (It is currently not enough that the type can be inferred later)")
    TRANSMUTE_SIZE_MISSMATCH = ("Type Error", "Sizes in transmute must match have {} and {}")
    TYPE_MISSMATCH_ASSIGNMENT = ("Type Error", "Can not assign expression of type {} to value of type {}")


@dataclass
class JTLTypeError(JTLError):
    location: CodeLocation
    fmt_args: Tuple[Any]

    def __str__(self) -> str:
        return f"[Type Error] {self.title} [{self.location}]:\n{self.message.format(*self.fmt_args)}\n"

    @classmethod
    def from_type(cls, type: JTLTypeErrorType, location: CodeLocation, *args) -> "JTLTypeError":
        return cls(type.value[0], type.value[1], location, args)