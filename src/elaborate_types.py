from collections import namedtuple
from dataclasses import dataclass, Field
from enum import Enum, auto
from typing import Optional, Dict, Set, List

from lexer_types import Token, BuildInType, CodeLocation


class TypeModifiers(Enum):
    Pointer = "*"


class TypeGroup(Enum):
    INT = "int"
    UINT = "uint"
    FLOAT = "float"
    BOOL = "bool"
    SYMBOL = "symbol"
    COMPOUND = "compound"


@dataclass(slots=True)
class TypeInfo:
    name: str
    size: Optional[int]
    group: TypeGroup

    def __repr__(self) -> str:
        return self.name


class ElaborationType(Enum):
    INT_LITERAL = TypeInfo("int_literal", None, TypeGroup.INT)
    FLOAT_LITERAL = TypeInfo("float_literal", None, TypeGroup.FLOAT)
    Bool_LITERAL = TypeInfo("bool_literal", None, TypeGroup.BOOL)
    INT = TypeInfo("int", 4, TypeGroup.INT)
    UINT = TypeInfo("uint", 4, TypeGroup.UINT)
    FLOAT = TypeInfo("float", 8, TypeGroup.FLOAT)
    BOOL = TypeInfo("bool", 8, TypeGroup.BOOL)
    STRING = TypeInfo("str", None, TypeGroup.COMPOUND)
    CHAR = TypeInfo("char", 1, TypeGroup.SYMBOL)
    I8 = TypeInfo("i8", 1, TypeGroup.INT)
    I16 = TypeInfo("i16", 2, TypeGroup.INT)
    I32 = TypeInfo("i32", 4, TypeGroup.INT)
    I64 = TypeInfo("i64", 8, TypeGroup.INT)
    ISIZE = TypeInfo("isize", 8, TypeGroup.INT)
    USIZE = TypeInfo("usize", 8, TypeGroup.UINT)
    U8 = TypeInfo("u8", 1, TypeGroup.UINT)
    U16 = TypeInfo("u16", 2, TypeGroup.UINT)
    U32 = TypeInfo("u32", 4, TypeGroup.UINT)
    U64 = TypeInfo("u64", 8, TypeGroup.UINT)
    F32 = TypeInfo("f32", 4, TypeGroup.FLOAT)
    F64 = TypeInfo("f64", 8, TypeGroup.FLOAT)
    TYPE = TypeInfo("type", None, TypeGroup.COMPOUND)

    def __repr__(self) -> str:
        return self.name


@dataclass(slots=True)
class Type:
    modifiers: List[TypeModifiers]
    type: ElaborationType | "Record" | None

    def __repr__(self) -> str:
        modifiers = "".join(map(lambda x: x.value, self.modifiers))
        match self.type:
            case Record():
                return modifiers + self.type.name
            case None:
                return modifiers + "<NONE>"
            case ElaborationType():
                return modifiers + self.type.name
            case _:
                raise RuntimeError("Printing bad Type")


@dataclass(slots=True)
class Record:
    name: "Name"
    # dict with field name as key and the Name object and a byte offset as value
    fields: Dict[str, ("Name", int)]


class Mutability(Enum):
    CONSTANT = auto()
    ONCE = auto()
    MUTABLE = auto()


@dataclass(slots=True)
class Name:
    name: str
    declaration_location: CodeLocation
    type: Type = Type([], None)
    mut: Mutability = Mutability.ONCE


@dataclass(slots=True)
class Temporary:
    id: int
    declaration_location: CodeLocation
    type: Type

    def __repr__(self) -> str:
        return f"$t{self.id}"


@dataclass(slots=True)
class Operand:
    value: Name | Temporary | str | int | bool | float | None
    location: CodeLocation

    def get_type(self) -> Type:
        match self.value:
            case Name():
                return self.value.type
            case Temporary():
                return self.value.type
            case str():
                raise NotImplementedError()
            case int():
                return Type([], ElaborationType.INT_LITERAL)
            case bool():
                return Type([], ElaborationType.Bool_LITERAL)
            case float():
                return Type([], ElaborationType.FLOAT_LITERAL)
            case None:
                return Type([], None)


@dataclass(slots=True)
class Instruction:
    pass


class PseudoBinaryOps(Enum):
    ADD = "+"
    SUB = "-"
    MUL = "*"
    DIV = "/"
    MOD = "%"
    LESS = "<"
    EQUAL = "=="
    NOTEQUAL = "!="
    GREATER = ">"
    LESS_EQUAL = "<="
    GREATER_EQUAL = ">="
    AND = "and"
    OR = "or"
    JUMP_IF = "jump_if"


@dataclass(slots=True)
class InstructionBinary(Instruction):
    op: PseudoBinaryOps
    src_a: Operand
    src_b: Operand
    dest: Operand


@dataclass(slots=True)
class PseudoUnaryOps(Enum):
    SUB = "-"
    NOR = "not"
    JUMP = "jump"
    CAST = "cast"
    REINTERPRET = "reinterpret"
    STORE = "store"
    LOAD = "load"


@dataclass(slots=True)
class InstructionUnary(Instruction):
    op: PseudoUnaryOps
    src: Operand
    dest: Operand


@dataclass(slots=True)
class Scope:
    parent: Optional['Scope']
    names: Dict[str, Name]
    instructions: List[Instruction]

    def lookup(self, name: str) -> Optional[Name]:
        # todo maybe give indication if found in same scope?
        if (name_obj := self.names.get(name, None)) is None:
            if self.parent is None:
                return None
            else:
                return self.parent.lookup(name)
        else:
            return name_obj
