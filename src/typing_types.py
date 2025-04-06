from copy import copy
from dataclasses import dataclass, field
from enum import Enum
from typing import Optional, Dict, List, Iterable, Any, TYPE_CHECKING, Protocol

from lexer_types import CodeLocation, BuildInType

if TYPE_CHECKING:
    from ast_types import ASTNode, ASTNodeProcedure, ASTNodeRecord
    from elaboration_types import Record


class TypeGroup(Enum):
    UNDEFINED = "undefined"
    INT = "int"
    UINT = "uint"
    FLOAT = "float"
    BOOL = "bool"
    SYMBOL = "symbol"
    POINTER = "pointer"
    RECORD = "record"
    PROCEDURE = "procedure"
    TYPE = "type"
    NO_VALUE = "no_value"
    RETURNS = "returns" # type of expression with a return


@dataclass(slots=True)
class TypeInfo:
    name: str
    size: Optional[int]
    group: TypeGroup

    def __str__(self) -> str:
        return self.name

    def __eq__(self, other: Any) -> bool:
        if not isinstance(other, TypeInfo):
            return False
        return self.size == other.size and self.group == other.group



PLATFORM_POINTER_SIZE = 8
PLATFORM_INT_SIZE = 4
PLATFORM_BOOL_SIZE = 4


class TypeType(Enum):
    UNDEFINED = TypeInfo("undefined", None, TypeGroup.UNDEFINED)
    INT_LITERAL = TypeInfo("int_literal", None, TypeGroup.INT)
    UINT_LITERAL = TypeInfo("uint_literal", None, TypeGroup.UINT)
    FLOAT_LITERAL = TypeInfo("float_literal", None, TypeGroup.FLOAT)
    INT = TypeInfo("int", PLATFORM_INT_SIZE, TypeGroup.INT)
    UINT = TypeInfo("uint", PLATFORM_INT_SIZE, TypeGroup.UINT)
    BOOL = TypeInfo("bool", PLATFORM_BOOL_SIZE, TypeGroup.BOOL)
    STRING = TypeInfo("str", None, TypeGroup.RECORD)
    CHAR = TypeInfo("char", 1, TypeGroup.SYMBOL)
    I8 = TypeInfo("i8", 1, TypeGroup.INT)
    I16 = TypeInfo("i16", 2, TypeGroup.INT)
    I32 = TypeInfo("i32", 4, TypeGroup.INT)
    I64 = TypeInfo("i64", 8, TypeGroup.INT)
    ISIZE = TypeInfo("isize", PLATFORM_POINTER_SIZE, TypeGroup.INT)
    USIZE = TypeInfo("usize", PLATFORM_POINTER_SIZE, TypeGroup.UINT)
    U8 = TypeInfo("u8", 1, TypeGroup.UINT)
    U16 = TypeInfo("u16", 2, TypeGroup.UINT)
    U32 = TypeInfo("u32", 4, TypeGroup.UINT)
    U64 = TypeInfo("u64", 8, TypeGroup.UINT)
    F32 = TypeInfo("f32", 4, TypeGroup.FLOAT)
    F64 = TypeInfo("f64", 8, TypeGroup.FLOAT)
    TYPE = TypeInfo("type", None, TypeGroup.TYPE)
    NO_VALUE = TypeInfo("no_value", None, TypeGroup.NO_VALUE)
    RECORD = TypeInfo("record", None, TypeGroup.RECORD)
    POINTER = TypeInfo("pointer", PLATFORM_POINTER_SIZE, TypeGroup.POINTER)
    PROCEDURE = TypeInfo("procedure", None, TypeGroup.PROCEDURE)
    RETURNS = TypeInfo("returns", None, TypeGroup.RETURNS)

    def __str__(self) -> str:
        return self.name


map_build_in_type_to_type_type: Dict[BuildInType, TypeType] = {
    BuildInType.INT: TypeType.INT,
    BuildInType.UINT: TypeType.UINT,
    BuildInType.BOOL: TypeType.BOOL,
    BuildInType.STRING: TypeType.STRING,
    BuildInType.CHAR: TypeType.CHAR,
    BuildInType.I8: TypeType.I8,
    BuildInType.I16: TypeType.I16,
    BuildInType.I32: TypeType.I32,
    BuildInType.I64: TypeType.I64,
    BuildInType.ISIZE: TypeType.ISIZE,
    BuildInType.USIZE: TypeType.USIZE,
    BuildInType.U8: TypeType.U8,
    BuildInType.U16: TypeType.U16,
    BuildInType.U32: TypeType.U32,
    BuildInType.U64: TypeType.U64,
    BuildInType.F32: TypeType.F32,
    BuildInType.F64: TypeType.F64,
    BuildInType.TYPE: TypeType.TYPE,
}


class Mutability(Enum):
    CONSTANT = "const"
    ONCE = "let"
    MUTABLE = "var"


class Type:
    __slots__ = ("info",)

    def __init__(self, info: TypeType = TypeType.UNDEFINED) -> None:
        self.info = copy(info.value)

    def __str__(self) -> str:
        return self.info.name

    def __eq__(self, other: Any) -> bool:
        if not isinstance(other, Type):
            return False
        return self.info == other.info


class TypeTable:
    def __init__(self) -> None:
        self.table: List[Type | int] = [Type(TypeType.UNDEFINED)]

    def new(self, type: Type) -> int:
        self.table.append(type)
        return len(self.table) - 1

    def get(self, index: int) -> Type:
        assert isinstance(index, int)
        new_index: Type | int = index
        while isinstance(new_index, int):
            new_index = self.table[new_index]
        return new_index

    def overwrite(self, old: int, new: int | Type):
        if old == 0:
            raise RuntimeError("Compiler Error tried to alter Sentinel Type")
        current = old
        while isinstance(next := self.table[current], int):
            current = next
        self.table[old] = new # this is not strictly necessary but reduces time to resolve type
        self.table[current] = new


class TypeRecordType(Type):
    __slots__ = ("info", "record")

    def __init__(self, record: "Record"):
        super().__init__(TypeType.TYPE)
        self.record: "Record" = record

    def __str__(self) -> str:
        return f"Record[{self.record.name}]"

    def __eq__(self, other: Any) -> bool:
        if not isinstance(other, TypeRecordType):
            return False
        return self.record == other.record

    def to_instance_type(self) -> "TypeRecordInstance":
        rv = TypeRecordInstance(self.record)
        rv.info = copy(self.info)
        rv.info.group = TypeGroup.RECORD
        return rv


class TypeRecordInstance(TypeRecordType):
    """Do not construct manually use TypeRecordType.to_instance_type()"""

    def __str__(self) -> str:
        return f"RecordInstance[{self.record.name}]"


class TypePointer(Type):
    __slots__ = ("info", "target_type", "target_mutable", "type_table")

    def __init__(self, target_type: int, target_mutable: bool, type_table: TypeTable) -> None:
        super().__init__(TypeType.POINTER)
        self.type_table = type_table
        self.target_type = target_type
        self.target_mutable = target_mutable  # a boolean if mutable else false (once and constant)

    def __str__(self) -> str:
        return f"^({self.type_table.get(self.target_type)})"

    def __eq__(self, other: Any) -> bool:
        if not isinstance(other, TypePointer):
            return False
        return self.type_table.get(self.target_type) == self.type_table.get(other.target_type)


class TypeProcedure(Type):
    __slots__ = ("info", "arguments", "return_type", "varargs", "type_table")

    def __init__(self, arguments: List[int], return_type: int, varargs: bool, type_table: TypeTable) -> None:
        super().__init__(TypeType.PROCEDURE)
        self.arguments = arguments
        self.return_type = return_type
        self.varargs = varargs
        self.type_table = type_table

    def __str__(self) -> str:
        return (f"procedure ({', '.join(map(str, map(self.type_table.get, self.arguments)))})"
                f" -> {self.type_table.get(self.return_type)}")

    def __eq__(self, other: Any) -> bool:
        if not isinstance(other, TypeProcedure):
            return False
        if len(self.arguments) != len(other.arguments):
            return False
        return (all(map(lambda x: x[0] == x[1], zip(self.arguments, other.arguments)))
                and self.return_type == other.return_type)