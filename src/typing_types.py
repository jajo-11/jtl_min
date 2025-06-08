from dataclasses import dataclass, field
from enum import Enum
from typing import Optional, Dict, List, Any, TYPE_CHECKING, Tuple

from lexer_types import BuildInType

if TYPE_CHECKING:
    from elaboration_types import Record


PLATFORM_POINTER_SIZE = 8
PLATFORM_INT_SIZE = 4
PLATFORM_BOOL_SIZE = 4


@dataclass(slots=True)
class Type:
    size: Optional[int]

    def __str__(self) -> str:
        return "<Bare Type>"

    def __eq__(self, other: Any) -> bool:
        return False

@dataclass(slots=True)
class NamedType(Type):
    size: Optional[int]
    name: str

    def __str__(self) -> str:
        return self.name

    def __eq__(self, other: Any) -> bool:
        if not isinstance(other, type(self)):
            return False
        return self.size == other.size


@dataclass(slots=True)
class TypeSentinel(NamedType):
    size: Optional[int] = None
    name: str = "sentinel"

    def __eq__(self, other: Any) -> bool:
        return False


@dataclass(slots=True)
class TypeNoValue(NamedType):
    size: Optional[int] = None
    name: str = "no_value"

    def __eq__(self, other: Any) -> bool:
        return False

@dataclass(slots=True)
class TypeReturns(NamedType):
    size: Optional[int] = None
    name: str = "returns"

    def __eq__(self, other: Any) -> bool:
        return False


@dataclass(slots=True)
class TypeUndefined(NamedType):
    size: Optional[int] = None
    name: str = "undefined"

    def __eq__(self, other: Any) -> bool:
        return False


class TypeInt(NamedType):
    ...


class TypeUInt(NamedType):
    ...


class TypeBool(NamedType):
    ...


class TypeFloat(NamedType):
    ...


@dataclass(slots=True)
class TypeFixedSizeArray(Type):
    size: Optional[int]
    n_elements: int
    base_type: int
    type_table: "TypeTable"

    def __str__(self) -> str:
        return f"[{self.n_elements}]{self.type_table.get(self.base_type)}"

    def __eq__(self, other: Any) -> bool:
        if not isinstance(other, TypeFixedSizeArray):
            return False
        return self.size == other.size and self.type_table.get(self.base_type) == self.type_table.get(other.base_type)


@dataclass(slots=True)
class TypeRecord(Type):
    record: "Record"

    def __str__(self) -> str:
        return f"Record[{self.record.name}]"

    def __eq__(self, other: Any) -> bool:
        if not isinstance(other, TypeRecord):
            return False
        return self.record == other.record


class TypeType(Type):
    __slots__ = ("size", "instance_type", "type_table")

    def __init__(self, instance_type: int, type_table: "TypeTable"):
        super().__init__(None)
        self.instance_type = instance_type
        self.type_table: "TypeTable"

    def __str__(self) -> str:
        return f"Type[{self.type_table.get(self.instance_type)}]"

    def __eq__(self, other: Any):
        if not isinstance(other, TypeType):
            return False
        return self.type_table.get(self.instance_type) == self.type_table.get(other.instance_type)


class TypePointer(Type):
    __slots__ = ("size", "target_type", "target_mutable", "type_table")

    def __init__(self, target_type: int, target_mutable: bool, type_table: "TypeTable"):
        super().__init__(PLATFORM_POINTER_SIZE)
        self.target_type = target_type
        self.target_mutable = target_mutable
        self.type_table = type_table

    def __str__(self) -> str:
        return f"^{"var" if self.target_mutable else ""}({self.type_table.get(self.target_type)})"

    def __eq__(self, other: Any) -> bool:
        if not isinstance(other, TypePointer):
            return False
        return (self.target_mutable == other.target_mutable and
                self.type_table.get(self.target_type) == self.type_table.get(other.target_type))


class TypeProcedure(Type):
    __slots__ = ("size", "arguments", "return_type", "varargs", "type_table")

    def __init__(self, arguments: List[int], return_type: int, varargs: bool, type_table: "TypeTable"):
        super().__init__(PLATFORM_POINTER_SIZE)
        self.arguments = arguments
        self.return_type = return_type
        self.varargs = varargs
        self.type_table = type_table

    def __str__(self) -> str:
        return (f"proc ({', '.join(map(str, map(self.type_table.get, self.arguments)))}"
                f"{", varargs" if self.varargs else ""})"
                f" -> {self.type_table.get(self.return_type)}")

    def __eq__(self, other: Any) -> bool:
        if not isinstance(other, TypeProcedure):
            return False
        if len(self.arguments) != len(other.arguments):
            return False
        for a, b in zip(self.arguments, other.arguments):
            if self.type_table.get(a) != self.type_table.get(b):
                return False
        return (self.varargs == other.varargs and
                self.type_table.get(self.return_type) == self.type_table.get(other.return_type))


class Mutability(Enum):
    CONSTANT = "const"
    ONCE = "let"
    MUTABLE = "var"


class TypeTable:
    """This gives types their unique ids and also tracks type inference"""

    def __init__(self) -> None:
        self.type_id: int = 0
        self.table: List[Type | int] = [TypeSentinel()]

        self.build_in_type_map: Dict[BuildInType, Type] = {
            BuildInType.INT: TypeInt(PLATFORM_INT_SIZE, "int"),
            BuildInType.UINT: TypeInt(PLATFORM_INT_SIZE, "uint"),
            BuildInType.BOOL: TypeBool(PLATFORM_BOOL_SIZE, "bool"),
            BuildInType.CHAR: TypeUInt(1, "char"),
            BuildInType.I8: TypeInt(1, "i8"),
            BuildInType.I16: TypeInt(2, "i16"),
            BuildInType.I32: TypeInt(4, "i32"),
            BuildInType.I64: TypeInt(8, "i64"),
            BuildInType.ISIZE: TypeInt(PLATFORM_POINTER_SIZE, "isize"),
            BuildInType.USIZE: TypeUInt(PLATFORM_POINTER_SIZE, "usize"),
            BuildInType.U8: TypeUInt(1, "u8"),
            BuildInType.U16: TypeUInt(2, "u16"),
            BuildInType.U32: TypeUInt(4, "u32"),
            BuildInType.U64: TypeUInt(8, "u64"),
            BuildInType.F32: TypeFloat(4, "f32"),
            BuildInType.F64: TypeFloat(8, "f64"),
            BuildInType.STRING: TypeUndefined(None, "str"),  # TODO not sure how to do this right now
            BuildInType.TYPE: TypeUndefined(None, "type")
        }

        self.int_literal = TypeInt(None, "int_literal")
        self.uint_literal = TypeUInt(None, "uint_literal")
        self.float_literal = TypeFloat(None, "float_literal")

    def new_build_in_type(self, build_in: BuildInType) -> int:
        return self.new(self.build_in_type_map[build_in])

    def new(self, new_type: Type) -> int:
        self.table.append(new_type)
        return len(self.table) - 1

    def new_larger_int(self, size: int) -> int:
        match size:
            case 1:
                return self.new_build_in_type(BuildInType.I16)
            case 2:
                return self.new_build_in_type(BuildInType.I32)
            case 4:
                return self.new_build_in_type(BuildInType.I64)
        raise RuntimeError()

    def get_with_table_index(self, index: int) -> Tuple[Type, int]:
        assert isinstance(index, int)
        new_index: Type | int = index
        last_index = index
        counter = 0
        while isinstance(new_index, int):
            counter += 1
            last_index = new_index
            new_index = self.table[new_index]
            if counter == 10000:
                raise RuntimeError("Infinite Loop in type table")
        return new_index, last_index

    def get(self, index: int) -> Type:
        return self.get_with_table_index(index)[0]

    def overwrite(self, old: int, new: int | Type | BuildInType):
        if old <= 0:
            raise RuntimeError("Compiler Error tried to alter Sentinel Type")
        current = old
        while isinstance(next_index := self.table[current], int):
            current = next_index
        self.table[old] = current  # this is not strictly necessary but reduces time to resolve type
        if isinstance(new, BuildInType):
            self.table[current] = self.build_in_type_map[new]
        else:
            self.table[current] = new
