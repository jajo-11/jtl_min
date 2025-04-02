from typing import Dict, List, TextIO, Optional
from dataclasses import dataclass, field

from elaboration_types import Name, Record
from lexer_types import CodeLocation


@dataclass
class IRType:
    size: int


class IRTypeInt(IRType):
    def __str__(self) -> str:
        return f"i{self.size * 8}"


class IRTypeFloat(IRType):
    def __str__(self) -> str:
        return f"f{self.size * 8}"


class IRTypeUInt(IRType):
    def __str__(self) -> str:
        return f"u{self.size * 8}"


class IRTypePointer(IRType):
    def __str__(self) -> str:
        return "ptr"


@dataclass
class IRTypeRecord(IRType):
    record: "IRRecord"

    def __str__(self) -> str:
        return f"&{self.record.name}"


@dataclass
class Register:
    name: str
    type: IRType

    def __str__(self) -> str:
        return f"%{self.name}"


@dataclass
class Immediate:
    value: int | float
    type: IRType

    def __str__(self) -> str:
        return str(self.value)


@dataclass
class IRInstruction:
    location: CodeLocation


@dataclass
class IRInstPHI(IRInstruction):
    dest: Register
    from1: str
    from2: str
    val1: Register | Immediate
    val2: Register | Immediate

    def __str__(self) -> str:
        return f"{self.dest} = PHI {self.dest.type} ({self.val1} from {self.from1}), ({self.val2} from {self.from2})"


@dataclass
class IRBinaryInstruction(IRInstruction):
    dest: Register
    op1: Register | Immediate
    op2: Register | Immediate
    name: str = "Generic Binary Instruction"

    def __str__(self) -> str:
        return f"{self.dest} = {self.name} {self.dest.type} {self.op1}, {self.op2}"

@dataclass
class IRInstAdd(IRBinaryInstruction):
    name: str = "add"


@dataclass
class IRInstSub(IRBinaryInstruction):
    name: str = "sub"


@dataclass
class IRInstMul(IRBinaryInstruction):
    name: str = "mul"


@dataclass
class IRInstDiv(IRBinaryInstruction):
    name: str = "div"


@dataclass
class IRInstRem(IRBinaryInstruction):
    name: str = "rem"

@dataclass
class IRInstAndBitwise(IRBinaryInstruction):
    name: str = "and_bitwise"


@dataclass
class IRInstOrBitwise(IRBinaryInstruction):
    name: str = "or_bitwise"


@dataclass
class IRInstXor(IRBinaryInstruction):
    name: str = "xor"


@dataclass
class IRInstEqual(IRBinaryInstruction):
    name: str = "eq"


@dataclass
class IRInstNotEqual(IRBinaryInstruction):
    name: str = "neq"


@dataclass
class IRInstLessEqual(IRBinaryInstruction):
    # TODO: For these comparison operators the ir currently shows u32 as type -> that is the resulting type
    # a better way would be listing the type of the operands
    name: str = "leq"


@dataclass
class IRInstGreaterEqual(IRBinaryInstruction):
    name: str = "geq"


@dataclass
class IRInstLess(IRBinaryInstruction):
    name: str = "less"


@dataclass
class IRInstGreater(IRBinaryInstruction):
    name: str = "greater"


@dataclass
class IRInstShiftRightLogical(IRBinaryInstruction):
    name: str = "lshr"


@dataclass
class IRInstShiftLeftLogical(IRBinaryInstruction):
    name: str = "shl"


@dataclass
class IRInstShiftRightArithmetic(IRBinaryInstruction):
    name: str = "ashr"


@dataclass
class IRUnaryInstruction(IRInstruction):
    dest: Register
    op1: Register | Immediate
    name: str = "Generic Unary Instruction"

    def __str__(self) -> str:
        return f"{self.dest} = {self.name} {self.dest.type} {self.op1}"


@dataclass
class IRInstNegative(IRUnaryInstruction):
    name: str = "neg"


@dataclass
class IRInstCopy(IRUnaryInstruction):
    name: str = "copy"


@dataclass
class IRInstCast(IRUnaryInstruction):
    name: str = "cast"


@dataclass
class IRInstReturn(IRInstruction):
    value: Register | Immediate | None

    def __str__(self) -> str:
        return f"return {self.value}"


@dataclass
class IRInstCall(IRInstruction):
    procedure: "IRProcedure"
    dest: Optional[Register]
    arguments: List[Register | Immediate]

    def __str__(self) -> str:
        args = ', '.join(map(lambda x: f'{x.type} {x}', self.arguments))
        if self.dest is None:
            return f"call {self.procedure}({args})"
        else:
            return f"{self.dest} = call {self.dest.type} {self.procedure}({args})"

@dataclass
class IRInstAllocate(IRInstruction):
    dest: Register
    size: int
    alignment: int

    def __str__(self) -> str:
        return f"{self.dest} = alloc {self.size} align {self.alignment}"


@dataclass
class IRInstStore(IRInstruction):
    dest: Register
    source: Register | Immediate

    def __str__(self) -> str:
        return f"store {self.source.type} {self.source} into {self.dest}"


@dataclass
class IRInstLoad(IRInstruction):
    dest: Register
    source: Register | Immediate

    def __str__(self) -> str:
        return f"{self.dest} = load {self.dest.type} {self.source}"


@dataclass
class IRInstLabel(IRInstruction):
    name: str

    def __str__(self) -> str:
        return f"{self.name}:"


@dataclass
class IRInstJump(IRInstruction):
    dest: str

    def __str__(self) -> str:
        return f"jump {self.dest}"


@dataclass
class IRInstJumpNotZero(IRInstruction):
    value: Register | Immediate
    is_zero: str
    not_zero: str

    def __str__(self) -> str:
        return f"jump_if {self.value} is zero {self.is_zero} else {self.not_zero}"


@dataclass
class IRInstGetElementPointer(IRInstruction):
    dest: Register
    base: Register
    element: Register | Immediate
    element_size: int
    # this is a bit redundant but having the offset is handy in qbe and llvm only needs the field_nr
    field_offset: int
    field_nr: int

    def __str__(self) -> str:
        return f"{self.dest} = get_ptr {self.base}[{self.element}].{self.field_nr}"


@dataclass
class IRInstMemcpy(IRInstruction):
    """Replace with procedure on backends discretion"""
    dest: Register
    src : Register | Immediate
    size: int

    def __str__(self) -> str:
        return f"memcpy {self.size} from {self.src} to {self.dest}"

@dataclass
class IRRecord:
    name: str
    fields: List[IRType]
    size: int

    def __str__(self) -> str:
        return f"&{self.name}"

    def write(self, out: TextIO):
        out.write(f"&{self.name} = type {', '.join(map(str, self.fields))}")


@dataclass
class IRProcedure:
    name: str
    arguments: Dict[str, IRType]
    return_type: Optional[IRType]
    alloc_instructions: List[IRInstruction]
    instructions: List[IRInstruction]
    export: bool = False

    def __str__(self) -> str:
        return f"@{self.name}"

    def write(self, out: TextIO):
        args = ', '.join(map(lambda x: f'{x[1]} %{x[0]}', self.arguments.items()))
        out.write(f"procedure {self.return_type} @{self.name}({args}) ")
        out.write("{\n")

        for inst in self.alloc_instructions:
            out.write(f"    {inst}\n")

        for inst in self.instructions:
            out.write(f"    {inst}\n")

        out.write("}\n")

@dataclass
class IRUnit:
    procedures: Dict[Name, IRProcedure] = field(default_factory=dict)
    records: Dict[Record, IRRecord] = field(default_factory=dict)
    alloc_instructions: List[IRInstruction] = field(default_factory=list)
    instructions: List[IRInstruction] = field(default_factory=list)

    def write(self, out: TextIO):
        for record in self.records.values():
            record.write(out)
            out.write("\n")

        for procedure in self.procedures.values():
            procedure.write(out)
            out.write("\n")

        for instruction in self.alloc_instructions:
            out.write(str(instruction))
            out.write("\n")

        for instruction in self.instructions:
            out.write(str(instruction))
            out.write("\n")
