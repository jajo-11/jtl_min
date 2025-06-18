from itertools import chain
from unittest import case

from ir_types import *
from typing_types import PLATFORM_POINTER_SIZE

if PLATFORM_POINTER_SIZE == 4:
    POINTER_QBE_TYPE = "w"
else:
    assert PLATFORM_POINTER_SIZE == 8
    POINTER_QBE_TYPE = "l"


def ir_type_to_qbe(ir_type: IRType) -> str:
    match ir_type:
        case IRTypeInt(size=size) if size <= 4:
            return "w"
        case IRTypeInt(size=8):
            return "l"
        case IRTypeUInt(size=size) if size <= 4:
            return "w"
        case IRTypeUInt(size=8):
            return "l"
        case IRTypeFloat(size=4):
            return "s"
        case IRTypeFloat(size=8):
            return "d"
        case IRTypePointer():
            return POINTER_QBE_TYPE
        case _:
            raise NotImplementedError()

def ir_type_to_qbe_ident(ir_type: IRType) -> Optional[str]:
    match ir_type:
        case IRTypeRecord():
            return f":record.{ir_type.record.name}"
        case IRTypeArray():
            item_type = str(ir_type.item_type)
            item_type = "".join(filter(lambda c: c not in "&[]", item_type))
            return f":array.{ir_type.length}.{item_type}"
        case _:
            return None

def ir_type_to_qbe_ext(ir_type: IRType) -> str:
    match ir_type:
        case IRTypeInt(size=1) | IRTypeUInt(size=1):
            return "b"
        case IRTypeInt(size=2) | IRTypeUInt(size=2):
            return "h"
        case _:
            if (ident := ir_type_to_qbe_ident(ir_type)) is not None:
                return ident
            return ir_type_to_qbe(ir_type)

def ir_type_to_qbe_subwty(ir_type: IRType) -> str:
    match ir_type:
        case IRTypeInt(size=1):
            return "sb"
        case IRTypeUInt(size=1):
            return "ub"
        case IRTypeInt(size=2):
            return "sh"
        case IRTypeUInt(size=2):
            return "uh"
        case _:
            if (ident := ir_type_to_qbe_ident(ir_type)) is not None:
                return ident
            return ir_type_to_qbe(ir_type)



def record_to_qbe(record: IRRecord) -> str:
    type_list = ", ".join(map(ir_type_to_qbe_ext, record.fields))
    return f"type :record.{record.name} = align {record.align} {{ {type_list} }}"


def array_to_qbe(array: IRTypeArray) -> str:
    return f"type {ir_type_to_qbe_ident(array)} = {{ {ir_type_to_qbe_ext(array.item_type)} {array.length} }}"


def qbe_value(value: Register | Immediate) -> str:
    if isinstance(value, Register):
        return f"%reg.{value.name}"
    else:
        if isinstance(value.type, IRTypeFloat):
            return f"{ir_type_to_qbe(value.type)}_{value.value}"
        elif isinstance(value.type, IRTypePointer):
            return f"$str.{value.value}"
        return str(value.value)


def is_integer(type: IRType) -> bool:
    return isinstance(type, (IRTypeInt, IRTypeUInt, IRTypePointer))


def comparison_sign_prefix(value: Register | Immediate) -> str:
    """This generates the prefix for comparison instructions"""
    match value.type:
        case IRTypeFloat():
            return ""
        case IRTypeUInt() | IRTypePointer():
            return "u"
        case IRTypeInt():
            return "s"
        case _:
            raise RuntimeError(f"No signedness for object {value.type}")


def write_shift(inst: IRBinaryInstruction, op: str, out_file: TextIO):
    assert is_integer(inst.op1.type)
    out_file.write(f"   %reg.{inst.dest.name} ={ir_type_to_qbe(inst.dest.type)} " +
                   f"{op} {qbe_value(inst.op1)}, {qbe_value(inst.op2)}\n")


def write_comparison(inst: IRBinaryInstruction, op: str, out_file: TextIO):
    assert not isinstance(inst.op1.type, IRTypeRecord)
    assert inst.op1.type == inst.op2.type
    assert not isinstance(inst.op1.type, IRTypeRecord)
    out_file.write(f"   %reg.{inst.dest.name} =w " +
                   f"c{comparison_sign_prefix(inst.op1)}{op}{ir_type_to_qbe(inst.dest.type)} " +
                   f"{qbe_value(inst.op1)}, {qbe_value(inst.op2)}\n")


def procedure_to_qbe(procedure: IRProcedure, out_file: TextIO):
    out_file.write(f"dbgfile \"{procedure.location.file_name}\"\n")
    return_type = "" if procedure.return_type is None else f"{ir_type_to_qbe_subwty(procedure.return_type)} "
    arguments = ", ".join([f"{ir_type_to_qbe_subwty(t)} %reg.{n}" for n, t in procedure.arguments.items()])
    if procedure.export:
        header = f"export function {return_type}${procedure.name}({arguments}) {{\n"
    else:
        header = f"function {return_type}$proc.{procedure.name}({arguments}) {{\n"
    out_file.write(header)
    out_file.write("@start\n")

    for inst in chain(procedure.alloc_instructions, procedure.instructions):
        # if not isinstance(inst, (IRInstLabel, IRInstPHI)):
        #     out_file.write(f"   dbgloc {inst.location.line_start}, {inst.location.col_start}\n")
        match inst:
            case IRInstLabel():
                out_file.write(f"@label.{inst.name}\n")
            case IRInstJump():
                out_file.write(f"   jmp @label.{inst.dest}\n")
            case IRInstJumpNotZero():
                out_file.write(f"   jnz {qbe_value(inst.value)}, @label.{inst.not_zero}, @label.{inst.is_zero}\n")
            case IRInstReturn():
                if inst.value is None:
                    out_file.write("    ret\n")
                else:
                    out_file.write(f"   ret {qbe_value(inst.value)}\n")
            case IRInstAdd():
                out_file.write(f"   %reg.{inst.dest.name} ={ir_type_to_qbe(inst.dest.type)} " +
                               f"add {qbe_value(inst.op1)}, {qbe_value(inst.op2)}\n")
            case IRInstSub():
                out_file.write(
                    f"   %reg.{inst.dest.name} ={ir_type_to_qbe(inst.dest.type)} " +
                    f"sub {qbe_value(inst.op1)}, {qbe_value(inst.op2)}\n")
            case IRInstDiv():
                op = "udiv" if isinstance(inst.dest.type, IRTypeUInt) else "div"
                out_file.write(
                    f"   %reg.{inst.dest.name} ={ir_type_to_qbe(inst.dest.type)} " +
                    f"{op} {qbe_value(inst.op1)}, {qbe_value(inst.op2)}\n")
            case IRInstRem():
                assert is_integer(inst.dest.type)
                op = "urem" if isinstance(inst.dest.type, IRTypeUInt) else "rem"
                out_file.write(
                    f"   %reg.{inst.dest.name} ={ir_type_to_qbe(inst.dest.type)} " +
                    f"{op} {qbe_value(inst.op1)}, {qbe_value(inst.op2)}\n")
            case IRInstMul():
                out_file.write(f"   %reg.{inst.dest.name} ={ir_type_to_qbe(inst.dest.type)} " +
                               f"mul {qbe_value(inst.op1)}, {qbe_value(inst.op2)}\n")
            case IRInstNegative():
                out_file.write(f"   %reg.{inst.dest.name} ={ir_type_to_qbe(inst.dest.type)} " +
                               f"neg {qbe_value(inst.op1)}\n")
            case IRInstOrBitwise():
                assert is_integer(inst.dest.type)
                out_file.write(f"   %reg.{inst.dest.name} ={ir_type_to_qbe(inst.dest.type)} " +
                               f"or {qbe_value(inst.op1)}, {qbe_value(inst.op2)}\n")
            case IRInstAndBitwise():
                assert is_integer(inst.dest.type)
                out_file.write(f"   %reg.{inst.dest.name} ={ir_type_to_qbe(inst.dest.type)} " +
                               f"and {qbe_value(inst.op1)}, {qbe_value(inst.op2)}\n")
            case IRInstXor():
                assert is_integer(inst.dest.type)
                out_file.write(f"   %reg.{inst.dest.name} ={ir_type_to_qbe(inst.dest.type)} " +
                               f"xor {qbe_value(inst.op1)}, {qbe_value(inst.op2)}\n")
            case IRInstShiftRightArithmetic():
                write_shift(inst, "sar", out_file)
            case IRInstShiftRightLogical():
                write_shift(inst, "shr", out_file)
            case IRInstShiftLeftLogical():
                write_shift(inst, "shl", out_file)
            case IRInstPHI():
                out_file.write(f"   %reg.{inst.dest.name} ={ir_type_to_qbe(inst.dest.type)} " +
                               f"phi @label.{inst.from1} {qbe_value(inst.val1)}, " +
                               f"@label.{inst.from2} {qbe_value(inst.val2)}\n")
            case IRInstEqual():
                assert not isinstance(inst.op1.type, IRTypeRecord)
                assert inst.op1.type == inst.op2.type
                assert not isinstance(inst.op1.type, IRTypeRecord)
                out_file.write(f"   %reg.{inst.dest.name} =w ceq{ir_type_to_qbe(inst.dest.type)} " +
                               f"{qbe_value(inst.op1)}, {qbe_value(inst.op2)}\n")
            case IRInstNotEqual():
                assert not isinstance(inst.op1.type, IRTypeRecord)
                assert inst.op1.type == inst.op2.type
                assert not isinstance(inst.op1.type, IRTypeRecord)
                out_file.write(f"   %reg.{inst.dest.name} =w cne{ir_type_to_qbe(inst.dest.type)} " +
                               f"{qbe_value(inst.op1)}, {qbe_value(inst.op2)}\n")
            case IRInstLessEqual():
                write_comparison(inst, "le", out_file)
            case IRInstGreaterEqual():
                write_comparison(inst, "ge", out_file)
            case IRInstLess():
                write_comparison(inst, "lt", out_file)
            case IRInstGreater():
                write_comparison(inst, "gt", out_file)
            case IRInstCopy():
                assert not isinstance(inst.op1.type, IRTypeRecord)
                op = "cast" if isinstance(inst.op1.type, IRTypeFloat) or isinstance(inst.dest.type,
                                                                                    IRTypeFloat) else "copy"
                out_file.write(f"   %reg.{inst.dest.name} ={ir_type_to_qbe(inst.dest.type)} " +
                               f"{op} {qbe_value(inst.op1)}\n")
            case IRInstCast():
                # TODO missing the extended types not w/l
                dest_type = ir_type_to_qbe(inst.dest.type)
                src_type = ir_type_to_qbe(inst.op1.type)
                out_file.write(f"   %reg.{inst.dest.name} ={dest_type} ")
                if dest_type == src_type:
                    out_file.write(f"copy {qbe_value(inst.op1)}\n")
                elif dest_type == "d" and src_type == "s":
                    out_file.write(f"exts {qbe_value(inst.op1)}\n")
                elif dest_type == "s" and src_type == "d":
                    out_file.write(f"truncd {qbe_value(inst.op1)}\n")
                elif isinstance(inst.dest.type, IRTypeFloat) and isinstance(inst.op1.type, IRTypeInt):
                    out_file.write(f"s{src_type}tof {qbe_value(inst.op1)}\n")
                elif isinstance(inst.dest.type, IRTypeFloat) and isinstance(inst.op1.type, (IRTypeUInt, IRTypePointer)):
                    out_file.write(f"u{src_type}tof {qbe_value(inst.op1)}\n")
                elif isinstance(inst.dest.type, (IRTypeUInt, IRTypePointer)) and isinstance(inst.op1.type, IRTypeFloat):
                    out_file.write(f"{src_type}toui {qbe_value(inst.op1)}\n")
                elif isinstance(inst.dest.type, IRTypeInt) and isinstance(inst.op1.type, IRTypeFloat):
                    out_file.write(f"{src_type}tosi {qbe_value(inst.op1)}\n")
                elif isinstance(inst.dest.type, IRTypeInt) and isinstance(inst.op1.type, IRTypeInt):
                    if inst.dest.type.size < inst.op1.type.size:
                        out_file.write(f"copy {qbe_value(inst.op1)}\n")
                    else:
                        out_file.write(f"extsw {qbe_value(inst.op1)}\n")
                elif (isinstance(inst.dest.type, (IRTypeUInt, IRTypePointer)) and
                      isinstance(inst.op1.type, (IRTypeUInt, IRTypePointer))):
                    if inst.dest.type.size < inst.op1.type.size:
                        out_file.write(f"copy {qbe_value(inst.op1)}\n")
                    else:
                        out_file.write(f"extuw {qbe_value(inst.op1)}\n")
                elif is_integer(inst.dest.type) and is_integer(
                        inst.op1.type) and inst.dest.type.size > inst.op1.type.size:
                    dest_sign = "s" if isinstance(inst.dest.type, IRTypeInt) else "u"
                    out_file.write(f"ext{dest_sign}w {qbe_value(inst.op1)}\n")
                else:
                    raise NotImplementedError(
                        f"Qbe backend does not know how to convert from {src_type} to {dest_type}")
            case IRInstReturn():
                if inst.value is not None:
                    out_file.write(f"   ret {qbe_value(inst.value)}\n")
                else:
                    out_file.write(f"   ret\n")
            case IRInstCall():
                out_file.write("   ")
                if inst.dest is not None:
                    out_file.write(f"%reg.{inst.dest.name} ={ir_type_to_qbe_subwty(inst.dest.type)} ")
                if inst.procedure.export:
                    out_file.write(f"call ${inst.procedure.name} (")
                else:
                    out_file.write(f"call $proc.{inst.procedure.name} (")
                for arg in inst.arguments:
                    if isinstance(arg, VarArgMarker):
                        out_file.write(f"..., ")
                    else:
                        out_file.write(f"{ir_type_to_qbe_subwty(arg.type)} {qbe_value(arg)}, ")
                out_file.write(")\n")
            case IRInstAllocate():
                # TODO: this might let some bad cases through without assertion
                alignment = 4 if inst.size < 4 else inst.alignment
                assert alignment in [4, 8, 16]
                out_file.write(
                    f"   %reg.{inst.dest.name} ={POINTER_QBE_TYPE} alloc{alignment} {inst.size}\n")
            case IRInstStore():
                out_file.write(
                    f"   store{ir_type_to_qbe_ext(inst.source.type)} {qbe_value(inst.source)}, {qbe_value(inst.dest)}\n")
            case IRInstLoad():
                match inst.dest.type:
                    case IRTypeFloat(size=8):
                        out_file.write(f"   %reg.{inst.dest.name} =d loadd {qbe_value(inst.source)}\n")
                    case IRTypeFloat(size=4):
                        out_file.write(f"   %reg.{inst.dest.name} =s loads {qbe_value(inst.source)}\n")
                    case IRTypeUInt(size=8) | IRTypePointer(size=8) | IRTypeInt(size=8):
                        out_file.write(f"   %reg.{inst.dest.name} =l loadl {qbe_value(inst.source)}\n")
                    case IRTypeUInt(size=4) | IRTypePointer(size=4):
                        out_file.write(f"   %reg.{inst.dest.name} =w loaduw {qbe_value(inst.source)}\n")
                    case IRTypeInt(size=4):
                        out_file.write(f"   %reg.{inst.dest.name} =w loadsw {qbe_value(inst.source)}\n")
                    case IRTypeUInt(size=2):
                        out_file.write(f"   %reg.{inst.dest.name} =w loaduh {qbe_value(inst.source)}\n")
                    case IRTypeInt(size=2):
                        out_file.write(f"   %reg.{inst.dest.name} =w loadsh {qbe_value(inst.source)}\n")
                    case IRTypeUInt(size=1):
                        out_file.write(f"   %reg.{inst.dest.name} =w loadub {qbe_value(inst.source)}\n")
                    case IRTypeInt(size=1):
                        out_file.write(f"   %reg.{inst.dest.name} =w loadsb {qbe_value(inst.source)}\n")
                    case _:
                        raise NotImplementedError()
            case IRInstMemcpy():
                out_file.write(f"   blit {qbe_value(inst.src)}, {qbe_value(inst.dest)}, {inst.size}\n")
            case IRInstGetElementPointer():
                type_char = ir_type_to_qbe(inst.dest.type)
                out_file.write(
                    f"   %reg.0.{inst.dest.name} ={type_char} mul {qbe_value(inst.element)}, {inst.element_size}\n")
                out_file.write(
                    f"   %reg.1.{inst.dest.name} ={type_char} add %reg.0.{inst.dest.name}, {inst.field_offset}\n")
                out_file.write(
                    f"   %reg.{inst.dest.name} ={type_char} add {qbe_value(inst.base)}, %reg.1.{inst.dest.name}\n")
            case _:
                raise NotImplementedError(f"No handler for {inst}")

    if procedure.return_type is None and not isinstance(procedure.instructions[-1], IRInstReturn):
        out_file.write("   ret\n")

    out_file.write("}\n")


def write_qbe(unit: IRUnit, out_file: TextIO):
    for record in unit.records.values():
        out_file.write(record_to_qbe(record))
        out_file.write("\n")

    for array in unit.fixed_size_arrays:
        out_file.write(array_to_qbe(array))
        out_file.write("\n")

    if len(unit.records) > 0:
        out_file.write("\n")

    for name, data in unit.data_literals:
        out_file.write(f"data $str.{name} = {{ b \"{data}\", b 0 }}")

    if len(unit.data_literals) > 0:
        out_file.write("\n")

    for procedure in unit.procedures.values():
        if procedure.stub:
            continue
        procedure_to_qbe(procedure, out_file)
        out_file.write("\n")
