from dataclasses import dataclass, field
from typing import List, Dict, Tuple, Optional

import typing_types
from ast_types import ASTNode, ASTNodeStatement, ASTNodeBinary, ASTNodeAssignment, ASTNodeUnary, ASTNodeValue, \
    ASTNodeScope, ASTNodeCall, ASTNodeIf, ASTNodeWhile, ASTNodeProcedure, ASTNodeCast, ASTNodeTransmute, \
    ASTNodeUnaryRight
from elaboration_types import Scope
from lexer_types import CodeLocation, TokenKeyword, Keyword, TokenOperator, Operator, TokenNumberLiteral, TokenName, \
    TokenStringLiteral, TokenBoolLiteral
from typing_types import Type, TypeGroup, TypeTable, Name, TypeProcedure, MemoryLocation


@dataclass
class IRType:
    width: int


class IRTypeInt(IRType):
    def __str__(self) -> str:
        return f"i{self.width*8}"


class IRTypeFloat(IRType):
    def __str__(self) -> str:
        return f"f{self.width*8}"


class IRTypeUInt(IRType):
    def __str__(self) -> str:
        return f"u{self.width*8}"

@dataclass()
class IRTypePointer(IRType):
    width: int = typing_types.PLATFORM_POINTER_SIZE

    def __str__(self):
        return f"ptr"


@dataclass
class VirtualRegister:
    name: str
    type: IRType

    def __str__(self) -> str:
        return f"<{self.type}>%{self.name}"

class VirtualVariable(VirtualRegister):

    def __str__(self) -> str:
        return f"<{self.type}>${self.name}"


@dataclass
class Immediate:
    value: int | float
    type: IRType

    def __str__(self) -> str:
        return f"<{self.type}>{self.value}"


@dataclass
class IRInstruction:
    location: CodeLocation


@dataclass
class IRInstBinary(IRInstruction):
    dest: VirtualRegister
    op1: VirtualRegister | Immediate
    op2: VirtualRegister | Immediate


class IRInstAdd(IRInstBinary):
    def __str__(self) -> str:
        return f"add {self.op1} {self.op2} -> {self.dest}"

class IRInstSub(IRInstBinary):
    def __str__(self) -> str:
        return f"subtract {self.op1} {self.op2} -> {self.dest}"

class IRInstMul(IRInstBinary):
    def __str__(self) -> str:
        return f"multiply {self.op1} {self.op2} -> {self.dest}"

class IRInstDiv(IRInstBinary):
    def __str__(self) -> str:
        return f"divide {self.op1} {self.op2} -> {self.dest}"

class IRInstRem(IRInstBinary):
    def __str__(self) -> str:
        return f"remainder {self.op1} {self.op2} -> {self.dest}"

class IRInstAndLogical(IRInstBinary):
    def __str__(self) -> str:
        return f"and_logical {self.op1} {self.op2} -> {self.dest}"

class IRInstAndBitwise(IRInstBinary):
    def __str__(self) -> str:
        return f"and_bitwise {self.op1} {self.op2} -> {self.dest}"

class IRInstOrLogical(IRInstBinary):
    def __str__(self) -> str:
        return f"or_logical {self.op1} {self.op2} -> {self.dest}"

class IRInstOrBitwise(IRInstBinary):
    def __str__(self) -> str:
        return f"or_bitwise {self.op1} {self.op2} -> {self.dest}"

class IRInstEqual(IRInstBinary):
    def __str__(self) -> str:
        return f"equal {self.op1} {self.op2} -> {self.dest}"

class IRInstNotEqual(IRInstBinary):
    def __str__(self) -> str:
        return f"not_equal {self.op1} {self.op2} -> {self.dest}"

class IRInstLessEqual(IRInstBinary):
    def __str__(self) -> str:
        return f"less_equal {self.op1} {self.op2} -> {self.dest}"

class IRInstGreaterEqual(IRInstBinary):
    def __str__(self) -> str:
        return f"greater_equal {self.op1} {self.op2} -> {self.dest}"

class IRInstLess(IRInstBinary):
    def __str__(self) -> str:
        return f"less {self.op1} {self.op2} -> {self.dest}"

class IRInstGreater(IRInstBinary):
    def __str__(self) -> str:
        return f"greater {self.op1} {self.op2} -> {self.dest}"

class IRInstShiftRightLogical(IRInstBinary):
    def __str__(self) -> str:
        return f"shift_right_logical {self.op1} {self.op2} -> {self.dest}"

class IRInstShiftLeftLogical(IRInstBinary):
    def __str__(self) -> str:
        return f"shift_left_logical {self.op1} {self.op2} -> {self.dest}"

class IRInstShiftRightArithmetic(IRInstBinary):
    def __str__(self) -> str:
        return f"shift_right_arithmetic {self.op1} {self.op2} -> {self.dest}"

class IRInstXor(IRInstBinary):
    def __str__(self) -> str:
        return f"xor {self.op1} {self.op2} -> {self.dest}"

@dataclass
class IRInstPhi(IRInstruction):
    dest: VirtualRegister
    src1: VirtualRegister | Immediate
    block1: "IRBlock"
    src2: VirtualRegister | Immediate
    block2: "IRBlock"

    def __str__(self) -> str:
        return f"phi @{self.block1.name} {self.src1} @{self.block2.name} {self.src2} -> {self.dest}"


@dataclass
class IRInstrUnary(IRInstruction):
    dest: VirtualRegister
    op1: VirtualRegister | Immediate


class IRInstNeg(IRInstrUnary):
    def __str__(self) -> str:
        return f"neg {self.op1} -> {self.dest}"

class IRInstNotLogical(IRInstrUnary):
    def __str__(self) -> str:
        return f"not_logical {self.op1} -> {self.dest}"

class IRInstNotBitwise(IRInstrUnary):
    def __str__(self) -> str:
        return f"not_bitwise {self.op1} -> {self.dest}"

class IRInstCast(IRInstrUnary):
    def __str__(self) -> str:
        return f"cast {self.op1} -> {self.dest}"

class IRInstMove(IRInstrUnary):
    def __str__(self) -> str:
        return f"move {self.op1} -> {self.dest}"

@dataclass
class IRInstAssign(IRInstruction):
    dest: VirtualRegister
    source: Immediate | VirtualRegister

    def __str__(self) -> str:
        return f"assign {self.source} -> {self.dest}"


@dataclass
class IRInstRet(IRInstruction):
    op1: VirtualRegister | Immediate | None

    def __str__(self) -> str:
        return f"return {self.op1}"


@dataclass
class IRInstrCall(IRInstruction):
    dest: Optional[VirtualRegister]
    procedure: "IRProcedure"
    arguments: List[VirtualRegister | Immediate]

    def __str__(self) -> str:
        return f"call {self.procedure.name} ({', '.join(map(str, self.arguments))}) -> {self.dest}"


@dataclass
class IRInstJump(IRInstruction):
    dest: "IRBlock"

    def __str__(self) -> str:
        return f"jump @{self.dest.name}"


@dataclass
class IRInstJumpNotZero(IRInstruction):
    dest_zero: "IRBlock"
    dest_not_zero: "IRBlock"
    condition: VirtualRegister | Immediate

    def __str__(self) -> str:
        return f"jump_not_zero {self.condition} -> @{self.dest_zero.name} | @{self.dest_not_zero.name}"

@dataclass
class IRInstAlloc:
    dest: VirtualRegister
    alignment: int
    size: int

    def __str__(self) -> str:
        return f"alloc {self.size} align {self.alignment}"


@dataclass
class IRInstStore(IRInstruction):
    dest: VirtualRegister
    source: VirtualRegister | Immediate

    def __str__(self) -> str:
        return f"store {self.source} -> {self.dest}"


@dataclass
class IRInstLoad(IRInstruction):
    dest: VirtualRegister
    source: VirtualRegister | Immediate

    def __str__(self) -> str:
        return f"load {self.source} -> {self.dest}"


@dataclass
class IRBlock:
    name: str
    instructions: List[IRInstruction] = field(default_factory=list)

    def __str__(self) -> str:
        return f"{self.name}:\n  " + "\n  ".join(map(str, self.instructions))


@dataclass
class IRProcedure:
    name: str
    blocks: List[IRBlock]
    return_register: VirtualVariable | Immediate | None
    arguments: List[VirtualVariable] = field(default_factory=list)

    def __str__(self) -> str:
        return_type = self.return_register.type if self.return_register else "None"
        out = f"procedure {self.name} ({', '.join(map(str, self.arguments))}) -> {return_type}:\n"
        return out + "\n".join(map(str, self.blocks))

binary_op_to_inst = {
    Operator.AND: IRInstAndLogical,
    Operator.OR: IRInstOrLogical,
    Operator.DIVIDE: IRInstDiv,
    Operator.MODULO: IRInstRem,
    Operator.TIMES: IRInstMul,
    Operator.LESS: IRInstLess,
    Operator.GREATER: IRInstGreater,
    Operator.LESSEQUAL: IRInstLessEqual,
    Operator.GREATEREQUAL: IRInstGreaterEqual,
    Operator.EQUAL: IRInstEqual,
    Operator.NOTEQUAL: IRInstNotEqual,
    Operator.PLUS: IRInstAdd,
    Operator.MINUS: IRInstSub,
    Operator.SHIFT_LEFT: IRInstShiftLeftLogical,
    Operator.SHIFT_RIGHT: IRInstShiftRightLogical,
    Operator.BITWISE_OR: IRInstOrBitwise,
    Operator.BITWISE_AND: IRInstAndBitwise,
    Operator.BITWISE_XOR: IRInstXor,
}

unary_op_to_inst = {
    Operator.MINUS: IRInstNeg,
    Operator.NOT: IRInstNotLogical,
    Operator.BITWISE_NOT: IRInstNotBitwise,
}

class IRUnit:
    __slots__ = ("type_table", "global_blocks", "n_variables", "variables", "procedures", "n_temporaries",
                 "n_ifs", "n_while", "n_proc", "main")

    def __init__(self, scope: Scope):
        assert scope.parent is None, "IRUnit expects parent scope as argument"
        self.type_table = scope.type_table
        self.variables: Dict[Name, VirtualRegister] = {}
        self.procedures: Dict[Name, IRProcedure] = {}
        self.global_blocks = [IRBlock("global_block")]
        self.n_temporaries: int = 0
        self.n_variables: int = 0
        self.n_ifs = 0
        self.n_while = 0
        self.n_proc = 0
        self.main: Optional[IRProcedure] = None

        for node in scope.nodes:
            self.parse_expr("global", node, scope, self.global_blocks)


    def type_to_ir_type(self, tt: int) -> Optional[IRType]:
        t = self.type_table.get(tt)
        if t.info.group == TypeGroup.NO_VALUE:
            return None
        assert t.info.size is not None, "Not implemented / Error"
        match t.info.group:
            case TypeGroup.INT:
                return IRTypeInt(t.info.size)
            case TypeGroup.FLOAT:
                return IRTypeFloat(t.info.size)
            case TypeGroup.UINT:
                return IRTypeUInt(t.info.size)
            case TypeGroup.BOOL:
                return IRTypeUInt(t.info.size)
            case TypeGroup.POINTER:
                return IRTypePointer()
            case _:
                raise NotImplementedError()

    def new_temporary(self, name: str, type: int):
        self.n_temporaries += 1
        register_type = self.type_to_ir_type(type)
        assert register_type is not None
        return VirtualRegister(f"t{self.n_temporaries}_{name}", register_type)

    def parse_left_value(self, expr: ASTNode, scope: Scope, instructions: List[IRInstruction])\
            -> Tuple[VirtualRegister, str]:
        """Returns a tuple of register and a name string.
        the name should be used for temporaries
        the instructions argument should be an empty list append these instructions after appending the right value
        instructions and before adding the final instruction
        you need to add the final store instruction yourself storing to the returned register returned from here
        """
        match expr:
            case ASTNodeValue(token=tok) if isinstance(tok, TokenName):
                nn = scope.lookup(tok.name)
                assert nn is not None
                return self.variables[nn], tok.name
            case ASTNodeUnaryRight(token=tok) if isinstance(tok, TokenOperator) and tok.op == Operator.POINTER:
                reg, name = self.parse_left_value(expr.child, scope, instructions)
                dest = self.new_temporary(name, expr.type)
                instructions.append(IRInstLoad(expr.child.token.location, dest, reg))
                return dest, name
            case _:
                raise NotImplementedError()

    def parse_expr(self, name: str, expr: ASTNode, scope: Scope, blocks: List[IRBlock]) \
            -> None | VirtualRegister | Immediate | IRProcedure:
        match expr:
            case ASTNodeStatement():
                match expr.token:
                    case (TokenKeyword(keyword=Keyword.LET) | TokenKeyword(keyword=Keyword.VARIABLE) |
                          TokenKeyword(keyword=Keyword.CONSTANT)):
                        assert False, "Compiler error should have been deleted in elaboration"
                    case TokenKeyword(keyword=Keyword.RETURN):
                        if expr.value is not None:
                            rv = self.parse_expr(f"return", expr.value, scope, blocks)
                            assert rv is not None
                            assert not isinstance(rv, IRProcedure)
                            blocks[-1].instructions.append(IRInstRet(expr.token.location, rv))
                        else:
                            blocks[-1].instructions.append(IRInstRet(expr.token.location, None))
                        return None
                    case _:
                        raise NotImplementedError()
            case ASTNodeAssignment():
                if isinstance(expr.target, MemoryLocation):
                    instructions: List[IRInstruction] = []
                    reg, name = self.parse_left_value(expr.target.expression, scope, instructions)
                    rv = self.parse_expr(name, expr.expression, scope, blocks)
                    assert rv is not None
                    if isinstance(rv, IRProcedure):
                        raise NotImplementedError("Compiler Error: can not reassign procedures")
                    blocks[-1].instructions += instructions
                    blocks[-1].instructions.append(IRInstStore(expr.token.location, reg, rv))
                    return rv
                else:
                    assert isinstance(expr.target, Name)
                    # these are declarations so there should be no such variable
                    assert self.variables.get(expr.target, None) is None

                    rv = self.parse_expr(expr.target.name, expr.expression, scope, blocks)
                    assert rv is not None
                    if isinstance(rv, IRProcedure):
                        if expr.target.name == "main":
                            self.main = rv
                        self.procedures[expr.target] = rv
                        return None
                    else:
                        self.n_variables += 1
                        n = VirtualVariable(f"v{self.n_variables}_{expr.target.name}", rv.type)
                        self.variables[expr.target] = n
                        blocks[-1].instructions.append(IRInstStore(expr.token.location, n, rv))
                        return rv
            case ASTNodeUnary():
                match expr.token:
                    case TokenOperator(op=Operator.PLUS):
                        # unary plus is a nop
                        return self.parse_expr(name, expr.child, scope, blocks)
                    case TokenOperator(_, op) if op in unary_op_to_inst.keys():
                        rv = self.parse_expr(name, expr.child, scope, blocks)
                        assert not isinstance(rv, IRProcedure)
                        if rv is not None:
                            cls = unary_op_to_inst[op]
                            dest = self.new_temporary(name, expr.type)
                            blocks[-1].instructions.append(cls(expr.token.location, dest, rv))
                            return dest
                        else:
                            raise RuntimeError(f'Compiler Error: unary operator must operate on something')
                    case TokenOperator(op=Operator.ADDRESS_OFF):
                        assert isinstance(expr.child, ASTNodeValue) and isinstance(expr.child.token, TokenName)
                        nn = scope.lookup(expr.child.token.name)
                        assert nn is not None
                        return self.variables[nn]
                    case _:
                        raise NotImplementedError()
            case ASTNodeUnaryRight(token=tok) if tok.op == Operator.POINTER:
                op1 = self.parse_expr(name, expr.child, scope, blocks)
                assert isinstance(op1   , VirtualRegister)
                dest = self.new_temporary(name, expr.type)
                blocks[-1].instructions.append(IRInstLoad(expr.token.location, dest, op1))
                return dest
            case ASTNodeBinary():
                op1 = self.parse_expr(name, expr.left, scope, blocks)
                op2 = self.parse_expr(name, expr.right, scope, blocks)
                assert op1 is not None
                assert op2 is not None
                assert not isinstance(op1, IRProcedure)
                assert not isinstance(op2, IRProcedure)
                dest = self.new_temporary(name, expr.type)
                if instr := binary_op_to_inst.get(expr.token.op):
                    if instr == IRInstShiftRightLogical and isinstance(op1.type, IRTypeUInt):
                        instr = IRInstShiftRightArithmetic
                    blocks[-1].instructions.append(instr(expr.token.location, dest, op1, op2))
                else:
                    raise NotImplementedError()
                return dest
            case ASTNodeValue():
                match expr.token:
                    case TokenNumberLiteral():
                        literal_type = self.type_to_ir_type(expr.type)
                        assert literal_type is not None
                        return Immediate(expr.token.value, literal_type)
                    case TokenName():
                        nn = scope.lookup(expr.token.name)
                        assert nn is not None
                        var_temp = self.new_temporary(nn.name, nn.type)
                        blocks[-1].instructions.append(IRInstLoad(expr.token.location, var_temp, self.variables[nn]))
                        return var_temp
                    case TokenStringLiteral():
                        raise NotImplementedError()
                    case TokenBoolLiteral():
                        return Immediate(1 if expr.token.value else 0, IRTypeUInt(8))
                    case _:
                        raise RuntimeError(f'Compiler Error: value should not be here in ir pass')
            case ASTNodeScope():
                local_scope = expr.elaborated_body
                assert local_scope is not None
                for node in local_scope.nodes:
                    self.parse_expr("", node, local_scope, blocks)
                return None
            case ASTNodeCall():
                # TODO currently the call gets the Name of the procedure but that is not the name the procedure has in
                # the IR, to fix this we would need to assure that the procedure is already named before a call is done
                # to assure this we need to parse constants before everything else maybe this can be done in the
                # elaborate stage (separate storage for constant declarations?)
                ir_proc = self.procedures.get(expr.procedure)
                assert ir_proc is not None, "Failure in evaluation order"
                args = []
                for x in expr.arguments:
                    if (xx := self.parse_expr(f"{name}_arg", x, scope, blocks)) is None:
                        raise RuntimeError(f'argument has no value')
                    assert not isinstance(xx, IRProcedure)
                    args.append(xx)
                proc_type = self.type_table.get(expr.procedure.type)
                assert isinstance(proc_type, TypeProcedure)
                if self.type_table.get(proc_type.return_type).info.group == TypeGroup.NO_VALUE:
                    dest = None
                else:
                    dest = self.new_temporary(name, proc_type.return_type)
                blocks[-1].instructions.append(IRInstrCall(expr.token.location, dest, ir_proc, args))
                return dest
            case ASTNodeIf():
                self.n_ifs += 1
                cond = self.parse_expr(f"if{self.n_ifs}_condition_{name}", expr.condition, scope, blocks)
                assert cond is not None
                assert not isinstance(cond, IRProcedure)
                true_block = IRBlock(f"if{self.n_ifs}_true")
                if expr.else_body is None:
                    second_block = IRBlock(f"if{self.n_ifs}_end")
                else:
                    second_block = IRBlock(f"if{self.n_ifs}_else")
                blocks[-1].instructions.append(
                    IRInstJumpNotZero(expr.condition.token.location, true_block, second_block, cond))
                blocks.append(true_block)
                true_value = self.parse_expr(name, expr.body, scope, blocks)
                if expr.else_body is not None:
                    assert expr.else_location is not None
                    final_block = IRBlock(f"if{self.n_ifs}_end")
                    blocks[-1].instructions.append(IRInstJump(expr.else_location, final_block))
                    blocks.append(second_block)
                    false_value = self.parse_expr(name, expr.else_body, scope, blocks)
                    blocks[-1].instructions.append(IRInstJump(expr.token.location, final_block))
                    blocks.append(final_block)
                    if true_value is not None:
                        assert not isinstance(true_value, IRProcedure)
                        assert false_value is not None
                        assert not isinstance(false_value, IRProcedure)
                        dest = self.new_temporary(name, expr.type)
                        blocks[-1].instructions.append(
                            IRInstPhi(expr.token.location, dest, true_value, true_block, false_value, second_block))
                        return dest
                else:
                    blocks.append(second_block)
                return None
            case ASTNodeWhile():
                self.n_while += 1
                blocks.append(loop_condition := IRBlock(f"while{self.n_while}_head"))
                loop_body = IRBlock(f"while{self.n_while}_body")
                loop_end = IRBlock(f"while{self.n_while}_end")
                cond = self.parse_expr(f"while{self.n_while}_condition", expr.condition, scope, blocks)
                assert cond is not None
                assert not isinstance(cond, IRProcedure)
                blocks[-1].instructions.append(IRInstJumpNotZero(expr.token.location, loop_body, loop_end, cond))
                blocks.append(loop_body)
                assert expr.elaborated_body is not None
                for local_expr in expr.elaborated_body.nodes:
                    self.parse_expr("", local_expr, expr.elaborated_body, blocks)
                blocks[-1].instructions.append(IRInstJump(expr.token.location, loop_condition))
                blocks.append(loop_end)
                return None
            case ASTNodeProcedure():
                # TODO think through passing procedures as arguments or defining them anonymously
                self.n_proc += 1
                proc_type = self.type_table.get(expr.type)
                assert isinstance(proc_type, TypeProcedure)
                proc_return_type = self.type_to_ir_type(proc_type.return_type)
                if proc_return_type is None:
                    return_register = None
                else:
                    return_register = VirtualVariable(f"proc{self.n_proc}_{name}_return", proc_return_type)
                arguments = []
                for arg in expr.argument_names:
                    argument_type = self.type_to_ir_type(arg.type)
                    assert argument_type is not None
                    v = VirtualVariable(f"proc{self.n_proc}_{name}_arg_{arg.name}", argument_type)
                    self.variables[arg] = v
                    arguments.append(v)
                block_list = [IRBlock(f"proc{self.n_proc}_{name}_body")]
                assert expr.elaborated_body is not None
                for local_expr in expr.elaborated_body.nodes:
                    self.parse_expr("", local_expr, expr.elaborated_body, block_list)
                return IRProcedure(f"proc{self.n_proc}_{name}", block_list, return_register, arguments)
            case ASTNodeCast():
                op1 = self.parse_expr(name, expr.node, scope, blocks)
                assert op1 is not None
                assert not isinstance(op1, IRProcedure)
                cast_result = self.new_temporary(name, expr.type)
                blocks[-1].instructions.append(IRInstCast(expr.token.location, cast_result, op1))
                return cast_result
            case ASTNodeTransmute():
                op1 = self.parse_expr(name, expr.node, scope, blocks)
                assert op1 is not None
                assert not isinstance(op1, IRProcedure)
                transmute_result = self.new_temporary(name, expr.type)
                blocks[-1].instructions.append(IRInstMove(expr.token.location, transmute_result, op1))
                return transmute_result
            case _:
                raise NotImplementedError()
        assert False, "Sentinel"


    def __str__(self) -> str:
        out = ""
        for procedure in self.procedures.values():
            out += str(procedure) + "\n\n"
        out += "\n"
        out += "\n".join(map(str, self.global_blocks))
        out += f"\nmain: {'Found' if self.main else 'Not Found'}"
        return out