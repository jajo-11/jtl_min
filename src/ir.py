from copy import copy
from typing import cast, Callable

from ast_types import ASTNode, ASTNodeValue, ASTNodeBinary, ASTNodeUnary, ASTNodeAssignment, ASTNodeUnaryRight, \
    ASTNodeStatement, ASTNodeCast, ASTNodeTransmute, ASTNodeCall, ASTNodeIf, ASTNodeWhile
from elaboration_types import Scope, Name, Record
from ir_types import *
from lexer_types import TokenNumberLiteral, TokenName, TokenStringLiteral, TokenBoolLiteral, Operator, Keyword, \
    TokenKeyword
from typing_types import TypeGroup, PLATFORM_POINTER_SIZE, TypeRecordType, PLATFORM_BOOL_SIZE, TypeRecordInstance, \
    TypeProcedure, TypePointer

BinaryOpToInstruction: Dict[Operator, type[IRBinaryInstruction]] = {
    Operator.PLUS: IRInstAdd,
    Operator.MINUS: IRInstSub,
    Operator.TIMES: IRInstMul,
    Operator.DIVIDE: IRInstDiv,
    Operator.MODULO: IRInstRem,
    Operator.BITWISE_AND: IRInstAndBitwise,
    Operator.BITWISE_OR: IRInstOrBitwise,
    Operator.BITWISE_XOR: IRInstXor,
    Operator.EQUAL: IRInstEqual,
    Operator.NOTEQUAL: IRInstNotEqual,
    Operator.LESS: IRInstLess,
    Operator.LESSEQUAL: IRInstLessEqual,
    Operator.GREATER: IRInstGreater,
    Operator.GREATEREQUAL: IRInstGreaterEqual,
    Operator.SHIFT_LEFT: IRInstShiftLeftLogical,
}

no_location = CodeLocation("phony", 0, 0, 0, "")

class IRContext:
    def __init__(self, global_scope: Scope):
        assert global_scope.parent is None, "IRUnit expects global scope as argument"
        self.global_scope = global_scope
        self.type_table = global_scope.type_table
        # a map from name object to register containing a pointer to the variable on the stack
        self.variable_stack_register: Dict[Name, Register] = {}
        self.n_temporary: int = 0
        self.unit: IRUnit = IRUnit()
        self.current_procedures: List[IRProcedure] = []

    def add_allocation(self, instruction: IRInstAllocate):
        if len(self.current_procedures) > 0:
            self.current_procedures[-1].alloc_instructions.append(instruction)
        else:
            self.unit.alloc_instructions.append(instruction)

    def add_instruction(self, instruction: IRInstruction):
        if len(self.current_procedures) > 0:
            self.current_procedures[-1].instructions.append(instruction)
        else:
            self.unit.instructions.append(instruction)

    def lower_unit(self) -> IRUnit:
        self.lower_scope(self.global_scope, [])
        return self.unit

    def type_to_ir_type(self, tt: int) -> Optional[IRType]:
        t = self.type_table.get(tt)
        if t.info.group == TypeGroup.NO_VALUE:
            return None
        assert t.info.size is not None, "IR types can not be zero sized except NO_VALUE"
        match t.info.group:
            case TypeGroup.INT:
                return IRTypeInt(t.info.size)
            case TypeGroup.UINT:
                return IRTypeUInt(t.info.size)
            case TypeGroup.FLOAT:
                return IRTypeFloat(t.info.size)
            case TypeGroup.BOOL:
                return IRTypeUInt(PLATFORM_BOOL_SIZE)
            case TypeGroup.SYMBOL:
                return IRTypeUInt(1)
            case TypeGroup.POINTER:
                return IRTypePointer(PLATFORM_POINTER_SIZE)
            case TypeGroup.RECORD:
                assert isinstance(t, TypeRecordType)
                record = self.unit.records.get(t.record)
                assert record is not None
                return IRTypeRecord(t.record.get_size(), record)
            case TypeGroup.PROCEDURE:
                return IRTypePointer(PLATFORM_POINTER_SIZE)
            case TypeGroup.TYPE:
                raise NotImplementedError()
            case TypeGroup.RETURNS:
                raise RuntimeError("Lowering to IR should not encounter RETURNS type")
            case _:
                raise RuntimeError(f"Un-handled type group {t.info.group}")

    def type_to_ir_type_not_none(self, tt: int) -> IRType:
        rv = self.type_to_ir_type(tt)
        assert(rv is not None)
        return rv

    def new_temporary(self, t: IRType | int) -> Register:
        if isinstance(t, int):
            t_ = self.type_to_ir_type(t)
            assert t_ is not None
            t = t_
        self.n_temporary += 1
        return Register(f"{self.n_temporary}", t)

    def new_label(self, loc: CodeLocation, suffix: Optional[str] = None) -> IRInstLabel:
        self.n_temporary += 1
        if suffix is not None:
            return IRInstLabel(loc, f"{self.n_temporary}_{suffix}")
        else:
            return IRInstLabel(loc, f"{self.n_temporary}")

    def l_value_to_ir(self, l_value: ASTNode | Name, scope: Scope) -> Register:
        if isinstance(l_value, Name):
            return self.variable_stack_register[l_value]
        else:
            match l_value:
                case ASTNodeValue():
                    # elaboration should ensure that this is a Name and not a literal
                    assert isinstance(l_value.token, TokenName)
                    nn = scope.lookup(l_value.token.name)
                    assert nn is not None
                    return self.variable_stack_register[nn]
                case ASTNodeBinary(token=tok) if tok.op == Operator.DOT:
                    base = self.l_value_to_ir(l_value.left, scope)
                    base_type = scope.type_table.get(l_value.left.type)
                    if isinstance(base_type, TypePointer):
                        deref_base = self.new_temporary(IRTypePointer(PLATFORM_POINTER_SIZE))
                        self.add_instruction(IRInstLoad(l_value.token.location, deref_base, base))
                        base = deref_base
                    return self.get_ptr_to_field(l_value, base)
                case ASTNodeUnaryRight(token=tok) if tok.op == Operator.POINTER:
                    # ast_node_to_ir will automatically load any variables (i.e. dereference them)
                    rv = self.ast_node_to_ir(l_value.child, scope)
                    assert isinstance(rv, Register)
                    return rv
                case _:
                    raise RuntimeError(f"Unexpected node in l_value")

    def get_ptr_to_field(self, node: ASTNodeBinary, base: Register) -> Register:
        assert base is not None
        assert isinstance(base, Register)
        assert isinstance(base.type, IRTypePointer) or isinstance(base.type, IRTypeRecord)
        lt = self.type_table.get(node.left.type)
        if isinstance(lt, TypePointer):
            lt = self.type_table.get(lt.target_type)
        assert isinstance(lt, TypeRecordInstance)
        assert isinstance(node.right, ASTNodeValue) and isinstance(node.right.token, TokenName)
        offsets = lt.record.get_offsets()
        field_nr = list(offsets.keys()).index(node.right.token.name)
        offset = offsets[node.right.token.name]
        ptr_dest = self.new_temporary(IRTypePointer(PLATFORM_POINTER_SIZE))
        self.add_instruction(IRInstGetElementPointer(
            node.token.location,
            ptr_dest,
            base,
            Immediate(0, IRTypeUInt(PLATFORM_POINTER_SIZE)),
            offset,
            field_nr
        ))
        return ptr_dest

    def ast_node_to_ir(self, node: ASTNode, scope: Scope) -> Optional[Register | Immediate]:
        match node:
            case ASTNodeValue():
                # R-Value
                match node.token:
                    case TokenNumberLiteral():
                        t = self.type_to_ir_type_not_none(node.type)
                        return Immediate(node.token.value, t)
                    case TokenName():
                        nn = scope.lookup(node.token.name)
                        assert nn is not None
                        ptr = self.variable_stack_register[nn]
                        if isinstance(self.type_table.get(nn.type), TypeRecordInstance):
                            # if it is a record we only want the address
                            return ptr
                        else:
                            # for values, we want to load from stack
                            temp = self.new_temporary(nn.type)
                            self.add_instruction(IRInstLoad(node.token.location, temp, ptr))
                            return temp
                    case TokenStringLiteral():
                        raise NotImplementedError()
                    case TokenBoolLiteral():
                        return Immediate(1 if node.token.value else 0, IRTypeUInt(PLATFORM_BOOL_SIZE))
                    case _:
                        raise NotImplementedError()
            case ASTNodeBinary(token=tok) if tok.op == Operator.AND or tok.op == Operator.OR:
                op1 = self.ast_node_to_ir(node.left, scope)
                assert op1 is not None
                op_label = self.new_label(node.token.location, str(tok.op) + "_left")
                self.add_instruction(op_label)
                second_condition = self.new_label(node.token.location, str(tok.op) + "_right")
                phi_label = self.new_label(node.token.location, str(tok.op))
                if tok.op == Operator.AND:
                    self.add_instruction(IRInstJumpNotZero(node.token.location, op1, second_condition.name, phi_label.name))
                else:
                    self.add_instruction(IRInstJumpNotZero(node.token.location, op1, phi_label.name, second_condition.name))
                self.add_instruction(second_condition)
                op2 = self.ast_node_to_ir(node.right, scope)
                assert op2 is not None
                self.add_instruction(IRInstJump(node.right.token.location, phi_label.name))
                self.add_instruction(phi_label)
                final_result = self.new_temporary(node.type)
                self.add_instruction(IRInstPHI(node.token.location, final_result, op_label.name, second_condition.name,
                                              op1, op2))
                return final_result
            case ASTNodeBinary(token=tok) if tok.op == Operator.DOT:
                base = self.ast_node_to_ir(node.left, scope)
                assert isinstance(base, Register)
                ptr_dest = self.get_ptr_to_field(node, base)
                if isinstance(self.type_table.get(node.type), TypeRecordInstance):
                    return ptr_dest
                else:
                    dest = self.new_temporary(node.type)
                    self.add_instruction(IRInstLoad(node.token.location, dest, ptr_dest))
                    return dest
            case ASTNodeBinary():
                op1 = self.ast_node_to_ir(node.left, scope)
                op2 = self.ast_node_to_ir(node.right, scope)
                assert op1 is not None
                assert op2 is not None
                temp = self.new_temporary(node.type)
                if (instr := BinaryOpToInstruction.get(node.token.op)) is None:
                    if node.token.op == Operator.SHIFT_RIGHT:
                        if isinstance(temp.type, IRTypeUInt):
                            instr = IRInstShiftRightLogical
                        else:
                            instr = IRInstShiftRightArithmetic
                    else:
                        raise NotImplementedError()
                self.add_instruction(instr(node.token.location, temp, op1, op2))
                return temp
            case ASTNodeUnary(token=tok) if tok.op == Operator.ADDRESS_OFF:
                if isinstance(node.child, ASTNodeValue):
                    assert isinstance(node.child.token, TokenName)
                    address_name = scope.lookup(node.child.token.name)
                    assert address_name is not None
                    return self.variable_stack_register[address_name]
                elif isinstance(node.child, ASTNodeAssignment):
                    _ = self.ast_node_to_ir(node.child, scope)
                    assert isinstance(node.child.target, Name)
                    return self.variable_stack_register[node.child.target]
                elif isinstance(node.child, ASTNodeBinary) and node.child.token.op == Operator.DOT:
                    base = self.ast_node_to_ir(node.child.left, scope)
                    assert isinstance(base, Register)
                    return self.get_ptr_to_field(node.child, base)
                else:
                    raise RuntimeError("Compiler Error can not take address of this")
            case ASTNodeUnary():
                op1 = self.ast_node_to_ir(node.child, scope)
                assert op1 is not None
                match node.token.op:
                    case Operator.BITWISE_NOT:
                        dest = self.new_temporary(node.type)
                        self.add_instruction(IRInstNotBitwise(node.token.location, dest, op1))
                        return dest
                    case Operator.MINUS:
                        dest = self.new_temporary(node.type)
                        self.add_instruction(IRInstNegative(node.token.location, dest, op1))
                        return dest
                    case Operator.PLUS:
                        return op1
                    case _:
                        raise NotImplementedError()
            case ASTNodeUnaryRight(token=tok) if tok.op == Operator.POINTER:
                op1 = self.ast_node_to_ir(node.child, scope)
                assert op1 is not None
                assert not isinstance(op1.type, IRTypeRecord)
                dest = self.new_temporary(node.type)
                self.add_instruction(IRInstLoad(node.token.location, dest, op1))
                return dest
            case ASTNodeAssignment():
                if isinstance(node.expression, ASTNodeCall) and (record := scope.lookup_record(node.expression.procedure)) is not None:
                    argument_registers = [self.ast_node_to_ir(a, scope) for a in node.expression.arguments]
                    dest_base = self.l_value_to_ir(node.target, scope)
                    for i, (reg, field) in enumerate(zip(argument_registers, record.get_offsets().values())):
                        assert reg is not None
                        dest = self.new_temporary(IRTypePointer(PLATFORM_POINTER_SIZE))
                        self.add_instruction(IRInstGetElementPointer(node.token.location, dest, dest_base,
                                                                    Immediate(0, IRTypeUInt(PLATFORM_POINTER_SIZE)),
                                                                    field, i))
                        self.add_instruction(IRInstStore(node.token.location, dest, reg))
                    return dest_base
                else:
                    value = self.ast_node_to_ir(node.expression, scope)
                    dest = self.l_value_to_ir(node.target, scope)
                    assert value is not None
                    assert isinstance(dest, Register)
                    expr_type = self.type_table.get(node.expression.type)
                    if isinstance(expr_type, TypeRecordInstance):
                        self.add_instruction(IRInstMemcpy(node.token.location, dest, value, expr_type.record.get_size()))
                    else:
                        self.add_instruction(IRInstStore(node.token.location, dest, value))
                    return value
            case ASTNodeStatement():
                match node.token:
                    case TokenKeyword(keyword=Keyword.RETURN):
                        if node.child is None:
                            self.add_instruction(IRInstReturn(node.token.location, None))
                            return None
                        else:
                            value = self.ast_node_to_ir(node.child, scope)
                            self.add_instruction(IRInstReturn(node.token.location, value))
                            return None
                    case _:
                        raise NotImplementedError()
            case ASTNodeCast():
                op1 = self.ast_node_to_ir(node.child, scope)
                assert op1 is not None
                dest = self.new_temporary(node.type)
                assert dest is not None
                self.add_instruction(IRInstCast(node.token.location, dest, op1))
                return dest
            case ASTNodeTransmute():
                # this is not a nop to make it easier for the backend to transmute floats to ints as they are in
                # different registers in many ISAs
                op1 = self.ast_node_to_ir(node.child, scope)
                assert op1 is not None
                dest = self.new_temporary(node.type)
                self.add_instruction(IRInstCopy(node.token.location, dest, op1))
                return dest
            case ASTNodeCall():
                tt = self.type_table.get(node.procedure.type)
                if isinstance(tt, TypeProcedure):
                    proc = self.unit.procedures[node.procedure]
                    if self.type_table.get(tt.return_type).info.group == TypeGroup.NO_VALUE:
                        return_register = None
                    else:
                        return_register = self.new_temporary(tt.return_type)
                    arguments = [self.ast_node_to_ir(arg, scope) for arg in node.arguments]
                    args = cast(List[Register | Immediate], arguments)
                    self.add_instruction(IRInstCall(node.token.location, proc, return_register, args))
                    return return_register
                elif isinstance(tt, TypeRecordType):
                    raise RuntimeError("Should have been handled in assignment")
                else:
                    raise RuntimeError("non callable called in ir generation")
            case ASTNodeIf():
                raise NotImplementedError()
            case ASTNodeWhile():
                loop_head = self.new_label(node.token.location, "while_head")
                loop_body = self.new_label(node.token.location, "while_body")
                loop_end = self.new_label(node.token.location, "while_end")
                self.add_instruction(loop_head)

                cond = self.ast_node_to_ir(node.condition, scope)
                assert cond is not None
                self.add_instruction(IRInstJumpNotZero(node.token.location, cond, loop_end.name, loop_body.name))

                self.add_instruction(loop_body)
                assert node.elaborated_body is not None
                self.lower_scope(node.elaborated_body, [])

                self.add_instruction(IRInstJump(node.token.location, loop_head.name))
                self.add_instruction(loop_end)
                return None
            case _:
                raise NotImplementedError()

    class MoveArguments:
        def __init__(self):
            pass

        def __call__(self, scope, instructions):
            pass

    def lower_scope(self, scope: Scope, function_arguments: List[Name]):
        for record in scope.record_types.values():
            self.n_temporary += 1
            ir_record = IRRecord(
                name = f"{self.n_temporary}_{record.name}",
                fields = [],
                size = record.get_size(),
            )
            self.unit.records[record] = ir_record
            ir_record.fields = list(map(lambda x: self.type_to_ir_type_not_none(x.type), record.fields.values()))

        for name, procedure in scope.procedures.items():
            self.n_temporary += 1
            arguments = dict(zip(map(lambda x: x.name, procedure.argument_names),
                                 map(lambda x: self.type_to_ir_type_not_none(x.type), procedure.argument_names)))
            return_type_ast = self.type_table.get(name.type)
            assert isinstance(return_type_ast, TypeProcedure)
            self.unit.procedures[name] = IRProcedure(
                name = f"{self.n_temporary}_{name.name}",
                arguments = arguments,
                return_type = self.type_to_ir_type(return_type_ast.return_type),
                alloc_instructions=[],
                instructions= [],
            )

        for ir_proc, procedure in zip(self.unit.procedures.values(), scope.procedures.values()):
            assert procedure.elaborated_body is not None
            self.current_procedures.append(ir_proc)
            self.lower_scope(procedure.elaborated_body, procedure.argument_names)
            self.current_procedures.pop()

        for s_name, o_name in scope.names.items():
            if self.type_table.get(o_name.type).info.group in [TypeGroup.PROCEDURE, TypeGroup.TYPE]:
                continue
            self.n_temporary += 1
            dest_type = self.type_to_ir_type_not_none(o_name.type)
            dest = Register(f"{self.n_temporary}_{s_name}", IRTypePointer(PLATFORM_POINTER_SIZE))
            if isinstance(ri := self.type_table.get(o_name.type), TypeRecordInstance):
                align = ri.record.get_alignment()
            else:
                align = dest_type.size
            self.add_allocation(IRInstAllocate(o_name.declaration_location, dest, dest_type.size, align))
            self.variable_stack_register[o_name] = dest

        for name in function_arguments:
            dest = self.variable_stack_register[name]
            dest_type = self.type_to_ir_type_not_none(name.type)
            src = Register(f"{name.name}", dest_type)
            if isinstance(dest.type, IRTypeRecord):
                self.add_instruction(IRInstMemcpy(name.declaration_location, dest, src, dest.type.size))
            else:
                self.add_instruction(IRInstStore(name.declaration_location, dest, src))

        for node in scope.body:
            self.ast_node_to_ir(node, scope)