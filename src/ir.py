from copy import copy
from typing import cast, Callable

from ast_types import ASTNode, ASTNodeValue, ASTNodeBinary, ASTNodeUnary, ASTNodeAssignment, ASTNodeUnaryRight, \
    ASTNodeStatement, ASTNodeCast, ASTNodeTransmute, ASTNodeCall, ASTNodeIf, ASTNodeWhile, ASTNodeScope, \
    ASTNodeArrayAccess, ASTNodeTupleLike
from elaboration_types import Scope, Name, Record
from ir_types import *
from lexer_types import TokenNumberLiteral, TokenName, TokenStringLiteral, TokenBoolLiteral, Operator, Keyword, \
    TokenKeyword, TokenBracket, BracketType
from typing_types import PLATFORM_POINTER_SIZE, PLATFORM_BOOL_SIZE, \
    TypeProcedure, TypePointer, TypeNoValue, TypeInt, TypeUInt, TypeFloat, TypeBool, TypeRecord, TypeType, TypeReturns, \
    TypeFixedSizeArray, PLATFORM_INT_SIZE, Type

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

no_location = CodeLocation("phony", 0, 0, 0, 1)


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
        if isinstance(t, TypeNoValue):
            return None
        if isinstance(t, TypeFixedSizeArray):
            item_type = self.type_to_ir_type(t.base_type)
            assert item_type is not None
            if isinstance(item_type, IRTypeArray):
                align = item_type.align
            elif isinstance(item_type, IRTypeRecord):
                align = item_type.record.align
            else:
                align = item_type.size
            residual = item_type.size % align
            size = item_type.size + ((align - residual) if residual > 0 else 0)
            array_size = t.n_elements * size
            array_type = IRTypeArray(array_size, t.n_elements, item_type, align)
            t.size = array_size
            self.unit.fixed_size_arrays.add(array_type)
            return array_type
        assert t.size is not None, "IR types can not be zero sized except NO_VALUE"
        match t:
            case TypeInt():
                return IRTypeInt(t.size)
            case TypeUInt():
                return IRTypeUInt(t.size)
            case TypeFloat():
                return IRTypeFloat(t.size)
            case TypeBool():
                return IRTypeUInt(PLATFORM_BOOL_SIZE)
            case TypePointer():
                return IRTypePointer(PLATFORM_POINTER_SIZE)
            case TypeRecord():
                record = self.unit.records.get(t.record)
                assert record is not None
                return IRTypeRecord(t.size, record)
            case TypeProcedure():
                return IRTypePointer(PLATFORM_POINTER_SIZE)
            case TypeType():
                raise NotImplementedError()
            case TypeReturns():
                raise RuntimeError("Lowering to IR should not encounter RETURNS type")
            case _:
                raise RuntimeError(f"Un-handled type {t}")

    def type_to_ir_type_not_none(self, tt: int) -> IRType:
        rv = self.type_to_ir_type(tt)
        assert (rv is not None)
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
                    return self.get_ptr_to_field(scope, l_value)
                case ASTNodeUnaryRight(token=tok) if tok.op == Operator.POINTER:
                    # ast_node_to_ir will automatically load any variables (i.e. dereference them)
                    rv = self.ast_node_to_ir(l_value.child, scope)
                    assert isinstance(rv, Register)
                    return rv
                case ASTNodeArrayAccess():
                    return self.get_ptr_to_element(scope, l_value)
                case _:
                    raise RuntimeError(f"Unexpected node in l_value")

    def get_ptr_to_field(self, scope: Scope, node: ASTNodeBinary) -> Register:
        base = self.ast_node_to_ir(node.left, scope)
        assert isinstance(base, Register)
        assert isinstance(base.type, IRTypePointer) or isinstance(base.type, IRTypeRecord)
        lt = self.type_table.get(node.left.type)
        if isinstance(lt, TypePointer):
            lt = self.type_table.get(lt.target_type)
        assert isinstance(lt, TypeRecord)
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
            lt.record.get_size(),
            offset,
            field_nr
        ))
        return ptr_dest

    def get_ptr_to_element(self, scope: Scope, node: ASTNodeArrayAccess) -> Register:
        base = self.ast_node_to_ir(node.array, scope)
        assert isinstance(base, Register)

        array_type = self.type_table.get(node.array.type)
        if isinstance(array_type, TypePointer):
            array_type = self.type_table.get(array_type.target_type)
        assert isinstance(array_type, TypeFixedSizeArray)

        index = self.ast_node_to_ir(node.index, scope)
        assert index is not None

        return self.get_ptr_to_index(node.token.location, array_type, base, index)

    def get_ptr_to_index(self, location: CodeLocation, array_type: TypeFixedSizeArray, base: Register,
                         index: Register | Immediate) -> Register:
        assert isinstance(base.type, IRTypePointer) or isinstance(base.type, IRTypeArray)
        assert isinstance(index.type, IRTypeUInt) and index.type.size is PLATFORM_POINTER_SIZE

        array_item_type = self.type_table.get(array_type.base_type)
        item_size = array_item_type.size
        assert item_size is not None
        if isinstance(array_item_type, TypeRecord):
            # pad size to ensure alignment
            alignment = array_item_type.record.get_alignment()
            residual = item_size % alignment
            item_size = item_size + ((alignment - residual) if alignment else 0)

        ptr_dest = self.new_temporary(IRTypePointer(PLATFORM_POINTER_SIZE))
        self.add_instruction(IRInstGetElementPointer(location, ptr_dest, base, index, item_size, 0, 0))
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
                        match self.type_table.get(nn.type):
                            case TypeRecord() | TypeFixedSizeArray():
                                # if it is a record we only want the address
                                return ptr
                            case _:
                                # for values, we want to load from stack
                                temp = self.new_temporary(nn.type)
                                self.add_instruction(IRInstLoad(node.token.location, temp, ptr))
                                return temp
                    case TokenStringLiteral():
                        if node.token.zero_terminated:
                            self.n_temporary += 1
                            self.unit.data_literals.append((str(self.n_temporary), node.token.content))
                            return Immediate(str(self.n_temporary), IRTypePointer(PLATFORM_POINTER_SIZE))
                        else:
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
                if tok.op == Operator.OR:
                    self.add_instruction(
                        IRInstJumpNotZero(node.token.location, op1, second_condition.name, phi_label.name))
                else:
                    self.add_instruction(
                        IRInstJumpNotZero(node.token.location, op1, phi_label.name, second_condition.name))
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
                ptr_dest = self.get_ptr_to_field(scope, node)
                if isinstance(self.type_table.get(node.type), TypeRecord):
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
            case ASTNodeUnary(token=tok) if tok.op in (Operator.ADDRESS_OFF, Operator.ADDRESS_OFF_MUTABLE):
                match node.child:
                    case ASTNodeValue():
                        assert isinstance(node.child.token, TokenName)
                        address_name = scope.lookup(node.child.token.name)
                        assert address_name is not None
                        return self.variable_stack_register[address_name]
                    case ASTNodeAssignment():
                        _ = self.ast_node_to_ir(node.child, scope)
                        assert isinstance(node.child.target, Name)
                        return self.variable_stack_register[node.child.target]
                    case ASTNodeBinary() if node.child.token.op == Operator.DOT:
                        return self.get_ptr_to_field(scope, node.child)
                    case ASTNodeArrayAccess():
                        return self.get_ptr_to_element(scope, node.child)
                    case _:
                        raise RuntimeError("Compiler Error can not take address of this")
            case ASTNodeUnary():
                op1 = self.ast_node_to_ir(node.child, scope)
                assert op1 is not None
                match node.token.op:
                    case Operator.BITWISE_NOT:
                        dest = self.new_temporary(node.type)
                        assert isinstance(dest.type, (IRTypeInt, IRTypeUInt))
                        self.add_instruction(IRInstXor(node.token.location, dest, op1,
                                                       Immediate(-1, op1.type)))
                        return dest
                    case Operator.MINUS:
                        dest = self.new_temporary(node.type)
                        self.add_instruction(IRInstNegative(node.token.location, dest, op1))
                        return dest
                    case Operator.PLUS:
                        return op1
                    case Operator.NOT:
                        dest = self.new_temporary(node.type)
                        self.add_instruction(IRInstEqual(node.token.location, dest, op1,
                                                         Immediate(0, op1.type)))
                        return dest
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
                match node.expression:
                    case ASTNodeCall() if (record := scope.lookup_record(node.expression.procedure)) is not None:
                        argument_registers = [self.ast_node_to_ir(a, scope) for a in node.expression.arguments]
                        dest_base = self.l_value_to_ir(node.target, scope)
                        for i, (reg, offset, filed) in enumerate(
                                zip(argument_registers, record.get_offsets().values(), record.fields.values())):
                            assert reg is not None
                            dest = self.new_temporary(IRTypePointer(PLATFORM_POINTER_SIZE))
                            self.add_instruction(IRInstGetElementPointer(node.token.location, dest, dest_base,
                                                                         Immediate(0,
                                                                                   IRTypeUInt(PLATFORM_POINTER_SIZE)),
                                                                         record.get_size(),
                                                                         offset, i))
                            self.store_value(reg, dest, filed.type, node.token.location)
                        return dest_base
                    case ASTNodeTupleLike(token=bracket) if (isinstance(bracket, TokenBracket) and
                                                             bracket.type == BracketType.SQUARE and
                                                             node.expression.parent is None):
                        item_registers = [self.ast_node_to_ir(a, scope) for a in node.expression.children]
                        dest_base = self.l_value_to_ir(node.target, scope)
                        array_type = self.type_table.get(node.expression.type)
                        assert isinstance(array_type, TypeFixedSizeArray)
                        item_type = self.type_table.get(array_type.base_type)
                        for i, item in enumerate(item_registers):
                            assert item is not None
                            dest = self.get_ptr_to_index(node.token.location, array_type, dest_base,
                                                         Immediate(i, IRTypeUInt(PLATFORM_POINTER_SIZE)))
                            self.store_value(item, dest, item_type, node.token.location)
                        return dest_base
                    case _:
                        value = self.ast_node_to_ir(node.expression, scope)
                        dest = self.l_value_to_ir(node.target, scope)
                        assert value is not None
                        assert isinstance(dest, Register)
                        self.store_value(value, dest, node.target.type, node.token.location)
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
                assert not isinstance(tt, TypeRecord), "Should have been handled in assignment"
                assert isinstance(tt, TypeProcedure), "non callable called in ir generation"
                proc = self.unit.procedures[node.procedure]

                arguments = [self.ast_node_to_ir(arg, scope) for arg in node.arguments]
                assert all(map(lambda x: x is not None, arguments))
                args = cast(List[Register | Immediate], arguments)

                for reg_imm, elaborated_arg in zip(args, node.arguments):
                    jtl_type = self.type_table.get(elaborated_arg.type)
                    match jtl_type:
                        case TypeFixedSizeArray() | TypeRecord():
                            reg_imm.type = self.type_to_ir_type_not_none(elaborated_arg.type)
                        case _:
                            pass

                args_with_vararg_marker = cast(List[Register | Immediate | VarArgMarker], args)
                if tt.varargs:
                    args_with_vararg_marker.insert(len(tt.arguments), VarArgMarker())

                match self.type_table.get(tt.return_type):
                    case TypeNoValue():
                        self.add_instruction(IRInstCall(node.token.location, proc, None, args_with_vararg_marker))
                        return None
                    case TypeRecord() | TypeFixedSizeArray():
                        return_register = self.new_temporary(self.type_to_ir_type_not_none(tt.return_type))
                        self.add_instruction(
                            IRInstCall(node.token.location, proc, return_register, args_with_vararg_marker))
                        return return_register
                    case _:
                        assert proc.return_type is not None
                        return_register = self.new_temporary(proc.return_type)
                        self.add_instruction(
                            IRInstCall(node.token.location, proc, return_register, args_with_vararg_marker))
                        return return_register
            case ASTNodeIf():
                return_type = self.type_table.get(node.type)
                if isinstance(return_type, TypeNoValue):
                    if_body = self.new_label(node.token.location, "if_body")
                    if node.else_body is not None:
                        if_else = self.new_label(node.token.location, "if_else")
                    if_end = self.new_label(node.token.location, "if_end")
                    cond = self.ast_node_to_ir(node.condition, scope)
                    assert cond is not None
                    self.add_instruction(IRInstJumpNotZero(node.token.location, cond,
                                                           if_else.name if node.else_body is not None else if_end.name,
                                                           if_body.name))

                    self.add_instruction(if_body)
                    # function works if this is false but violates mental model
                    assert isinstance(node.body, ASTNodeScope)
                    self.ast_node_to_ir(node.body, scope)
                    self.add_instruction(IRInstJump(node.token.location, if_end.name))
                    if node.else_body is not None:
                        self.add_instruction(if_else)
                        assert isinstance(node.else_body, ASTNodeScope)
                        self.ast_node_to_ir(node.else_body, scope)
                        self.add_instruction(IRInstJump(node.token.location, if_end.name))
                    self.add_instruction(if_end)
                    return None
                else:
                    assert node.else_body is not None
                    if_true = self.new_label(node.token.location, "if_true")
                    if_false = self.new_label(node.token.location, "if_false")
                    if_end = self.new_label(node.token.location, "if_end")
                    cond = self.ast_node_to_ir(node.condition, scope)
                    assert cond is not None
                    self.add_instruction(IRInstJumpNotZero(node.token.location, cond, if_false.name, if_true.name))
                    self.add_instruction(if_true)
                    true_value = self.ast_node_to_ir(node.body, scope)
                    self.add_instruction(IRInstJump(node.token.location, if_end.name))
                    self.add_instruction(if_false)
                    false_value = self.ast_node_to_ir(node.else_body, scope)
                    self.add_instruction(IRInstJump(node.token.location, if_end.name))
                    self.add_instruction(if_end)
                    assert true_value is not None
                    assert false_value is not None
                    rv = self.new_temporary(node.type)
                    self.add_instruction(IRInstPHI(node.token.location, rv, if_true.name, if_false.name, true_value,
                                                   false_value))
                    return rv
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
            case ASTNodeScope():
                assert node.elaborated_body is not None
                self.lower_scope(node.elaborated_body, [])
                return None
            case ASTNodeArrayAccess():
                element_ptr = self.get_ptr_to_element(scope, node)
                element_type = self.type_to_ir_type_not_none(node.type)
                if isinstance(element_type, (IRTypeRecord, IRTypeArray)):
                    return element_ptr
                dest = self.new_temporary(element_type)
                self.add_instruction(IRInstLoad(node.get_location(), dest, element_ptr))
                return dest
            case _:
                raise NotImplementedError()

    def lower_scope(self, scope: Scope, function_arguments: List[Name]):
        for record in scope.record_types.values():
            self.n_temporary += 1

            # until we get packed structs just pad struct size to match alignment (for arrays)
            alignment = record.get_alignment()
            size = record.get_size() + alignment - (record.get_size() % alignment)

            ir_record = IRRecord(
                name=f"{self.n_temporary}_{record.name}",
                align=alignment,
                fields=[],
                size=size,
            )
            self.unit.records[record] = ir_record
            ir_record.fields = list(map(lambda x: self.type_to_ir_type_not_none(x.type), record.fields.values()))

        for name, procedure_stub in scope.procedure_stubs.items():
            arguments = dict(zip(map(lambda x: x.name, procedure_stub.argument_names),
                                 map(lambda x: self.type_to_ir_type_not_none(x.type), procedure_stub.argument_names)))
            return_type_ast = self.type_table.get(name.type)
            assert isinstance(return_type_ast, TypeProcedure)
            self.unit.procedures[name] = IRProcedure(
                name=name.name,
                arguments=arguments,
                location=name.declaration_location,
                return_type=self.type_to_ir_type(return_type_ast.return_type),
                stub=True,
                alloc_instructions=[],
                instructions=[],
                export=True
            )

        for name, procedure in scope.procedures.items():
            self.n_temporary += 1
            arguments = dict(zip(map(lambda x: x.name, procedure.argument_names),
                                 map(lambda x: self.type_to_ir_type_not_none(x.type), procedure.argument_names)))
            return_type_ast = self.type_table.get(name.type)
            assert isinstance(return_type_ast, TypeProcedure)
            # FIXME: a more canonical way to declare a function exported
            if name.name == "main":
                assert scope.parent is None, "Main must be defined in top scope"
                proc_name = "main"
                proc_export = True
            else:
                proc_name = f"{self.n_temporary}_{name.name}"
                proc_export = False
            self.unit.procedures[name] = IRProcedure(
                name=proc_name,
                arguments=arguments,
                location=name.declaration_location,
                return_type=self.type_to_ir_type(return_type_ast.return_type),
                stub=False,
                alloc_instructions=[],
                instructions=[],
                export=proc_export
            )

        for name, procedure in scope.procedures.items():
            assert procedure.elaborated_body is not None
            self.current_procedures.append(self.unit.procedures[name])
            self.lower_scope(procedure.elaborated_body, procedure.argument_names)
            self.current_procedures.pop()

        for s_name, o_name in scope.names.items():
            if isinstance(self.type_table.get(o_name.type), (TypeProcedure, TypeType)):
                continue
            self.n_temporary += 1
            dest_type = self.type_to_ir_type_not_none(o_name.type)
            dest = Register(f"{self.n_temporary}_{s_name}", IRTypePointer(PLATFORM_POINTER_SIZE))

            match dest_type:
                case IRTypeRecord():
                    record_type = self.type_table.get(o_name.type)
                    assert isinstance(record_type, TypeRecord)
                    align = record_type.record.get_alignment()
                case IRTypeArray():
                    align = dest_type.align
                case _:
                    align = dest_type.size
            self.add_allocation(IRInstAllocate(o_name.declaration_location, dest, dest_type.size, align))
            self.variable_stack_register[o_name] = dest

        for name in function_arguments:
            dest = self.variable_stack_register[name]
            dest_type = self.type_to_ir_type_not_none(name.type)
            if isinstance(dest_type, (IRTypeArray, IRTypeRecord)):
                dest_type = IRTypePointer(PLATFORM_POINTER_SIZE)
            src = Register(f"{name.name}", dest_type)
            self.store_value(src, dest, name.type, name.declaration_location)

        for node in scope.body:
            self.ast_node_to_ir(node, scope)

        return None

    def store_value(self, src: Register | Immediate, dest: Register, dest_type: int | Type, location: CodeLocation):
        if isinstance(dest_type, int):
            dest_type = self.type_table.get(dest_type)
        match dest_type:
            case TypeFixedSizeArray() | TypeRecord():
                assert isinstance(dest.type, IRTypePointer)
                size = dest_type.size
                assert size is not None
                self.add_instruction(IRInstMemcpy(location, dest, src, size))
            case _:
                self.add_instruction(IRInstStore(location, dest, src))
