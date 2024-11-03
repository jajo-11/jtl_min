from dataclasses import dataclass, field
from enum import Enum
from typing import List, Dict, Tuple

from ast_types import ASTNode, ASTNodeStatement, ASTNodeBinary, ASTNodeAssignment, ASTNodeUnary, ASTNodeValue
from elaboration_types import Scope
from lexer_types import CodeLocation, TokenKeyword, Keyword, TokenOperator, Operator, TokenNumberLiteral, TokenName, \
    TokenStringLiteral, TokenBoolLiteral
from typing_types import Type, TypeGroup, TypeTable


@dataclass
class IRType:
    width: int

class IRTypeInt(IRType):
    pass

class IRTypeFloat(IRType):
    pass

class IRTypeUInt(IRType):
    pass


class IROp(Enum):
    ADD = "add"
    SUB = "sub"
    MUL = "mul"
    DIV = "div"
    REM = "rem"
    AND = "and"
    OR = "or"
    NEG = "neg"
    CALL = "call"
    RET = "ret"
    ALLOC = "alloc"
    DEALLOC = "dealloc"


@dataclass
class MemoryLocation:
    name: str


@dataclass
class VirtualRegister:
    name: str
    type: IRType


@dataclass
class Label:
    name: str


@dataclass
class Immediate:
    value: int | float
    type: IRType


@dataclass
class IRInstruction:
    instruction: IROp
    location: CodeLocation


@dataclass
class IRInstArithmetic(IRInstruction):
    dest: VirtualRegister
    op1: VirtualRegister | MemoryLocation | Immediate
    op2: VirtualRegister | MemoryLocation | Immediate

@dataclass
class IRInstrUnary(IRInstruction):
    dest: VirtualRegister
    op1: VirtualRegister | MemoryLocation | Immediate


@dataclass
class IRInstrCall(IRInstruction):
    dest: VirtualRegister
    function: str
    arguments: List[VirtualRegister | MemoryLocation | Immediate]


@dataclass
class IRInstJump(IRInstruction):
    dest: Label


@dataclass
class IRProcedure:
    name: str
    arguments: List[VirtualRegister]
    instructions: List[IRInstruction]


@dataclass
class IRUnit:
    current_procedure: IRProcedure
    type_table: TypeTable
    data_section: Dict[MemoryLocation, str] = field(default_factory=dict)
    memory_location_names: Dict[str, int] = field(default_factory=dict)
    virtual_register_names: Dict[str, int] = field(default_factory=dict)
    label_names: Dict[str, int] = field(default_factory=dict)
    procedures: List[IRProcedure] = field(default_factory=list)

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.procedures.append(self.current_procedure)

    def _new_name[T](self, name: str, field_name: str, return_class: T) -> T:
        if (count := getattr(self, field_name).get(name, None)) is None:
            count += 1
        else:
            count = 0
        setattr(self, field_name, count)
        return return_class(name + count)

    def type_to_ir_type(self, tt: int) -> IRType:
        t = self.type_table.get(tt)
        match t.info.group:
            case TypeGroup.INT:
                return IRTypeInt(t.info.size)
            case TypeGroup.FLOAT:
                return IRTypeFloat(t.info.size)
            case TypeGroup.UINT:
                return IRTypeUInt(t.info.size)
            case _:
                raise NotImplementedError()

    def new_memory_location(self, name: str) -> MemoryLocation:
        return self._new_name(name, "memory_location_names", MemoryLocation)

    def new_virtual_register(self, name: str) -> VirtualRegister:
        return self._new_name(name, "virtual_register_names", VirtualRegister)

    def new_label(self, name: str) -> Label:
        return self._new_name(name, "label_names", Label)

    def parse_expr(self, name: str, expr: ASTNode, scope: Scope) -> None | VirtualRegister | MemoryLocation | Immediate:
        match expr:
            case ASTNodeStatement():
                match expr.token:
                    case TokenKeyword(Keyword.LET) | TokenKeyword(Keyword.VARIABLE):
                        return self.parse_expr("", expr.value)
                    case _:
                        raise NotImplementedError()
            case ASTNodeAssignment():
                rv = self.parse_expr(expr.name.name, expr.expression, scope)
                if isinstance(rv, Immediate):
                    expr.name.name_iteration = rv
                elif isinstance(rv, VirtualRegister) or isinstance(rv, MemoryLocation):
                    expr.name.name_iteration = self.current_procedure.instructions[-1].dest
                else:
                    raise RuntimeError(f'Compiler Error: assignment must get something')
                return rv
            case ASTNodeUnary():
                match expr.token:
                    case TokenOperator(Operator.PLUS):
                        return self.parse_expr(name, expr.child, scope)
                    case TokenOperator(Operator.MINUS):
                        rv = self.parse_expr(name, expr.child, scope)
                        if isinstance(rv, Immediate):
                            rv.value = -rv.value
                            return rv
                        elif isinstance(rv, VirtualRegister) or isinstance(rv, MemoryLocation):
                            dest = self.new_virtual_register(name)
                            self.current_procedure.instructions.append(IRInstrUnary(
                                IROp.SUB,
                                expr.token.location,
                                dest,
                                rv,
                                self.type_to_ir_type(expr.type)
                            ))
                        else:
                            raise RuntimeError(f'Compiler Error: unary operator must operate on something')
            case ASTNodeBinary():
                op1 = self.parse_expr(name, expr.left, scope)
                op2 = self.parse_expr(name, expr.right, scope)
                match expr.token:
                    case TokenOperator(Operator.PLUS):
                        if isinstance(op1, Immediate) and isinstance(op2, Immediate):
                            return Immediate(op1.value + op2.value, self.type_to_ir_type(expr.type))
                        op = IROp.ADD
                    case TokenOperator(Operator.MINUS):
                        if isinstance(op1, Immediate) and isinstance(op2, Immediate):
                            return Immediate(op1.value - op2.value, self.type_to_ir_type(expr.type))
                        op = IROp.SUB
                    case TokenOperator(Operator.TIMES):
                        if isinstance(op1, Immediate) and isinstance(op2, Immediate):
                            return Immediate(op1.value * op2.value, self.type_to_ir_type(expr.type))
                        op = IROp.MUL
                    case TokenOperator(Operator.DIVIDE):
                        if isinstance(op1, Immediate) and isinstance(op2, Immediate):
                            return Immediate(op1.value / op2.value, self.type_to_ir_type(expr.type))
                        op = IROp.DIV
                    case TokenOperator(Operator.MODULO):
                        if isinstance(op1, Immediate) and isinstance(op2, Immediate):
                            return Immediate(op1.value % op2.value, self.type_to_ir_type(expr.type))
                        op = IROp.REM
                    case _:
                        raise NotImplementedError()
                dest = self.new_virtual_register(name)
                self.current_procedure.instructions.append(IRInstArithmetic(
                    op,
                    expr.token.location,
                    dest,
                    op1,
                    op2,
                    self.type_to_ir_type(expr.type)
                ))
                return dest
            case ASTNodeValue():
                match expr.token:
                    case TokenNumberLiteral():
                        return Immediate(expr.token.value)
                    case TokenName():
                        n = scope.lookup(expr.token.name)
                        assert n is not None
                        return n.name_iteration
                    case TokenStringLiteral():
                        ml = self.new_memory_location(name)
                        self.data_section[ml] = expr.token.content
                        return ml
                    case TokenBoolLiteral():
                        return Immediate(1 if expr.token.value else 0)
                    case _:
                        raise RuntimeError(f'Compiler Error: value should not be here in ir pass')
