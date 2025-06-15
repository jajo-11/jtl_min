from typing import Tuple

from ast_types import *
from elaboration_types import Scope, Name, Record
from typing_types import *
from errors import ElaborationError, ElaborationErrorType, JTLTypeError, JTLTypeErrorType, get_error_with_line_info
from typing_types import TypeUInt, TypeFloat, TypeBool, TypePointer


def elaborate_module(nodes: List[ASTNode]) -> Scope:
    scope = Scope(TypeTable())
    _ = elaborate_scope(scope, nodes, None)

    # take literal types whose types could not be inferred and convert them to discrete types
    fixed_arrays: List[TypeFixedSizeArray] = []
    for i, t in enumerate(scope.type_table.table):
        if i == 0:  # skip sentinel
            continue
        if isinstance(t, Type) and t.size is None:
            match t:
                case TypeType() | TypeNoValue() | TypeReturns():
                    pass
                case TypeUndefined():
                    raise RuntimeError("Found unresolved type in Type Table")
                case TypeInt():
                    scope.type_table.overwrite(i, BuildInType.I64)
                case TypeUInt():
                    # unless the - unary is used all int literals start out as UINTs converting them to U64 would
                    # probably be surprising to the user when doing a - b etc. so if no information is available they
                    # get converted to I64
                    scope.type_table.overwrite(i, BuildInType.I64)
                case TypeFloat():
                    scope.type_table.overwrite(i, BuildInType.F64)
                case TypeFixedSizeArray():
                    fixed_arrays.append(t)
                case _:
                    raise RuntimeError(f"Found type {t} with none size in Type Table")

    # FIXME: nested fixed_arrays can break this quiet easily
    # FIXME: size calculation is off if size is not multiple of alignment
    for fa in fixed_arrays:
        child_size = fa.type_table.get(fa.base_type).size
        assert child_size is not None
        fa.size = fa.n_elements * child_size

    return scope


def elaborate_scope(parent: Scope, nodes: List[ASTNode], returnable: Optional[ASTNodeProcedure]) -> bool:
    """Returns True if scope ends on a return"""
    non_constant_nodes: List[Tuple[int, ASTNode]] = []
    constant_nodes: List[ASTNodeStatement] = []
    records: List[Tuple[Record, ASTNodeRecord]] = []

    # records need to be elaborated first so that field access can be validated

    # first get all the names of records
    for i, node in enumerate(nodes):
        if isinstance(node, ASTNodeStatement) and isinstance(node.token,
                                                             TokenKeyword) and node.token.keyword == Keyword.CONSTANT:
            assert node.child is not None
            if (isinstance(node.child, ASTNodeBinary) and isinstance(node.child.token, TokenOperator)
                and node.child.token.op == Operator.ASSIGNMENT) and isinstance(node.child.right, ASTNodeRecord):
                left = node.child.left
                if not (isinstance(left, ASTNodeValue) and isinstance(left.token, TokenName)):
                    raise ElaborationError.from_type(ElaborationErrorType.EXPECTED_NAME_DECLARATION,
                                                     left.token.location)
                record = Record(parent.unique_indexer.next(), node.child.right, left.token.name)
                record_instance_type = parent.type_table.new(TypeRecord(None, record))
                record_type = parent.type_table.new(TypeType(record_instance_type, parent.type_table))
                name = Name(
                    parent.unique_indexer.next(),
                    left.token.name,
                    left.token.location,
                    record_type,
                    parent.type_table)
                parent.names[left.token.name] = name
                parent.record_types[name] = record
                records.append((record, node.child.right))
                node.child.right.type = record_type
            else:
                constant_nodes.append(node)
        else:
            non_constant_nodes.append((i, node))

    # second elaborate the records
    for record, node in records:
        elaborate_record(record, parent, node.body)

    # third update size of record types (needed to elaborate the records first)
    # this also performs a cyclic dependency check
    for record, node in records:
        ntt = parent.type_table.get(node.type)
        assert isinstance(ntt, TypeType)
        nt = parent.type_table.get(ntt.instance_type)
        nt.size = record.get_size()

    # fourth process the constants - calls to procedures are not allowed for now (Constructors?)
    for node in constant_nodes:
        assert node.child is not None
        expr = elaborate_declaration(parent, node.child, True, returnable)
        assert isinstance(expr.target, Name)
        expr.target.mut = Mutability.CONSTANT
        # TODO these should be evaluated at compile time and added to a separate memory section (data)
        if not isinstance(expr.expression, (ASTNodeProcedure, ASTNodeProcedureStub)):
            parent.body.append(expr)

    # fifth all other expressions - procedure bodies are deferred
    for i, node in non_constant_nodes:
        match node:
            case ASTNodeStatement():
                match node.token:
                    case TokenKeyword(keyword=Keyword.VARIABLE):
                        assert (node.child is not None)
                        expr = elaborate_declaration(parent, node.child, False, returnable)
                        assert isinstance(expr.target, Name)
                        expr.target.mut = Mutability.MUTABLE
                        parent.body.append(expr)
                    case TokenKeyword(keyword=Keyword.LET):
                        assert (node.child is not None)
                        expr = elaborate_declaration(parent, node.child, False, returnable)
                        assert isinstance(expr.target, Name)
                        expr.target.mut = Mutability.ONCE
                        parent.body.append(expr)
                    case TokenKeyword(keyword=Keyword.CONSTANT):
                        assert False, "These should have been handled already"
                    case _:
                        parent.body.append(elaborate_expression(parent, node, False, returnable))
            case _:
                parent.body.append(elaborate_expression(parent, node, False, returnable))

        last_node_type = parent.type_table.get(parent.body[-1].type)
        if isinstance(last_node_type, TypeReturns) and i != (len(nodes) - 1):
            raise ElaborationError.from_type(ElaborationErrorType.UNREACHABLE, nodes[-1].get_location())
        if i == (len(nodes) - 1) and not isinstance(last_node_type, TypeReturns) and returnable is not None:
            return False

    # sixth procedures are elaborated last so that they can refer to them self
    # we can deal with all procedures the same way as they define all externally relevant stuff in their signature
    # records on the other hand can not be handled in the same way as fields have to be known during parsing, and those
    # fields can depend on types that are defined later...
    for name, proc in parent.procedures.items():
        proc.elaborated_body = Scope(parent)
        for n in proc.argument_names:
            proc.elaborated_body.names[n.name] = n
        if not elaborate_scope(proc.elaborated_body, proc.body, proc) and proc.return_type_expr is not None:
            raise ElaborationError.from_type(ElaborationErrorType.MISSING_RETURN, proc.get_location(),
                                             name.name)

    return True


def elaborate_record(record: Record, parent: Scope, nodes: List[ASTNode]):
    for node in nodes:
        if not isinstance(node, ASTNodeStatement):
            raise ElaborationError.from_type(ElaborationErrorType.NOT_A_DECLARATION, node.get_location())
        assert node.child is not None
        match node.token:
            case TokenKeyword(keyword=Keyword.VARIABLE):
                n = elaborate_name_and_type(parent, node.child)
                n.mut = Mutability.MUTABLE
                record.fields[n.name] = n
            case TokenKeyword(keyword=Keyword.LET):
                n = elaborate_name_and_type(parent, node.child)
                n.mut = Mutability.ONCE
                record.fields[n.name] = n
            case TokenKeyword(keyword=Keyword.CONSTANT):
                raise NotImplementedError()
            case _:
                raise ElaborationError.from_type(ElaborationErrorType.NOT_A_DECLARATION,
                                                 node.get_location())


def elaborate_declaration(parent: Scope, node: ASTNode, constant: bool, returnable: Optional[ASTNodeProcedure]
                          ) -> ASTNodeAssignment:
    # returnable is always unused as it returns nothing and therefore can not be assigned, but I prefer getting a
    # no value error than there is nothing to return from error. (as that might not be true)
    if not (isinstance(node, ASTNodeBinary) and isinstance(node.token, TokenOperator)
            and node.token.op == Operator.ASSIGNMENT):
        raise ElaborationError.from_type(ElaborationErrorType.NO_ASSIGNMENT_DECLARATION,
                                         node.get_location())
    n = elaborate_name_and_type(parent, node.left)
    e = elaborate_expression(parent, node.right, constant, returnable, direct_assignment=True)

    e_type = parent.type_table.get(e.type)

    if isinstance(e_type, TypeNoValue):
        raise ElaborationError.from_type(ElaborationErrorType.EXPECTED_VALUE, node.get_location())

    if not narrow_type(parent.type_table, n.type, e.type):
        raise JTLTypeError.from_type(JTLTypeErrorType.TYPE_MISSMATCH_DECLARATION, node.get_location(),
                                     parent.type_table.get(n.type), parent.type_table.get(e.type))
    parent.names[n.name] = n

    if isinstance(e, ASTNodeProcedure):
        if constant:
            parent.procedures[n] = e
        else:
            uid = parent.unique_indexer.next()
            str_name = f"anonymous_procedure_{uid}"
            name = Name(
                uid,
                str_name,
                e.token.location,
                e.type,
                parent.type_table,
                Mutability.CONSTANT,
            )
            parent.names[str_name] = name
            parent.procedures[name] = e
            type_id = e.type
            e = ASTNodeValue(TokenName(e.token.location, str_name))
            e.type = type_id
    elif isinstance(e, ASTNodeProcedureStub):
        if constant:
            parent.procedure_stubs[n] = e
        else:
            raise ElaborationError.from_type(ElaborationErrorType.EXTERNAL_PROCEDURE_NOT_CONST, e.get_location())

    new_node = ASTNodeAssignment(node.token, node, n, e)
    new_node.type = n.type
    return new_node


def elaborate_name_and_type(parent: Scope, node: ASTNode) -> Name:
    """parent must be const (only use it for lookup and name index generation
    if you need to change this make sure you do not break elaborate_record (which uses it's parent scope)"""
    if isinstance(node, ASTNodeBinary) and node.token.op == Operator.COLON:
        if not (isinstance(node.left, ASTNodeValue) and isinstance(node.left.token, TokenName)):
            raise ElaborationError.from_type(ElaborationErrorType.EXPECTED_NAME_DECLARATION,
                                             node.get_location())
        return Name(parent.unique_indexer.next(), node.left.token.name, node.get_location(),
                    elaborate_type(parent, node.right), parent.type_table)
    elif isinstance(node, ASTNodeValue) and isinstance(node.token, TokenName):
        return Name(parent.unique_indexer.next(), node.token.name, node.get_location(),
                    parent.type_table.new(TypeUndefined()), parent.type_table)
    else:
        raise ElaborationError.from_type(ElaborationErrorType.EXPECTED_NAME_DECLARATION,
                                         node.get_location())


def elaborate_type(parent: Scope, node: ASTNode) -> int:
    """This will return an index to an instance type (i.e. will not wrap the type in a TypeType"""
    pointer_to_mutable = []
    while True:
        match node:
            case ASTNodeUnary() if node.token.op == Operator.POINTER:
                pointer_to_mutable.append(False)
                node = node.child
            case ASTNodeUnary() if node.token.op == Operator.MUTABLE_POINTER:
                pointer_to_mutable.append(True)
                node = node.child
            case ASTNodeValue() if isinstance(node.token, TokenBuildInType):
                base_type_id = parent.type_table.new_build_in_type(node.token.type)
                for mut in reversed(pointer_to_mutable):
                    base_type_id = parent.type_table.new(TypePointer(base_type_id, mut, parent.type_table))
                return base_type_id
            case ASTNodeValue() if isinstance(node.token, TokenName):
                name_obj = parent.lookup(node.token.name)
                if name_obj is None:
                    raise ElaborationError.from_type(ElaborationErrorType.UNDECLARED_NAME, node.get_location())
                base_type = parent.type_table.get(name_obj.type)
                if not isinstance(base_type, TypeType):
                    raise JTLTypeError.from_type(JTLTypeErrorType.TYPE_MISSMATCH_DECLARATION, node.get_location(),
                                                 base_type)
                base_type_id = base_type.instance_type
                for mut in reversed(pointer_to_mutable):
                    base_type_id = parent.type_table.new(TypePointer(base_type_id, mut, parent.type_table))
                return base_type_id
            case ASTNodeArrayType():
                # TODO this is currently simplified should accept more than one dimension and dimension as constant and not just immediate
                assert len(node.array_children) == 1
                array_size = node.array_children[0]
                array_size = elaborate_expression(parent, array_size, True, None, False)
                array_size_type = parent.type_table.get(array_size.type)
                if not (isinstance(array_size, ASTNodeValue) and isinstance(array_size.token, TokenNumberLiteral)
                        and isinstance(array_size.token.value, int)):
                    raise JTLTypeError.from_type(JTLTypeErrorType.EXPECTED_INT_IMMEDIATE, array_size.get_location(),
                                                 array_size_type)
                if array_size.token.value <= 0:
                    raise JTLTypeError.from_type(JTLTypeErrorType.EXPECTED_INT_IMMEDIATE, array_size.get_location())
                base_type_id = elaborate_type(parent, node.type_expression)
                base_type_id = parent.type_table.new(TypeFixedSizeArray(None, array_size.token.value, base_type_id,
                                                                        parent.type_table))
                for mut in reversed(pointer_to_mutable):
                    base_type_id = parent.type_table.new(TypePointer(base_type_id, mut, parent.type_table))
                return base_type_id
            case _:
                raise ElaborationError.from_type(ElaborationErrorType.EXPECTED_TYPE, node.get_location())


def is_numeric(t: Type) -> bool:
    return isinstance(t, (TypeInt, TypeUInt, TypeFloat))


def is_integer(t: Type) -> bool:
    return isinstance(t, (TypeInt, TypeUInt))


def resolve_field(parent: Scope, node: ASTNodeBinary) -> ASTNodeBinary:
    assert node.token.op == Operator.DOT
    node.left = lhs = elaborate_expression(parent, node.left, False, None)
    lhs_type = parent.type_table.get(lhs.type)
    if isinstance(lhs_type, TypePointer):
        lhs_type = parent.type_table.get(lhs_type.target_type)
    if not isinstance(lhs_type, TypeRecord):
        raise ElaborationError.from_type(ElaborationErrorType.EXPECTED_RECORD, lhs.token.location)
    if not (isinstance(node.right, ASTNodeValue) and isinstance(node.right.token, TokenName)):
        raise ElaborationError.from_type(ElaborationErrorType.EXPECTED_FIELD_NAME, node.right.token.location)
    name = node.right.token.name
    if (field_name := lhs_type.record.fields.get(name)) is not None:
        node.type = field_name.type
        node.mutable = field_name.mut == Mutability.MUTABLE
    else:
        raise ElaborationError.from_type(ElaborationErrorType.EXPECTED_FIELD_NAME, node.right.token.location)
    node.mutable = node.mutable and lhs.mutable
    return node


def elaborate_expression(parent: Scope, node: ASTNode, constant: bool,
                         returnable: Optional[ASTNodeProcedure], direct_assignment: bool = False) -> ASTNode:
    # direct_assignment shall be true when expression is the top most node to the right of an assignment
    # if direct_assignment is true anonymous records get a dummy name so that they get allocated on the stack later
    # anonymous procedures are also given a dummy name and are replaced with them
    match node:
        case ASTNodeBinary(token=tok) if tok.op == Operator.DOT:
            return resolve_field(parent, node)
        case ASTNodeBinary():
            node.left = elaborate_expression(parent, node.left, constant, returnable)
            node.right = elaborate_expression(parent, node.right, constant, returnable,
                                              node.token.op == Operator.ASSIGNMENT)
            lt = parent.type_table.get(node.left.type)
            rt = parent.type_table.get(node.right.type)
            match node.token.op:
                case Operator.PLUS | Operator.MINUS | Operator.TIMES | Operator.DIVIDE | Operator.MODULO:
                    if isinstance(lt, TypePointer) or isinstance(rt, TypeType):
                        raise JTLTypeError.from_type(JTLTypeErrorType.POINTER_ARITHMETIC,
                                                     node.token.location, node.token.op)
                    for e, et in ((node.left, lt), (node.right, rt)):
                        if not is_numeric(et):
                            raise JTLTypeError.from_type(JTLTypeErrorType.NOT_NUMERIC,
                                                         e.get_location(), et)
                    node.left, node.right = promote_either(parent.type_table, node.left, node.right)
                    node.type = node.left.type
                    return node
                case Operator.BITWISE_XOR | Operator.BITWISE_AND | Operator.BITWISE_XOR | Operator.BITWISE_OR:
                    if not (is_integer(lt) and is_integer(rt)):
                        raise JTLTypeError.from_type(JTLTypeErrorType.BIT_OPERATOR_ON_NON_INTEGER,
                                                     node.get_location(), str(node.token.op), str(lt),
                                                     str(node.token.op), str(rt))
                    if lt.size is None:
                        parent.type_table.overwrite(node.left.type, node.right.type)
                        lt = rt
                    elif rt.size is None:
                        parent.type_table.overwrite(node.right.type, node.left.type)
                        rt = lt
                    if lt.size != rt.size:
                        raise JTLTypeError.from_type(JTLTypeErrorType.BIT_OPERATOR_SIZE_MISSMATCH,
                                                     node.get_location(), lt.size, rt.size)
                    if type(lt) != type(rt):
                        raise JTLTypeError.from_type(JTLTypeErrorType.BIT_OPERATOR_SIGNED_UNSIGNED,
                                                     node.get_location(), str(lt), node.token.op, str(rt))
                    node.type = node.left.type
                    return node
                case Operator.SHIFT_LEFT | Operator.SHIFT_RIGHT:
                    if not is_integer(lt):
                        raise JTLTypeError.from_type(JTLTypeErrorType.SHIFT_ON_NON_INTEGER,
                                                     node.get_location(), str(node.token.op), str(lt))
                    if not is_integer(rt):
                        raise JTLTypeError.from_type(JTLTypeErrorType.SHIFT_BY_NON_INTEGER,
                                                     node.get_location(), str(node.token.op), str(rt))
                    node.type = node.left.type
                    return node
                case Operator.EQUAL | Operator.NOTEQUAL:
                    node.type = parent.type_table.new_build_in_type(BuildInType.BOOL)
                    if is_numeric(lt) and is_numeric(rt):
                        node.left, node.right = promote_either(parent.type_table, node.left, node.right)
                        return node
                    if type(lt) != type(rt):
                        raise JTLTypeError.from_type(JTLTypeErrorType.NOT_COMPARABLE,
                                                     node.get_location(),
                                                     lt, rt)
                    if isinstance(lt, TypeBool):
                        return node
                    if isinstance(lt, TypePointer) and isinstance(rt, TypePointer):
                        if lt == rt:
                            return node
                        else:
                            raise JTLTypeError.from_type(JTLTypeErrorType.NOT_COMPARABLE,
                                                         node.get_location(),
                                                         lt, rt)
                    if isinstance(lt, (TypeRecord, TypeType, TypeProcedure)):
                        # TODO compound aka strings, types, procedures
                        raise NotImplementedError()
                    raise JTLTypeError.from_type(JTLTypeErrorType.NOT_COMPARABLE, node.get_location(),
                                                 lt, rt)
                case Operator.LESS | Operator.GREATER | Operator.LESSEQUAL | Operator.GREATEREQUAL:
                    if (is_numeric(parent.type_table.get(node.left.type))
                            and is_numeric(parent.type_table.get(node.right.type))):
                        node.left, node.right = promote_either(parent.type_table, node.left, node.right)
                        node.type = parent.type_table.new_build_in_type(BuildInType.BOOL)
                        return node
                    else:
                        raise JTLTypeError.from_type(JTLTypeErrorType.NOT_NUMERIC, node.get_location(),
                                                     parent.type_table.get(node.left.type),
                                                     parent.type_table.get(node.right.type))
                case Operator.AND | Operator.OR:
                    at, bt = parent.type_table.get(node.left.type), parent.type_table.get(node.right.type)
                    if isinstance(at, TypeBool) and isinstance(bt, TypeBool):
                        node.type = node.left.type
                        return node
                    raise JTLTypeError.from_type(JTLTypeErrorType.NOT_COMPARABLE, node.get_location(),
                                                 node.token.op, at, bt)
                case Operator.COLON:
                    raise NotImplementedError()  # FIXME: is this even valid anywhere?
                case Operator.ASSIGNMENT:
                    if not node.left.mutable:
                        raise ElaborationError.from_type(ElaborationErrorType.UN_ASSIGNABLE, node.left.get_location())
                    if not narrow_type(parent.type_table, node.left.type, node.right.type):
                        raise JTLTypeError.from_type(JTLTypeErrorType.TYPE_MISSMATCH_ASSIGNMENT,
                                                     node.get_location(),
                                                     parent.type_table.get(node.right.type),
                                                     lt)

                    if isinstance(node.right, ASTNodeProcedure):
                        uid = parent.unique_indexer.next()
                        str_name = f"anonymous_procedure_{uid}"
                        name = Name(
                            uid,
                            str_name,
                            node.right.token.location,
                            node.right.type,
                            parent.type_table,
                            Mutability.CONSTANT,
                        )
                        parent.names[str_name] = name
                        parent.procedures[name] = node.right
                        type_id = node.right.type
                        node.right = ASTNodeValue(TokenName(node.right.token.location, str_name))
                        node.right.type = type_id
                    elif isinstance(node.right, ASTNodeProcedureStub):
                        raise ElaborationError.from_type(ElaborationErrorType.EXTERNAL_PROCEDURE_NOT_CONST,
                                                         node.get_location())

                    assign = ASTNodeAssignment(node.token, node, node.left, node.right)
                    assign.type = node.type
                    assign.mutable = node.right.mutable
                    return assign
                case _:
                    raise RuntimeError(f"Unexpected Operator {node}")
        case ASTNodeUnary():
            node.child = operand = elaborate_expression(parent, node.child, constant, returnable)
            ot = parent.type_table.get(operand.type)
            match node.token.op:
                case Operator.PLUS:
                    if is_numeric(ot):
                        node.type = operand.type
                        return node
                    else:
                        raise JTLTypeError.from_type(JTLTypeErrorType.NOT_NUMERIC, node.get_location(), ot)
                case Operator.MINUS:
                    if is_numeric(ot):
                        if isinstance(ot, TypeUInt):
                            if ot.size is None:
                                parent.type_table.overwrite(operand.type, parent.type_table.int_literal)
                                node.type = operand.type
                            elif ot.size < 8:
                                cast = ASTNodeCast(node.token, operand)
                                cast.type = parent.type_table.new_larger_int(ot.size)
                                node.child = cast
                                node.type = cast.type
                            else:
                                raise JTLTypeError.from_type(JTLTypeErrorType.UNSAFE_UINT_TO_INT, node.token.location,
                                                             ot)
                        else:
                            node.type = operand.type
                        return node
                    else:
                        raise JTLTypeError.from_type(JTLTypeErrorType.NOT_NUMERIC, node.get_location(), ot)
                case Operator.NOT:
                    if not isinstance(ot, TypeBool):
                        raise JTLTypeError.from_type(JTLTypeErrorType.UNSAFE_UINT_TO_INT, node.token.location,
                                                     Operator.NOT, ot)
                    node.type = operand.type
                    return node
                case Operator.POINTER:
                    raise ElaborationError.from_type(ElaborationErrorType.UNEXPECTED_TYPE_MODIFIER, node.token.location)
                case Operator.ADDRESS_OFF | Operator.ADDRESS_OFF_MUTABLE:
                    # TODO of what can you take an address? Just names or also (some) literals?
                    want_mutable = node.token.op == Operator.ADDRESS_OFF_MUTABLE
                    if isinstance(operand, ASTNodeValue) and isinstance(operand.token, TokenName):
                        name_of_target = parent.lookup(operand.token.name)
                        if name_of_target is None:
                            raise ElaborationError.from_type(ElaborationErrorType.UNDECLARED_NAME,
                                                             operand.token.location)
                        if want_mutable and name_of_target.mut:
                            node.type = parent.type_table.new(TypePointer(operand.type, True,
                                                                          parent.type_table))
                        elif not want_mutable:
                            node.type = parent.type_table.new(TypePointer(operand.type, False,
                                                                          parent.type_table))
                        else:
                            raise ElaborationError.from_type(ElaborationErrorType.MUTABLE_ADDRESS_OF_CONST,
                                                             node.token.location)
                        return node
                    elif isinstance(ot, (TypeRecord, TypeFixedSizeArray)):
                        # anonymous record or array literal
                        node.type = parent.type_table.new(TypePointer(operand.type, want_mutable,
                                                                      parent.type_table))
                        return node
                    elif isinstance(operand, ASTNodeArrayAccess) or (isinstance(operand, ASTNodeBinary) and
                                                                     operand.token.op == Operator.DOT):
                        if want_mutable and not operand.mutable:
                            raise ElaborationError.from_type(ElaborationErrorType.MUTABLE_ADDRESS_OF_CONST,
                                                             node.get_location())
                        node.type = parent.type_table.new(TypePointer(operand.type, want_mutable, parent.type_table))
                        return node
                    else:
                        raise ElaborationError.from_type(ElaborationErrorType.ADDRESS_OF_UNASSIGNED,
                                                         node.token.location)
                case Operator.BITWISE_NOT:
                    if not is_integer(ot):
                        raise JTLTypeError.from_type(JTLTypeErrorType.UNARY_BIT_OPERATOR_ON_NON_INTEGER,
                                                     node.get_location(), node.token.op, str(ot))
                    node.type = operand.type
                    return node
                case _:
                    raise RuntimeError(f"Invalid Unary Operator {node}")
        case ASTNodeUnaryRight():
            assert node.token.op == Operator.POINTER
            node.child = operand = elaborate_expression(parent, node.child, constant, returnable)
            ot = parent.type_table.get(operand.type)
            if not isinstance(ot, TypePointer):
                raise JTLTypeError.from_type(JTLTypeErrorType.DEREFERENCE_VALUE, node.token.location, ot)
            node.type = ot.target_type
            node.mutable = ot.target_mutable
            return node
        case ASTNodeStatement():
            if node.token.keyword == Keyword.RETURN:
                if returnable is None:
                    raise ElaborationError.from_type(ElaborationErrorType.CAN_NOT_RETURN_FROM_HERE, node.token.location)

                assert node.child is not None
                node.child = elaborate_expression(parent, node.child, constant, returnable)
                rt = parent.type_table.get(returnable.type)
                assert isinstance(rt, TypeProcedure)
                if not narrow_type(parent.type_table, rt.return_type, node.child.type):
                    raise JTLTypeError.from_type(JTLTypeErrorType.TYPE_MISSMATCH_RETURN,
                                                 node.get_location(),
                                                 parent.type_table.get(node.child.type),
                                                 parent.type_table.get(rt.return_type))

                node.type = parent.type_table.new(TypeReturns())
                return node
            raise ElaborationError.from_type(ElaborationErrorType.UNEXPECTED_STATEMENT, node.token.location)
        case ASTNodeValue():
            match node.token:
                case TokenNumberLiteral():
                    if isinstance(node.token.value, int):
                        node.type = parent.type_table.new(parent.type_table.uint_literal)
                        return node
                    elif isinstance(node.token.value, float):
                        node.type = parent.type_table.new(parent.type_table.float_literal)
                        return node
                    else:
                        raise RuntimeError(f"Bad Number literal {node}")
                case TokenStringLiteral():
                    if node.token.zero_terminated:
                        t = parent.type_table.new_build_in_type(BuildInType.CHAR)
                        node.type = parent.type_table.new(TypePointer(t, False, parent.type_table))
                    else:
                        node.type = parent.type_table.new_build_in_type(BuildInType.STRING)
                    return node
                case TokenBoolLiteral():
                    node.type = parent.type_table.new_build_in_type(BuildInType.BOOL)
                    return node
                case TokenName():
                    if (name_obj := parent.lookup(node.token.name)) is None:
                        raise ElaborationError.from_type(ElaborationErrorType.UNDECLARED_NAME,
                                                         node.token.location)
                    if constant and name_obj.mut != Mutability.CONSTANT:
                        raise ElaborationError.from_type(ElaborationErrorType.NON_CONSTANT_IN_CONSTANT_EXPRESSION,
                                                         node.token.location)
                    node.type = name_obj.type
                    node.mutable = name_obj.mut == Mutability.MUTABLE
                    return node
                case TokenBuildInType():
                    node.type = parent.type_table.new_build_in_type(BuildInType.TYPE)
                    return node
        case ASTNodeIf():
            node.condition = elaborate_expression(parent, node.condition, constant, returnable)
            condition_type = parent.type_table.get(node.condition.type)
            if not isinstance(condition_type, TypeBool):
                raise JTLTypeError.from_type(JTLTypeErrorType.EXPECTED_BOOLEAN_CONDITION, node.condition.token.location,
                                             condition_type)
            node.body = elaborate_expression(parent, node.body, constant, returnable)
            if node.else_body is not None:
                node.else_body = elaborate_expression(parent, node.else_body, constant, returnable)
                at = parent.type_table.get(node.body.type)
                bt = parent.type_table.get(node.else_body.type)
                if isinstance(at, TypeReturns) and isinstance(bt, TypeReturns):
                    node.type = node.body.type
                elif isinstance(at, TypeReturns):
                    node.type = node.else_body.type
                elif isinstance(bt, TypeReturns):
                    node.type = node.body.type
                else:
                    node.body, node.else_body = promote_either(parent.type_table, node.body, node.else_body)
                    node.type = node.body.type
            else:
                node.type = parent.type_table.new(TypeNoValue())
            return node
        case ASTNodeWhile():
            node.condition = elaborate_expression(parent, node.condition, constant, returnable)
            condition_type = parent.type_table.get(node.condition.type)
            if not isinstance(condition_type, TypeBool):
                raise JTLTypeError.from_type(JTLTypeErrorType.EXPECTED_BOOLEAN_CONDITION, node.condition.token.location,
                                             condition_type)
            node.elaborated_body = Scope(parent)
            if elaborate_scope(node.elaborated_body, node.body, returnable):
                node.type = parent.type_table.new(TypeReturns())
            else:
                node.type = parent.type_table.new(TypeNoValue())
            return node
        case ASTNodeProcedure() | ASTNodeProcedureStub():
            arguments = []
            untyped = []
            for i, arg in enumerate(node.arguments):
                # TODO: allow for default values - this will also require field names to be saved in a procedure type
                if isinstance(arg, ASTNodeVarArgs):
                    if i != len(node.arguments) - 1:
                        raise ElaborationError.from_type(ElaborationErrorType.VAR_ARGS_NOT_LAST, arg.token.location)
                    elif isinstance(node, ASTNodeProcedure):
                        raise ElaborationError.from_type(ElaborationErrorType.VAR_ARGS_NOT_EXTERNAL,
                                                         node.token.location)
                    node.var_args = True
                    break
                arguments.append(elaborate_name_and_type(parent, arg))
                if isinstance(parent.type_table.get(arguments[-1].type), TypeUndefined):
                    untyped.append(arguments[-1])
                elif len(untyped) > 0:
                    collective_type = arguments[-1].type
                    for ua in untyped:
                        ua.type = collective_type
                    untyped = []
            if len(untyped) > 0:
                raise ElaborationError.from_type(ElaborationErrorType.MISSING_TYPE, node.token.location)

            if node.return_type_expr is not None:
                return_type = elaborate_type(parent, node.return_type_expr)
            else:
                return_type = parent.type_table.new(TypeNoValue())

            node.argument_names = arguments
            node.type = parent.type_table.new(
                TypeProcedure(list(map(lambda x: x.type, arguments)), return_type, node.var_args,
                              parent.type_table))

            if not direct_assignment:
                if isinstance(node, ASTNodeProcedureStub):
                    raise ElaborationError.from_type(ElaborationErrorType.EXTERNAL_PROCEDURE_NOT_CONST,
                                                     node.token.location)
                uid = parent.unique_indexer.next()
                name = Name(
                    uid,
                    f"anonymous_procedure_{uid}",
                    node.token.location,
                    node.type,
                    parent.type_table,
                    Mutability.CONSTANT,
                )
                parent.names[f"anonymous_procedure_{uid}"] = name
                parent.procedures[name] = node
            else:
                return node
        case ASTNodeScope():
            # TODO maybe scopes can return something
            node.elaborated_body = Scope(parent)
            if elaborate_scope(node.elaborated_body, node.body, returnable):
                node.type = parent.type_table.new(TypeReturns())
            else:
                node.type = parent.type_table.new(TypeNoValue())
            return node
        case ASTNodeTupleLike(token=bracket_token, parent=p) if (
                    p is not None and isinstance(bracket_token, TokenBracket)
                    and bracket_token.type == BracketType.ROUND):
            # This is a procedure call or record initializer
            # TODO allow more complex situations like module.proc_name() or calling a just defined function
            if not (isinstance(p, ASTNodeValue) and isinstance(p.token, TokenName)):
                raise ElaborationError.from_type(ElaborationErrorType.NOT_CALLABLE, p.token.location)

            if (proc_name := parent.lookup(p.token.name)) is None:
                raise ElaborationError.from_type(ElaborationErrorType.UNDECLARED_PROC, p.token.location)

            proc_type = parent.type_table.get(proc_name.type)
            argument_list: List[int]
            varargs = False

            match proc_type:
                case TypeProcedure():
                    argument_list = proc_type.arguments
                    return_type = proc_type.return_type
                    varargs = proc_type.varargs
                case TypeType(instance_type=it_index) if isinstance(it := parent.type_table.get(it_index), TypeRecord):
                    argument_list = [field_name.type for field_name in it.record.fields.values()]
                    instance_type = it_index
                    return_type = it_index
                case _:
                    raise ElaborationError.from_type(ElaborationErrorType.NOT_CALLABLE, p.token.location)

            if len(node.children) != len(argument_list) and not varargs:
                raise ElaborationError.from_type(ElaborationErrorType.WRONG_NUMBER_OF_ARGUMENTS, node.get_location(),
                                                 len(argument_list), len(node.children))

            call_arguments = []
            for i, (n, t) in enumerate(zip(node.children, argument_list)):
                elab = elaborate_expression(parent, n, constant, returnable)
                call_arguments.append(elab)
                # TODO do we want promotion here?
                if not narrow_type(parent.type_table, t, elab.type):
                    raise ElaborationError.from_type(ElaborationErrorType.WRONG_ARGUMENT_TYPE,
                                                     elab.get_location(),
                                                     i + 1,
                                                     parent.type_table.get(t),
                                                     parent.type_table.get(elab.type))

            if varargs:
                for n in node.children[len(argument_list):]:
                    elab = elaborate_expression(parent, n, constant, returnable)
                    call_arguments.append(elab)

            if parent.parent is None:
                # To simplify code generation all dynamic code like function calls are disallowed in the globals scope
                raise ElaborationError.from_type(ElaborationErrorType.GLOBAL_CALL, node.get_location())
            call = ASTNodeCall(p.token, proc_name, call_arguments)

            call.type = return_type

            if (isinstance(proc_type, TypeType)
                    and isinstance(parent.type_table.get(proc_type.instance_type), TypeRecord)
                    and not direct_assignment):
                # TODO this is probably broken when records are nested...
                uid = parent.unique_indexer.next()
                name = Name(
                    uid,
                    f"anonymous_record_{uid}",
                    node.token.location,
                    instance_type,
                    parent.type_table,
                    Mutability.MUTABLE,  # TODO is this right
                )
                parent.names[f"anonymous_record_{uid}"] = name
                assignment = ASTNodeAssignment(node.token,
                                               ASTNodeBinary(
                                                   TokenOperator(node.token.location, Operator.ASSIGNMENT),
                                                   ASTNode(node.token),
                                                   ASTNode(node.token)),
                                               name,
                                               call)
                assignment.type = call.type
                return assignment
            return call

        case ASTNodeTupleLike(token=tok_kw) if isinstance(tok_kw, TokenKeyword) and tok_kw.keyword == Keyword.CAST:
            if len(node.children) != 2:
                raise ElaborationError.from_type(ElaborationErrorType.WRONG_NUMBER_OF_ARGUMENTS_CAST,
                                                 node.get_location(), len(node.children))
            target_type = elaborate_type(parent, node.children[0])
            source_expr = elaborate_expression(parent, node.children[1], constant, returnable)
            if can_cast(parent.type_table, target_type, source_expr.type):
                rv = ASTNodeCast(tok_kw, source_expr)
                rv.type = target_type
                return rv
            else:
                raise ElaborationError.from_type(ElaborationErrorType.WRONG_ARGUMENT_TYPE,
                                                 node.get_location(),
                                                 parent.type_table.get(source_expr.type),
                                                 parent.type_table.get(target_type))
        case ASTNodeTupleLike(token=tok_kw) if isinstance(tok_kw, TokenKeyword) and tok_kw.keyword == Keyword.TRANSMUTE:
            if len(node.children) != 2:
                raise ElaborationError.from_type(ElaborationErrorType.WRONG_NUMBER_OF_ARGUMENTS_CAST,
                                                 node.get_location(), len(node.children))
            target_type = elaborate_type(parent, node.children[0])
            source_expr = elaborate_expression(parent, node.children[1], constant, returnable)
            source_type = parent.type_table.get(source_expr.type)
            # TODO if this is handled better later new checks for all the type groups have to be considered
            if source_type.size is None:
                raise JTLTypeError.from_type(JTLTypeErrorType.TRANSMUTE_UNRESOLVED_SIZE,
                                             node.get_location())
            target_type_t = parent.type_table.get(target_type)
            if source_type.size != target_type_t.size:
                raise JTLTypeError.from_type(JTLTypeErrorType.TRANSMUTE_SIZE_MISSMATCH,
                                             node.get_location(), target_type_t.size,
                                             source_type.size)
            rv_transmute = ASTNodeTransmute(tok_kw, source_expr)
            rv_transmute.type = target_type
            return rv_transmute
        case ASTNodeTupleLike(token=bracket) if (isinstance(bracket, TokenBracket) and
                                                 bracket.type == BracketType.SQUARE and node.parent is not None):
            # Array member access
            # TODO allow multiple indices
            assert len(node.children) == 1
            index = elaborate_expression(parent, node.children[0], constant, None)
            index = ensure_index_type(index, parent.type_table, node.children[0].get_location())
            array = elaborate_expression(parent, node.parent, constant, None)
            array_type = parent.type_table.get(array.type)
            if isinstance(array_type, TypePointer):
                array_type = parent.type_table.get(array_type.target_type)
            if not isinstance(array_type, TypeFixedSizeArray):
                raise JTLTypeError.from_type(JTLTypeErrorType.INDEX_INTO_NON_ARRAY, array.get_location(), array_type)
            node_array_access = ASTNodeArrayAccess(node.token, node.get_location(), index, array)
            node_array_access.type = array_type.base_type
            node_array_access.mutable = array.mutable
            return node_array_access
        case ASTNodeTupleLike(token=bracket) if (isinstance(bracket, TokenBracket) and
                                                 bracket.type == BracketType.SQUARE and node.parent is None):
            # Array literal
            assert node.parent is None
            elaborated_children: List[ASTNode] = []
            assumed_type: Optional[Type] = None
            for child in node.children:
                elaborated_children.append(elaborate_expression(parent, child, constant, returnable))
                child_type = parent.type_table.get(elaborated_children[-1].type)
                if assumed_type is None:
                    assumed_type = child_type
                elif assumed_type != child_type:
                    assert len(elaborated_children) > 1
                    elaborated_children[0], elaborated_children[-1] = promote_either(parent.type_table,
                                                                                     elaborated_children[0],
                                                                                     elaborated_children[-1])

            assert assumed_type is not None
            node.children = elaborated_children
            node.type = parent.type_table.new(TypeFixedSizeArray(None, len(elaborated_children), node.children[-1].type,
                                                                 parent.type_table))

            # insert a pseudo stack variable if array is not assigned immediately
            if not direct_assignment:
                uid = parent.unique_indexer.next()
                name = Name(
                    uid,
                    f"anonymous_array_{uid}",
                    node.token.location,
                    node.type,
                    parent.type_table,
                    Mutability.MUTABLE,
                )
                parent.names[f"anonymous_array{uid}"] = name
                assignment = ASTNodeAssignment(node.token,
                                               ASTNodeBinary(
                                                   TokenOperator(node.token.location, Operator.ASSIGNMENT),
                                                   ASTNode(node.token),
                                                   ASTNode(node.token)),
                                               name,
                                               node)
                assignment.type = node.type
                return assignment

            return node
        case ASTNodeRecord():
            raise NotImplementedError("Inline records are not supported yet")
        case _:
            raise RuntimeError(f"Bad ast node in expression {node}")
    raise RuntimeError("This is here so mypy is happy (I am probably missing a return statement somewhere)")


def ensure_index_type(index_node: ASTNode, type_table: TypeTable, location: CodeLocation) -> ASTNode:
    # TODO in the future allow signed ints and do an automatic start at end for negative numbers
    index_type = type_table.get(index_node.type)

    if not isinstance(index_type, TypeUInt):
        raise JTLTypeError.from_type(JTLTypeErrorType.INDEX_MUST_BE_USIZE, location)
    elif index_type.size is None:
        type_table.overwrite(index_node.type, BuildInType.USIZE)
        return index_node
    elif index_type.size > PLATFORM_POINTER_SIZE:
        raise JTLTypeError.from_type(JTLTypeErrorType.INDEX_MUST_BE_USIZE, location, index_type.size * 8,
                                     PLATFORM_POINTER_SIZE * 8)
    elif index_type.size == PLATFORM_POINTER_SIZE:
        return index_node
    else:
        t = type_table.new_build_in_type(BuildInType.USIZE)
        cast = ASTNodeCast(token=index_node.token, child=index_node)
        cast.type = t
        return cast


def narrow_type(type_table: TypeTable, narrow: int, broad: int) -> bool:
    nt, ni = type_table.get_with_table_index(narrow)
    bt, bi = type_table.get_with_table_index(broad)

    # if narrow type is missing just use broad type (unless also undefined or no value)
    if isinstance(nt, TypeUndefined):
        if isinstance(bt, (TypeUndefined, TypeNoValue)):
            return False
        type_table.overwrite(narrow, broad)
        return True

    # if same do nothing
    if bt == nt:
        # only numeric literals might need to be coerced, to make sure type inference propagates we overwrite
        if is_numeric(bt) and bt.size is None and ni != bi:
            type_table.overwrite(broad, narrow)
        return True

    # handle fixed size arrays
    if isinstance(nt, TypeFixedSizeArray) and isinstance(bt, TypeFixedSizeArray):
        if nt.size != bt.size:
            return False
        return narrow_type(type_table, nt.base_type, bt.base_type)

    # unwrap pointers
    while isinstance(nt, TypePointer) and isinstance(bt, TypePointer):
        narrow = nt.target_type
        broad = bt.target_type
        nt = type_table.get(narrow)
        bt = type_table.get(broad)

    # if broad type already specific fail
    if bt.size is not None:
        return False

    # everything after this point has a LITERAL in the bt

    # convert int literal to float
    if isinstance(nt, TypeFloat) and isinstance(bt, TypeInt):
        type_table.overwrite(broad, narrow)
        return True

    # convert uint literal to int
    if isinstance(nt, TypeInt) and isinstance(bt, TypeUInt):
        type_table.overwrite(broad, narrow)
        return True

    if type(nt) != type(bt):
        return False

    type_table.overwrite(broad, narrow)
    return True


def promote_either(type_table: TypeTable, a: ASTNode, b: ASTNode) -> Tuple[ASTNode, ASTNode]:
    """Tries to make node a and b compatible for arithmetic, fails if unsafe cast is needed"""
    at, ai = type_table.get_with_table_index(a.type)
    bt, bi = type_table.get_with_table_index(b.type)

    # if same do nothing
    if at == bt:
        # only numeric literals might need to be coerced, to make sure type inference propagates we overwrite
        if is_numeric(bt) and bt.size is None and ai != bi:
            type_table.overwrite(a.type, b.type)
        return a, b

    assert not isinstance(at, TypeUndefined)
    assert not isinstance(bt, TypeUndefined)

    # try to convert literals to more specific type
    if narrow_type(type_table, a.type, b.type) or narrow_type(type_table, b.type, a.type):
        return a, b

    assert at.size is not None
    assert bt.size is not None

    # enlarge one type
    if type(at) == type(bt):
        if at.size > bt.size:
            cast = ASTNodeCast(b.token, b)
            cast.type = a.type
            return a, cast
        else:
            cast = ASTNodeCast(a.token, a)
            cast.type = b.type
            return cast, b

    # integers to floats
    if isinstance(at, TypeFloat) and is_numeric(bt):
        cast = ASTNodeCast(b.token, b)
        cast.type = a.type
        return a, cast
    if isinstance(bt, TypeFloat) and is_numeric(at):
        cast = ASTNodeCast(a.token, a)
        cast.type = b.type
        return cast, b

    # uint to int if int is larger than uint
    if isinstance(at, TypeUInt) and isinstance(bt, TypeInt):
        if at.size < bt.size:
            cast = ASTNodeCast(b.token, b)
            cast.type = a.type
            return a, cast
        if at.size < 8 and bt.size < 8:
            cast_a = ASTNodeCast(a.token, a)
            cast_b = ASTNodeCast(b.token, b)
            nt = type_table.new_larger_int(at.size)
            cast_a.type = nt
            cast_b.type = nt
            return cast_a, cast_b
        raise JTLTypeError.from_type(JTLTypeErrorType.UNSAFE_AUTOMATIC_CAST, a.get_location(), at, bt)
    if isinstance(bt, TypeUInt) and isinstance(at, TypeInt):
        if bt.size < at.size:
            cast = ASTNodeCast(a.token, a)
            cast.type = b.type
            return cast, b
        if bt.size < 8 and at.size < 8:
            cast_a = ASTNodeCast(a.token, a)
            cast_b = ASTNodeCast(b.token, b)
            nt = type_table.new_larger_int(bt.size)
            cast_a.type = nt
            cast_b.type = nt
            return cast_a, cast_b
        raise JTLTypeError.from_type(JTLTypeErrorType.UNSAFE_AUTOMATIC_CAST, a.get_location(), at, bt)

    raise JTLTypeError.from_type(JTLTypeErrorType.INCOMPATIBLE_TYPE_GROUP, a.get_location(), at, bt)


castable_type_groups = (TypeInt, TypeUInt, TypeFloat, TypeBool, TypePointer)


def can_cast(type_table: TypeTable, a: int, b: int) -> bool:
    at, bt = type_table.get(a), type_table.get(b)
    return isinstance(at, castable_type_groups) and isinstance(bt, castable_type_groups)
