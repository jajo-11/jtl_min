import operator
from typing import Tuple

from ast_types import *
from elaboration_types import Scope, Name, Record
from typing_types import *
from errors import ElaborationError, ElaborationErrorType, JTLTypeError, JTLTypeErrorType


def elaborate_module(nodes: List[ASTNode]) -> Scope:
    scope = Scope(TypeTable())
    _ = elaborate_scope(scope, nodes, None)

    # take literal types whose types could not be inferred and convert them to discrete types
    for i, t in enumerate(scope.type_table.table):
        if i == 0:  # skip sentinel
            continue
        if isinstance(t, Type) and t.info.size is None:
            match t.info.group:
                case TypeGroup.TYPE | TypeGroup.NO_VALUE:
                    pass
                case TypeGroup.UNDEFINED:
                    raise RuntimeError("Found unresolved type in Type Table")
                case TypeGroup.INT:
                    scope.type_table.overwrite(i, Type(TypeType.I64))
                case TypeGroup.UINT:
                    # unless the - unary is used all int literals start out as UINTs converting them to U64 would
                    # probably be surprising to the user when doing a - b etc. so if no information is available they
                    # get converted to I64
                    scope.type_table.overwrite(i, Type(TypeType.I64))
                case TypeGroup.FLOAT:
                    scope.type_table.overwrite(i, Type(TypeType.F64))
                case TypeGroup.BOOL:
                    raise RuntimeError("Found boolean type with none size in Type Table")
                case TypeGroup.SYMBOL:
                    raise RuntimeError("Found symbol type with none size in Type Table")
                case TypeGroup.POINTER:
                    raise RuntimeError("Found pointer type with none size in Type Table")
                case TypeGroup.RECORD:
                    raise RuntimeError("Found compound type with none size in Type Table")

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
                record_type = parent.type_table.new(TypeRecordType(record))
                name = Name(
                    parent.unique_indexer.next(),
                    left.token.name,
                    left.token.location.whole_line(),
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
    # FIXME: this is a bit odd this sets the size of the type to the size of the instance
    for record, node in records:
        nt = parent.type_table.get(node.type)
        nt.info.size = record.get_size()

    # fourth process the constants - calls to procedures are not allowed for now (Constructors?)
    for node in constant_nodes:
        assert node.child is not None
        expr = elaborate_declaration(parent, node.child, True, returnable)
        assert isinstance(expr.target, Name)
        expr.target.mut = Mutability.CONSTANT
        # TODO these should be evaluated at compile time and added to a separate memory section (data)
        if not isinstance(expr.expression, ASTNodeProcedure):
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
        if last_node_type.info.group == TypeGroup.RETURNS and i != (len(nodes) - 1):
            raise ElaborationError.from_type(ElaborationErrorType.UNREACHABLE, nodes[-1].token.location.whole_line())
        if i == (len(nodes) - 1) and last_node_type.info.group != TypeGroup.RETURNS and returnable is not None:
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
            raise ElaborationError.from_type(ElaborationErrorType.MISSING_RETURN, proc.token.location.whole_line(),
                                             name.name)

    return True


def elaborate_record(record: Record, parent: Scope, nodes: List[ASTNode]):
    for node in nodes:
        if not isinstance(node, ASTNodeStatement):
            raise ElaborationError.from_type(ElaborationErrorType.NOT_A_DECLARATION, node.token.location.whole_line())
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
                                                 node.token.location.whole_line())


def elaborate_declaration(parent: Scope, node: ASTNode, constant: bool, returnable: Optional[ASTNodeProcedure]
                          ) -> ASTNodeAssignment:
    # returnable is always unused as it returns nothing and therefore can not be assigned, but I prefer getting a
    # no value error than there is nothing to return from error. (as that might not be true)
    if not (isinstance(node, ASTNodeBinary) and isinstance(node.token, TokenOperator)
            and node.token.op == Operator.ASSIGNMENT):
        raise ElaborationError.from_type(ElaborationErrorType.NO_ASSIGNMENT_DECLARATION,
                                         node.token.location.whole_line())
    n = elaborate_name_and_type(parent, node.left)
    e = elaborate_expression(parent, node.right, constant, returnable, direct_assignment=True)

    e_type = parent.type_table.get(e.type)

    if e_type.info.group == TypeGroup.NO_VALUE:
        raise ElaborationError.from_type(ElaborationErrorType.EXPECTED_VALUE, node.token.location.whole_line())

    if not narrow_type(parent.type_table, n.type, e.type):
        raise JTLTypeError.from_type(JTLTypeErrorType.TYPE_MISSMATCH_DECLARATION, node.token.location.whole_line(),
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

    new_node = ASTNodeAssignment(node.token, node, n, e)
    new_node.type = n.type
    return new_node


def elaborate_name_and_type(parent: Scope, node: ASTNode) -> Name:
    """parent must be const (only use it for lookup and name index generation
    if you need to change this make sure you do not break elaborate_record (which uses it's parent scope)"""
    if isinstance(node, ASTNodeBinary) and node.token.op == Operator.COLON:
        if not (isinstance(node.left, ASTNodeValue) and isinstance(node.left.token, TokenName)):
            raise ElaborationError.from_type(ElaborationErrorType.EXPECTED_NAME_DECLARATION,
                                             node.token.location.whole_line())
        return Name(parent.unique_indexer.next(), node.left.token.name, node.token.location.whole_line(),
                    elaborate_type(parent, node.right), parent.type_table)
    elif isinstance(node, ASTNodeValue) and isinstance(node.token, TokenName):
        return Name(parent.unique_indexer.next(), node.token.name, node.token.location.whole_line(),
                    parent.type_table.new(Type()), parent.type_table)
    else:
        raise ElaborationError.from_type(ElaborationErrorType.EXPECTED_NAME_DECLARATION,
                                         node.token.location.whole_line())


def elaborate_type(parent: Scope, node: ASTNode) -> int:
    pointer_to_mutable = []
    while True:
        if isinstance(node, ASTNodeUnary) and node.token.op == Operator.POINTER:
            pointer_to_mutable.append(True)
            node = node.child
        if isinstance(node, ASTNodeUnary) and node.token.op == Operator.CONSTANT_POINTER:
            pointer_to_mutable.append(False)
            node = node.child
        elif isinstance(node, ASTNodeValue) and isinstance(node.token, TokenBuildInType):
            base_type = Type(map_build_in_type_to_type_type[node.token.type])
            for mut in pointer_to_mutable:
                base_type = TypePointer(parent.type_table.new(base_type), mut, parent.type_table)
            return parent.type_table.new(base_type)
        elif isinstance(node, ASTNodeValue) and isinstance(node.token, TokenName):
            name_obj = parent.lookup(node.token.name)
            if name_obj is None:
                raise ElaborationError.from_type(ElaborationErrorType.UNDECLARED_NAME, node.token.location.whole_line())
            base_type = name_obj.type  # type: ignore
            for mut in pointer_to_mutable:
                base_type = TypePointer(parent.type_table.new(base_type), mut, parent.type_table)
            return parent.type_table.new(base_type)
        else:
            raise ElaborationError.from_type(ElaborationErrorType.EXPECTED_TYPE, node.token.location.whole_line())


def is_numeric(type: Type) -> bool:
    return type.info.group in (TypeGroup.INT, TypeGroup.FLOAT, TypeGroup.UINT)


def is_integer(type: Type) -> bool:
    return type.info.group in (TypeGroup.INT, TypeGroup.UINT)


def check_assignable(node: ASTNode, parent: Scope):
    """ensure that is indeed assignable, raises ElaborationError"""
    current_node = node
    last_mutable: Optional[bool] = None
    while True:
        match current_node:
            case ASTNodeValue(token=nt) if isinstance(nt, TokenName):
                if (name := parent.lookup(nt.name)) is None:
                    raise ElaborationError.from_type(ElaborationErrorType.UNDECLARED_NAME, nt.location)

                if last_mutable is None:
                    if name.mut != Mutability.MUTABLE:
                        raise ElaborationError.from_type(ElaborationErrorType.NOT_MUTABLE,
                                                         nt.location.whole_line(),
                                                         name.mut)
                elif not last_mutable:
                    raise ElaborationError.from_type(ElaborationErrorType.WRITE_TO_CONST_POINTER,
                                                     nt.location.whole_line())

                return
            case ASTNodeUnaryRight(token=top) if top.op == Operator.POINTER:
                child_type = parent.type_table.get(current_node.child.type)
                assert isinstance(child_type, TypePointer)
                last_mutable = child_type.target_mutable
                current_node = current_node.child
            case ASTNodeBinary(token=dot) if dot.op == Operator.DOT:
                lhs_type = parent.type_table.get(current_node.left.type)
                if isinstance(lhs_type, TypePointer):
                    if not lhs_type.target_mutable:
                        raise ElaborationError.from_type(ElaborationErrorType.WRITE_TO_CONST_POINTER,
                                                         node.token.location.whole_line())
                    lhs_type = parent.type_table.get(lhs_type.target_type)
                assert isinstance(lhs_type, TypeRecordInstance)  # or isinstance(lhs_type, TypePointer) and
                assert isinstance(current_node.right, ASTNodeValue) and isinstance(current_node.right.token, TokenName)
                if (m := lhs_type.record.fields[current_node.right.token.name].mut) != Mutability.MUTABLE:
                    raise ElaborationError.from_type(ElaborationErrorType.NOT_MUTABLE,
                                                     node.token.location.whole_line(),
                                                     m)
                current_node = current_node.left
            case _:
                raise ElaborationError.from_type(ElaborationErrorType.UN_ASSIGNABLE, node.token.location)


def resolve_field(parent: Scope, node: ASTNodeBinary) -> ASTNodeBinary:
    assert node.token.op == Operator.DOT
    lhs = elaborate_expression(parent, node.left, False, None)
    lhs_type = parent.type_table.get(lhs.type)
    if isinstance(lhs_type, TypePointer):
        lhs_type = parent.type_table.get(lhs_type.target_type)
    if not isinstance(lhs_type, TypeRecordInstance):
        raise ElaborationError.from_type(ElaborationErrorType.EXPECTED_RECORD, lhs.token.location)
    if not (isinstance(node.right, ASTNodeValue) and isinstance(node.right.token, TokenName)):
        raise ElaborationError.from_type(ElaborationErrorType.EXPECTED_FIELD_NAME, node.right.token.location)
    name = node.right.token.name
    if (field_name := lhs_type.record.fields.get(name)) is not None:
        node.type = field_name.type
    else:
        raise ElaborationError.from_type(ElaborationErrorType.EXPECTED_FIELD_NAME, node.right.token.location)
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
                                                     node.token.location.whole_line(), node.token.op)
                    for e, et in ((node.left, lt), (node.right, rt)):
                        if not is_numeric(et):
                            raise JTLTypeError.from_type(JTLTypeErrorType.NOT_NUMERIC,
                                                         e.token.location.whole_line())
                    node.left, node.right = promote_either(parent.type_table, node.left, node.right)
                    node.type = node.left.type
                    return node
                case Operator.BITWISE_XOR | Operator.BITWISE_AND | Operator.BITWISE_XOR | Operator.BITWISE_OR:
                    if not (is_integer(lt) and is_integer(rt)):
                        raise JTLTypeError.from_type(JTLTypeErrorType.BIT_OPERATOR_ON_NON_INTEGER,
                                                     node.token.location.whole_line(), str(node.token.op))
                    if lt.info.size is None:
                        parent.type_table.overwrite(node.left.type, node.right.type)
                        lt = rt
                    elif rt.info.size is None:
                        parent.type_table.overwrite(node.right.type, node.left.type)
                        rt = lt
                    if lt.info.size != rt.info.size:
                        raise JTLTypeError.from_type(JTLTypeErrorType.BIT_OPERATOR_SIZE_MISSMATCH,
                                                     node.token.location.whole_line())
                    if lt.info.group != rt.info.group:
                        raise JTLTypeError.from_type(JTLTypeErrorType.BIT_OPERATOR_SIGNED_UNSIGNED,
                                                     node.token.location.whole_line())
                    node.type = node.left.type
                    return node
                case Operator.SHIFT_LEFT | Operator.SHIFT_RIGHT:
                    if not (is_integer(lt) and is_integer(rt)):
                        raise JTLTypeError.from_type(JTLTypeErrorType.SHIFT_ON_NON_INTEGER,
                                                     node.token.location.whole_line(), str(node.token.op))
                    node.type = node.left.type
                    return node
                case Operator.EQUAL | Operator.NOTEQUAL:
                    node.type = parent.type_table.new(Type(TypeType.BOOL))
                    if is_numeric(lt) and is_numeric(rt):
                        node.left, node.right = promote_either(parent.type_table, node.left, node.right)
                        return node
                    if lt.info.group != rt.info.group:
                        raise JTLTypeError.from_type(JTLTypeErrorType.NOT_COMPARABLE,
                                                     node.left.token.location.whole_line(),
                                                     lt, rt)
                    if lt.info.group in {TypeGroup.BOOL, TypeGroup.SYMBOL}:
                        return node
                    if isinstance(lt, TypePointer) and isinstance(rt, TypePointer):
                        if lt == rt:
                            return node
                        else:
                            raise JTLTypeError.from_type(JTLTypeErrorType.POINTER_ARITHMETIC,
                                                         node.left.token.location.whole_line(),
                                                         lt.target_type, rt.target_type)
                    if lt.info.group in {TypeGroup.RECORD, TypeGroup.POINTER, TypeGroup.TYPE}:
                        # TODO compound aka strings, types, procedures
                        raise NotImplementedError()
                    raise JTLTypeError.from_type(JTLTypeErrorType.NOT_COMPARABLE, node.token.location.whole_line(),
                                                 lt, rt)
                case Operator.LESS | Operator.GREATER | Operator.LESSEQUAL | Operator.GREATEREQUAL:
                    if (is_numeric(parent.type_table.get(node.left.type))
                            and is_numeric(parent.type_table.get(node.right.type))):
                        node.left, node.right = promote_either(parent.type_table, node.left, node.right)
                        node.type = parent.type_table.new(Type(TypeType.BOOL))
                        return node
                    else:
                        raise JTLTypeError.from_type(JTLTypeErrorType.NOT_NUMERIC, node.token.location.whole_line(),
                                                     parent.type_table.get(node.left.type),
                                                     parent.type_table.get(node.right.type))
                case Operator.AND | Operator.OR:
                    at, bt = parent.type_table.get(node.left.type), parent.type_table.get(node.right.type)
                    if at.info.group == TypeGroup.BOOL and bt.info.group == TypeGroup.BOOL:
                        node.type = node.left.type
                        return node
                    raise JTLTypeError.from_type(JTLTypeErrorType.NOT_COMPARABLE, node.token.location.whole_line(),
                                                 node.token.op, at, bt)
                case Operator.COLON:
                    raise NotImplementedError()  # FIXME: is this even valid anywhere?
                case Operator.ASSIGNMENT:
                    check_assignable(node.left, parent)
                    if not narrow_type(parent.type_table, node.left.type, node.right.type):
                        raise JTLTypeError.from_type(JTLTypeErrorType.TYPE_MISSMATCH_ASSIGNMENT,
                                                     node.token.location.whole_line(),
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

                    assign = ASTNodeAssignment(node.token, node, node.left, node.right)
                    assign.type = node.type
                    return assign
                case _:
                    raise RuntimeError(f"Unexpected Operator {node}")
        case ASTNodeUnary():
            node.child = operand = elaborate_expression(parent, node.child, constant, returnable)
            match node.token.op:
                case Operator.PLUS:
                    if is_numeric(parent.type_table.get(operand.type)):
                        node.type = operand.type
                        return node
                    else:
                        raise JTLTypeError.from_type(JTLTypeErrorType.NOT_NUMERIC, node.token.location.whole_line())
                case Operator.MINUS:
                    if is_numeric(parent.type_table.get(operand.type)):
                        ot = parent.type_table.get(operand.type)
                        if ot.info.group == TypeGroup.UINT:
                            if ot.info.size is None:
                                parent.type_table.overwrite(operand.type, Type(TypeType.INT_LITERAL))
                                node.type = operand.type
                            elif ot.info.size < 8:
                                cast = ASTNodeCast(node.token, operand)
                                cast.type = parent.type_table.new(Type(upgrade_int_size[ot.info.size]))
                                node.child = cast
                                node.type = cast.type
                            else:
                                raise JTLTypeError.from_type(JTLTypeErrorType.UNSAFE_UINT_TO_INT, node.token.location,
                                                             ot)
                        else:
                            node.type = operand.type
                        return node
                    else:
                        raise JTLTypeError.from_type(JTLTypeErrorType.NOT_NUMERIC, node.token.location.whole_line())
                case Operator.NOT:
                    ot = parent.type_table.get(operand.type)
                    if ot.info.group != TypeGroup.BOOL:
                        raise JTLTypeError.from_type(JTLTypeErrorType.UNSAFE_UINT_TO_INT, node.token.location,
                                                     Operator.NOT, ot)
                    node.type = operand.type
                    return node
                case Operator.POINTER:
                    raise ElaborationError.from_type(ElaborationErrorType.UNEXPECTED_TYPE_MODIFIER, node.token.location)
                case Operator.ADDRESS_OFF:
                    # TODO of what can you take an address? Just names or also (some) literals?
                    # TODO address of record field
                    operand_type = parent.type_table.get(operand.type)
                    if isinstance(operand, ASTNodeValue) and isinstance(operand.token, TokenName):
                        name_of_target = parent.lookup(operand.token.name)
                        if name_of_target is None:
                            raise ElaborationError.from_type(ElaborationErrorType.UNDECLARED_NAME,
                                                             operand.token.location)
                        node.type = parent.type_table.new(TypePointer(operand.type,
                                                                      name_of_target.mut == Mutability.MUTABLE,
                                                                      parent.type_table))
                        return node
                    elif isinstance(operand_type, TypeRecordInstance):
                        node.type = parent.type_table.new(TypePointer(operand.type, True, parent.type_table))
                        return node
                    elif isinstance(operand, ASTNodeBinary) and operand.token.op == Operator.DOT:
                        # TODO what about constants in records etc.
                        node.type = parent.type_table.new(TypePointer(operand.type, True, parent.type_table))
                        return node
                    else:
                        raise ElaborationError.from_type(ElaborationErrorType.ADDRESS_OF_UNASSIGNED,
                                                         node.token.location)
                case Operator.BITWISE_NOT:
                    ot = parent.type_table.get(operand.type)
                    if not is_integer(ot):
                        raise JTLTypeError.from_type(JTLTypeErrorType.BIT_OPERATOR_ON_NON_INTEGER,
                                                     node.token.location.whole_line(), node.token.op)
                    node.type = operand.type
                    return node
                case _:
                    raise RuntimeError(f"Invalid Unary Operator {node}")
        case ASTNodeUnaryRight():
            node.child = operand = elaborate_expression(parent, node.child, constant, returnable)
            ot = parent.type_table.get(operand.type)
            if not isinstance(ot, TypePointer):
                raise JTLTypeError.from_type(JTLTypeErrorType.DEREFERENCE_VALUE, node.token.location, ot)
            node.type = ot.target_type
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
                                                 node.token.location.whole_line(),
                                                 parent.type_table.get(node.child.type),
                                                 parent.type_table.get(rt.return_type))

                node.type = parent.type_table.new(Type(TypeType.RETURNS))
                return node
            raise ElaborationError.from_type(ElaborationErrorType.UNEXPECTED_STATEMENT, node.token.location)
        case ASTNodeValue():
            match node.token:
                case TokenNumberLiteral():
                    if isinstance(node.token.value, int):
                        node.type = parent.type_table.new(Type(TypeType.UINT_LITERAL))
                        return node
                    elif isinstance(node.token.value, float):
                        node.type = parent.type_table.new(Type(TypeType.FLOAT_LITERAL))
                        return node
                    else:
                        raise RuntimeError(f"Bad Number literal {node}")
                case TokenStringLiteral():
                    node.type = parent.type_table.new(Type(TypeType.STRING))
                    return node
                case TokenBoolLiteral():
                    node.type = parent.type_table.new(Type(TypeType.BOOL))
                    return node
                case TokenName():
                    if (name_obj := parent.lookup(node.token.name)) is None:
                        raise ElaborationError.from_type(ElaborationErrorType.UNDECLARED_NAME,
                                                         node.token.location.whole_line())
                    if constant and name_obj.mut != Mutability.CONSTANT:
                        raise ElaborationError.from_type(ElaborationErrorType.NON_CONSTANT_IN_CONSTANT_EXPRESSION,
                                                         node.token.location)
                    node.type = name_obj.type
                    return node
        case ASTNodeIf():
            node.condition = elaborate_expression(parent, node.condition, constant, returnable)
            condition_type = parent.type_table.get(node.condition.type)
            if condition_type.info.group != TypeGroup.BOOL:
                raise JTLTypeError.from_type(JTLTypeErrorType.EXPECTED_BOOLEAN_CONDITION, node.condition.token.location,
                                             condition_type)
            node.body = elaborate_expression(parent, node.body, constant, returnable)
            if node.else_body is not None:
                node.else_body = elaborate_expression(parent, node.else_body, constant, returnable)
                at = parent.type_table.get(node.body.type)
                bt = parent.type_table.get(node.else_body.type)
                if at.info.group == TypeGroup.RETURNS and bt.info.group == TypeGroup.RETURNS:
                    node.type = node.body.type
                elif at.info.group == TypeGroup.RETURNS:
                    node.type = node.else_body.type
                elif bt.info.group == TypeGroup.RETURNS:
                    node.type = node.body.type
                else:
                    node.body, node.else_body = promote_either(parent.type_table, node.body, node.else_body)
                    node.type = node.body.type
            else:
                node.type = parent.type_table.new(Type(TypeType.NO_VALUE))
            return node
        case ASTNodeWhile():
            node.condition = elaborate_expression(parent, node.condition, constant, returnable)
            condition_type = parent.type_table.get(node.condition.type)
            if condition_type.info.group != TypeGroup.BOOL:
                raise JTLTypeError.from_type(JTLTypeErrorType.EXPECTED_BOOLEAN_CONDITION, node.condition.token.location,
                                             condition_type)
            node.elaborated_body = Scope(parent)
            if elaborate_scope(node.elaborated_body, node.body, returnable):
                node.type = parent.type_table.new(Type(TypeType.RETURNS))
            else:
                node.type = parent.type_table.new(Type(TypeType.NO_VALUE))
            return node
        case ASTNodeProcedure():
            arguments = []
            untyped = []
            for arg in node.arguments:
                # TODO: allow for default values - this will also require field names to be saved in a procedure type
                arguments.append(elaborate_name_and_type(parent, arg))
                if parent.type_table.get(arguments[-1].type).info.group == TypeGroup.UNDEFINED:
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
                return_type = parent.type_table.new(Type(TypeType.NO_VALUE))

            node.argument_names = arguments
            node.type = parent.type_table.new(TypeProcedure(list(map(lambda x: x.type, arguments)), return_type,
                                                            parent.type_table))

            if not direct_assignment:
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
                node.type = parent.type_table.new(Type(TypeType.RETURNS))
            else:
                node.type = parent.type_table.new(Type(TypeType.NO_VALUE))
            return node
        case ASTNodeTupleLike(parent=p) if p is not None:
            # This is a procedure call
            # TODO allow more complex situations like module.proc_name() or calling a just defined function
            if not (isinstance(p, ASTNodeValue) and isinstance(p.token, TokenName)):
                raise ElaborationError.from_type(ElaborationErrorType.NOT_CALLABLE, p.token.location)

            if (proc_name := parent.lookup(p.token.name)) is None:
                raise ElaborationError.from_type(ElaborationErrorType.UNDECLARED_PROC, p.token.location)

            proc_type = parent.type_table.get(proc_name.type)
            argument_list: List[int]

            match proc_type:
                case TypeProcedure():
                    argument_list = proc_type.arguments
                    return_type = proc_type.return_type
                case TypeRecordType():
                    argument_list = [field_name.type for field_name in proc_type.record.fields.values()]
                    instance_type = parent.type_table.new(proc_type.to_instance_type())
                    return_type = instance_type
                case _:
                    raise ElaborationError.from_type(ElaborationErrorType.NOT_CALLABLE, p.token.location)

            if len(node.children) != len(argument_list):
                raise ElaborationError.from_type(ElaborationErrorType.WRONG_NUMBER_OF_ARGUMENTS,
                                                 node.token.location.whole_line(),
                                                 len(argument_list), len(node.children))

            call_arguments = []
            for i, (n, t) in enumerate(zip(node.children, argument_list)):
                elab = elaborate_expression(parent, n, constant, returnable)
                call_arguments.append(elab)
                # TODO do we want promotion here?
                if not narrow_type(parent.type_table, t, elab.type):
                    raise ElaborationError.from_type(ElaborationErrorType.WRONG_ARGUMENT_TYPE,
                                                     elab.token.location.whole_line(),
                                                     i + 1,
                                                     parent.type_table.get(t),
                                                     parent.type_table.get(elab.type))
            if parent.parent is None:
                # FIXME: Global call - why did I disallow this
                raise ElaborationError.from_type(ElaborationErrorType.GLOBAL_CALL, node.token.location.whole_line())
            call = ASTNodeCall(p.token, proc_name, call_arguments)

            call.type = return_type

            if isinstance(proc_type, TypeRecordType) and not direct_assignment:
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
                                                 node.token.location.whole_line(), len(node.children))
            target_type = elaborate_type(parent, node.children[0])
            source_expr = elaborate_expression(parent, node.children[1], constant, returnable)
            if can_cast(parent.type_table, target_type, source_expr.type):
                rv = ASTNodeCast(tok_kw, source_expr)
                rv.type = target_type
                return rv
            else:
                raise ElaborationError.from_type(ElaborationErrorType.WRONG_ARGUMENT_TYPE,
                                                 node.token.location.whole_line(),
                                                 parent.type_table.get(source_expr.type),
                                                 parent.type_table.get(target_type))
        case ASTNodeTupleLike(token=tok_kw) if isinstance(tok_kw, TokenKeyword) and tok_kw.keyword == Keyword.TRANSMUTE:
            if len(node.children) != 2:
                raise ElaborationError.from_type(ElaborationErrorType.WRONG_NUMBER_OF_ARGUMENTS_CAST,
                                                 node.token.location.whole_line(), len(node.children))
            target_type = elaborate_type(parent, node.children[0])
            source_expr = elaborate_expression(parent, node.children[1], constant, returnable)
            source_type = parent.type_table.get(source_expr.type)
            # TODO if this is handled better later new checks for all the type groups have to be considered
            if source_type.info.size is None:
                raise JTLTypeError.from_type(JTLTypeErrorType.TRANSMUTE_UNRESOLVED_SIZE,
                                             node.token.location.whole_line())
            target_type_t = parent.type_table.get(target_type)
            if source_type.info.size != target_type_t.info.size:
                raise JTLTypeError.from_type(JTLTypeErrorType.TRANSMUTE_SIZE_MISSMATCH,
                                             node.token.location.whole_line(), target_type_t.info.size,
                                             source_type.info.size)
            rv_transmute = ASTNodeTransmute(tok_kw, source_expr)
            rv_transmute.type = target_type
            return rv_transmute
        case ASTNodeRecord():
            raise NotImplementedError("Inline records are not supported yet")
        case _:
            raise RuntimeError(f"Bad ast node in expression {node}")
    raise RuntimeError("This is here so mypy is happy (I am probably missing a return statement somewhere)")


def narrow_type(type_table: TypeTable, narrow: int, broad: int) -> bool:
    nt = type_table.get(narrow)
    bt = type_table.get(broad)

    # if narrow type is missing just use broad type (unless also undefined or no value)
    if nt.info.group == TypeGroup.UNDEFINED:
        if bt.info.group in {TypeGroup.UNDEFINED, TypeGroup.NO_VALUE}:
            return False
        type_table.overwrite(narrow, broad)
        return True

    # if same do nothing
    if bt == nt:
        return True

    # unwrap pointers
    while isinstance(nt, TypePointer) and isinstance(bt, TypePointer):
        narrow = nt.target_type
        broad = bt.target_type
        nt = type_table.get(narrow)
        bt = type_table.get(broad)

    # if broad type already specific fail
    if bt.info.size is not None:
        return False

    # everything after this point has a LITERAL in the bt

    # convert int literal to float
    if nt.info.group == TypeGroup.FLOAT and bt.info.group == TypeGroup.INT:
        type_table.overwrite(broad, narrow)
        return True

    # convert uint literal to int
    if nt.info.group == TypeGroup.INT and bt.info.group == TypeGroup.UINT:
        type_table.overwrite(broad, narrow)
        return True

    if bt.info.group != nt.info.group and bt.info.group:
        return False

    type_table.overwrite(broad, narrow)
    return True


upgrade_int_size = {1: TypeType.I16, 2: TypeType.I32, 4: TypeType.I64}


def promote_either(type_table: TypeTable, a: ASTNode, b: ASTNode) -> Tuple[ASTNode, ASTNode]:
    """Tries to make node a and b compatible for arithmetic, fails if unsafe cast is needed"""
    at = type_table.get(a.type)
    bt = type_table.get(b.type)

    # if same do nothing
    if at == bt:
        return a, b

    assert at.info.group != TypeGroup.UNDEFINED
    assert bt.info.group != TypeGroup.UNDEFINED

    # try to convert literals to more specific type
    if narrow_type(type_table, a.type, b.type) or narrow_type(type_table, b.type, a.type):
        return a, b

    assert at.info.size is not None
    assert bt.info.size is not None

    # enlarge one type
    if at.info.group == bt.info.group:
        if at.info.size > bt.info.size:
            cast = ASTNodeCast(b.token, b)
            cast.type = a.type
            return a, cast
        else:
            cast = ASTNodeCast(a.token, a)
            cast.type = b.type
            return cast, b

    # integers to floats
    if at.info.group == TypeGroup.FLOAT and is_numeric(bt):
        cast = ASTNodeCast(b.token, b)
        cast.type = a.type
        return a, cast
    if bt.info.group == TypeGroup.FLOAT and is_numeric(at):
        cast = ASTNodeCast(a.token, a)
        cast.type = b.type
        return cast, b

    # uint to int if int is larger than uint
    if at.info.group == TypeGroup.UINT and bt.info.group == TypeGroup.INT:
        if at.info.size < bt.info.size:
            cast = ASTNodeCast(b.token, b)
            cast.type = a.type
            return a, cast
        if at.info.size < 8 and bt.info.size < 8:
            cast_a = ASTNodeCast(a.token, a)
            cast_b = ASTNodeCast(b.token, b)
            nt = type_table.new(Type(upgrade_int_size[at.info.size]))
            cast_a.type = nt
            cast_b.type = nt
            return cast_a, cast_b
        raise JTLTypeError.from_type(JTLTypeErrorType.UNSAFE_AUTOMATIC_CAST, a.token.location.whole_line(), at, bt)
    if bt.info.group == TypeGroup.UINT and at.info.group == TypeGroup.INT:
        if bt.info.size < at.info.size:
            cast = ASTNodeCast(a.token, a)
            cast.type = b.type
            return cast, b
        if bt.info.size < 8 and at.info.size < 8:
            cast_a = ASTNodeCast(a.token, a)
            cast_b = ASTNodeCast(b.token, b)
            nt = type_table.new(Type(upgrade_int_size[bt.info.size]))
            cast_a.type = nt
            cast_b.type = nt
            return cast_a, cast_b
        raise JTLTypeError.from_type(JTLTypeErrorType.UNSAFE_AUTOMATIC_CAST, a.token.location.whole_line(), at, bt)

    raise JTLTypeError.from_type(JTLTypeErrorType.INCOMPATIBLE_TYPE_GROUP, a.token.location.whole_line(), at, bt)


castable_type_groups = {
    TypeGroup.INT,
    TypeGroup.UINT,
    TypeGroup.FLOAT,
    TypeGroup.BOOL,
    TypeGroup.SYMBOL,
    TypeGroup.POINTER,
}


def can_cast(type_table: TypeTable, a: int, b: int) -> bool:
    at, bt = type_table.get(a), type_table.get(b)
    return at.info.group in castable_type_groups and bt.info.group in castable_type_groups
