from typing import Tuple

from ast_types import *
from elaboration_types import Scope
from typing_types import *
from errors import ElaborationError, ElaborationErrorType, JTLTypeError, JTLTypeErrorType


def elaborate_module(nodes: List[ASTNode]) -> Scope:
    scope = Scope(None, {}, [], TypeTable())
    elaborate_scope(scope, nodes, None)

    # take literal types whose types could not be inferred and convert them to discrete types
    for i, t in enumerate(scope.type_table.table):
        if i == 0: # skip sentinel
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
                    # unless the - unary is used all int literals start out as UINTs conveting them to U64 would
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
                case TypeGroup.COMPOUND:
                    raise RuntimeError("Found compound type with none size in Type Table")

    return scope


def elaborate_scope(parent: Scope, nodes: List[ASTNode], returnable: Optional[ASTNodeProcedure]) -> None:
    for i, node in enumerate(nodes):
        elaborate_statement(parent, node, returnable)
        if parent.type_table.get(parent.nodes[-1].type).info.group == TypeGroup.RETURNS and i != (len(nodes) - 1):
            raise ElaborationError.from_type(ElaborationErrorType.UNREACHABLE, nodes[-1].token.location.whole_line())

    for proc in parent.delayed_elaborate:
        proc.elaborated_body = Scope(parent, {}, [], parent.type_table)
        for n in proc.argument_names:
            proc.elaborated_body.names[n.name] = n
        elaborate_scope(proc.elaborated_body, proc.body, proc)
        # TODO make sure all paths return (don't overcomplicate if/else evaluation (don't analyze if all conditions are
        # covered just make sure there is an else (i.e. only check if all branches return or not)


def elaborate_statement(parent: Scope, node: ASTNode, returnable: Optional[ASTNodeProcedure]) -> None:
    expr: ASTNode
    match node:
        case ASTNodeStatement():
            match node.token:
                case TokenKeyword(keyword=Keyword.VARIABLE):
                    assert (node.value is not None)
                    expr = elaborate_declaration(parent, node.value, False, returnable)
                    expr.name.mut = Mutability.MUTABLE
                case TokenKeyword(keyword=Keyword.LET):
                    assert (node.value is not None)
                    expr = elaborate_declaration(parent, node.value, False, returnable)
                    expr.name.mut = Mutability.ONCE
                case TokenKeyword(keyword=Keyword.CONSTANT):
                    assert (node.value is not None)
                    expr = elaborate_declaration(parent, node.value, True, returnable)
                    expr.name.mut = Mutability.CONSTANT
                case _:
                    expr = elaborate_expression(parent, node, False, returnable)
        case _:
            expr = elaborate_expression(parent, node, False, returnable)
    parent.nodes.append(expr)

def elaborate_declaration(parent: Scope, node: ASTNode, constant: bool, returnable: Optional[ASTNodeProcedure]
                          ) -> ASTNodeAssignment:
    # returnable is always unused as it returns nothing and therefore can not be assigned, but I prefer getting a
    # no value error than there is nothing to return from error. (as that might not be true)
    if not (isinstance(node, ASTNodeBinary) and isinstance(node.token, TokenOperator)
            and node.token.op == Operator.ASSIGNMENT):
        raise ElaborationError.from_type(ElaborationErrorType.NO_ASSIGNMENT_DECLARATION,
                                         node.token.location.whole_line())
    n = elaborate_name_and_type(parent, node.left)
    e = elaborate_expression(parent, node.right, constant, returnable)

    if parent.type_table.get(e.type).info.group == TypeGroup.NO_VALUE:
        raise ElaborationError.from_type(ElaborationErrorType.EXPECTED_VALUE, node.token.location.whole_line())

    if not narrow_type(parent.type_table, n.type, e.type):
        raise JTLTypeError.from_type(JTLTypeErrorType.TYPE_MISSMATCH_DECLARATION, node.token.location.whole_line(),
                                     parent.type_table.get(n.type), parent.type_table.get(e.type))
    parent.names[n.name] = n
    new_node = ASTNodeAssignment(node.token, node, n, e)
    new_node.type = n.type
    return new_node


def elaborate_name_and_type(parent: Scope, node: ASTNode) -> Name:
    if isinstance(node, ASTNodeBinary) and node.token.op == Operator.COLON:
        if not (isinstance(node.left, ASTNodeValue) and isinstance(node.left.token, TokenName)):
            raise ElaborationError.from_type(ElaborationErrorType.EXPECTED_NAME_DECLARATION, node.token.location.whole_line())
        return Name(node.left.token.name, node.token.location.whole_line(), elaborate_type(parent, node.right),
                    parent.type_table)
    elif isinstance(node, ASTNodeValue) and isinstance(node.token, TokenName):
        return Name(node.token.name, node.token.location.whole_line(), parent.type_table.new(Type()), parent.type_table)
    else:
        raise ElaborationError.from_type(ElaborationErrorType.EXPECTED_NAME_DECLARATION, node.token.location.whole_line())


def elaborate_type(parent: Scope, node: ASTNode) -> int:
    n_indirections = 0
    while True:
        if isinstance(node, ASTNodeUnary) and node.token.op == Operator.POINTER:
            n_indirections += 1
            node = node.child
        elif isinstance(node, ASTNodeValue) and isinstance(node.token, TokenBuildInType):
            base_type = Type(map_build_in_type_to_type_type[node.token.type])
            for _ in range(n_indirections):
                base_type = TypePointer(parent.type_table.new(base_type), parent.type_table)
            return parent.type_table.new(base_type)
        elif isinstance(node, ASTNodeValue) and isinstance(node.token, TokenName):
            name_obj = parent.lookup(node.token.name)
            if name_obj is None:
                raise ElaborationError.from_type(ElaborationErrorType.UNDECLARED_NAME, node.token.location.whole_line())
            base_type = name_obj.type  # type: ignore
            for _ in range(n_indirections):
                base_type = TypePointer(parent.type_table.new(base_type), parent.type_table)
            return parent.type_table.new(base_type)
        else:
            raise ElaborationError.from_type(ElaborationErrorType.EXPECTED_TYPE, node.token.location.whole_line())


def is_numeric(type: Type) -> bool:
    return type.info.group in (TypeGroup.INT, TypeGroup.FLOAT, TypeGroup.UINT)


def elaborate_expression(parent: Scope, node: ASTNode, constant: bool,
                         returnable: Optional[ASTNodeProcedure]) -> ASTNode:
    match node:
        case ASTNodeBinary():
            node.left = elaborate_expression(parent, node.left, constant, returnable)
            node.right = elaborate_expression(parent, node.right, constant, returnable)
            match node.token.op:
                case Operator.PLUS | Operator.MINUS | Operator.TIMES | Operator.DIVIDE | Operator.MODULO:
                    if isinstance(node.left.type, TypePointer) or isinstance(node.right.type, TypeType):
                        raise JTLTypeError.from_type(JTLTypeErrorType.POINTER_ARITHMETIC,
                                                     node.token.location.whole_line(), node.token.op)
                    for e in (node.left, node.right):
                        if not is_numeric(parent.type_table.get(e.type)):
                            raise JTLTypeError.from_type(JTLTypeErrorType.NOT_NUMERIC, e.token.location.whole_line())

                    node.left, node.right = promote_either(parent.type_table, node.left, node.right)
                    node.type = node.left.type
                    return node
                case Operator.EQUAL | Operator.NOTEQUAL:
                    node.type = parent.type_table.new(Type(TypeType.BOOL))
                    if is_numeric(parent.type_table.get(node.left.type)):
                        if not is_numeric(parent.type_table.get(node.right.type)):
                            raise JTLTypeError.from_type(JTLTypeErrorType.NOT_COMPARABLE,
                                                         node.left.token.location.whole_line(),
                                                         parent.type_table.get(node.left.type),
                                                         parent.type_table.get(node.right.type))
                        node.left, node.right = promote_either(parent.type_table, node.left, node.right)
                        return node
                    at, bt = parent.type_table.get(node.left.type), parent.type_table.get(node.right.type)
                    if at.info.group != bt.info.group:
                        raise JTLTypeError.from_type(JTLTypeErrorType.NOT_COMPARABLE,
                                                     node.left.token.location.whole_line(),
                                                     at, bt)
                    if at.info.group in {TypeGroup.BOOL, TypeGroup.SYMBOL}:
                        return node
                    if isinstance(at, TypePointer) and isinstance(bt, TypePointer):
                        if at == bt:
                            return node
                        else:
                            raise JTLTypeError.from_type(JTLTypeErrorType.POINTER_ARITHMETIC,
                                                         node.left.token.location.whole_line(),
                                                         at.target_type, bt.target_type)

                    if isinstance(at, TypeRecord) and isinstance(bt, TypeRecord) and at == bt:
                        return node

                    raise JTLTypeError.from_type(JTLTypeErrorType.NOT_COMPARABLE, node.token.location.whole_line(),
                                                 at, bt)
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
                case Operator.DOT:
                    raise NotImplementedError()
                case Operator.COLON:
                    raise NotImplementedError()
                case Operator.ASSIGNMENT:
                    # TODO allow for more complex names
                    if not (isinstance(node.left, ASTNodeValue) and isinstance(node.left.token, TokenName)):
                        raise ElaborationError.from_type(ElaborationErrorType.UN_ASSIGNABLE, node.left.token.location)

                    if (var_name := parent.lookup(node.left.token.name)) is None:
                        raise ElaborationError.from_type(ElaborationErrorType.UNDECLARED_PROC,
                                                         node.left.token.location)

                    # TODO think about constant == True

                    if var_name.mut != Mutability.MUTABLE:
                        raise ElaborationError.from_type(ElaborationErrorType.NOT_MUTABLE, node.left.token.location,
                                                         var_name.mut.value)

                    expr = elaborate_expression(parent, node.right, constant, returnable)
                    if not narrow_type(parent.type_table, var_name.type, expr.type):
                        pass

                    assign = ASTNodeAssignment(node.token, node, var_name, expr)
                    assign.type = var_name.type
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
                    if isinstance(operand, ASTNodeValue) and isinstance(operand.token, TokenName):
                        node.type = parent.type_table.new(TypePointer(operand.type, parent.type_table))
                        return node
                    else:
                        raise ElaborationError.from_type(ElaborationErrorType.ADDRESS_OF_UNASSIGNED,
                                                         node.token.location)
                case _:
                    raise RuntimeError(f"Invalid Unary Operator {node}")
        case ASTNodeUnaryRight():
            node.child = operand = elaborate_expression(parent, node.child, constant, returnable)
            ot = parent.type_table.get(operand.type)
            if not isinstance(ot, TypePointer):
                raise JTLTypeError.from_type(JTLTypeErrorType.DEREFERENCE_VALUE, node.token.location)
            node.type = ot.target_type
            return node
        case ASTNodeStatement():
            if node.token.keyword == Keyword.RETURN:
                if returnable is None:
                    raise ElaborationError.from_type(ElaborationErrorType.CAN_NOT_RETURN_FROM_HERE, node.token.location)

                assert node.value is not None
                node.value = elaborate_expression(parent, node.value, constant, returnable)
                rt = parent.type_table.get(returnable.type)
                assert isinstance(rt, TypeProcedure)
                if not narrow_type(parent.type_table, rt.return_type, node.value.type):
                    raise JTLTypeError.from_type(JTLTypeErrorType.TYPE_MISSMATCH_RETURN,
                                                 node.token.location.whole_line(),
                                                 parent.type_table.get(node.value.type),
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
            node.elaborated_body = Scope(parent, {}, [], parent.type_table)
            elaborate_scope(node.elaborated_body, node.body, returnable)
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

            parent.delayed_elaborate.append(node)

            node.argument_names = arguments
            node.type = parent.type_table.new(TypeProcedure(list(map(lambda x: x.type, arguments)), return_type,
                                                            parent.type_table))

            return node
        case ASTNodeScope():
            # TODO maybe scopes can return something
            node.elaborated_body = Scope(parent, {}, [], parent.type_table)
            elaborate_scope(node.elaborated_body, node.nodes, returnable)
            if (len(node.elaborated_body.nodes) > 0
                    and parent.type_table.get(node.elaborated_body.nodes[-1].type).info.group == TypeGroup.RETURNS):
                node.type = node.elaborated_body.nodes[-1].type
            else:
                node.type = parent.type_table.new(Type(TypeType.NO_VALUE))
            return node
        case ASTNodeTupleLike():
            if node.parent is not None:
                # TODO allow more complex situations like module.proc_name() or calling a just defined function
                if not (isinstance(node.parent, ASTNodeValue) and isinstance(node.parent.token, TokenName)):
                    raise ElaborationError.from_type(ElaborationErrorType.NOT_CALLABLE, node.parent.token.location)

                if (proc_name := parent.lookup(node.parent.token.name)) is None:
                    raise ElaborationError.from_type(ElaborationErrorType.UNDECLARED_PROC, node.parent.token.location)

                proc_type = parent.type_table.get(proc_name.type)

                if not isinstance(proc_type, TypeProcedure):
                    raise ElaborationError.from_type(ElaborationErrorType.NOT_CALLABLE, node.parent.token.location)

                if len(node.children) != len(proc_type.arguments):
                    raise ElaborationError.from_type(ElaborationErrorType.WRONG_NUMBER_OF_ARGUMENTS,
                                                     node.token.location.whole_line(),
                                                     len(proc_type.arguments), len(node.children))

                call_arguments = []
                for i, (n, t) in enumerate(zip(node.children, proc_type.arguments)):
                    elab = elaborate_expression(parent, n, constant, returnable)
                    call_arguments.append(elab)
                    # TODO do we want promotion here?
                    if not narrow_type(parent.type_table, t, elab.type):
                        raise ElaborationError.from_type(ElaborationErrorType.WRONG_ARGUMENT_TYPE,
                                                         elab.token.location.whole_line(),
                                                         i+1,
                                                         parent.type_table.get(t),
                                                         parent.type_table.get(elab.type))
                call = ASTNodeCall(node.parent.token, proc_name, call_arguments)

                call.type = proc_type.return_type
                return call
            else:
                # TODO Casts, Tuples, Arrays, Records, (Dicts?)
                raise NotImplementedError()
        case _:
            raise RuntimeError(f"Bad ast node in expression {node}")
    raise RuntimeError("This is here so mypy is happy")


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

    # try to convert literals to more specific type
    if narrow_type(type_table, a.type, b.type):
        return a, b
    elif narrow_type(type_table, b.type, a.type):
        return b, a

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
