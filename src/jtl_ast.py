# file name must be jtl_ast and not ast as not to collide with internal python file ast.py
from copy import copy
from typing import Iterable, Tuple

from ast_types import *
from lexer_types import *
from errors import ParserError, ParserErrorType, get_error_with_line_info
from utils import PeakableIterator


class PeakableTokenIterator(PeakableIterator):
    def __init__(self, it: Iterable[Token]):
        super().__init__(it, TokenNewLine(CodeLocation("SENTINEL", 0, 0, 0, 1)))

    def peak(self) -> Token:
        try:
            if self.next_element is None:
                self.next_element = next(self.it)
            return self.next_element
        except StopIteration:
            # iterator is not truly empty but the tokenizer guarantees the last element is a NewLineToken
            # in which acting as if the iterator is empty is fine
            self._empty = True
            assert (isinstance(self.last, TokenNewLine))
            return self.last


def parse_tokens(tokens: List[Token]) -> List[ASTNode]:
    assert (len(tokens) > 0), "Parser called on empty token list, need at least a NewLineToken"
    it = PeakableTokenIterator(tokens)
    return parse_list_of_exprs(it)


def parse_list_of_exprs(it: PeakableTokenIterator, end_on_curly: bool = False) -> List[ASTNode]:
    nodes: List[ASTNode] = []
    while not it.is_empty():
        # check that we moved forward
        if not it.has_moved():
            raise RuntimeError(f"Compiler error: parser did not consume next token. Current Token: {it.last}")

        # skip empty lines
        if isinstance(it.peak(), TokenNewLine):
            it.next()
        else:
            if (n := parse_expr(it)) is not None:
                nodes.append(n)
                match tok := it.peak():
                    case TokenNewLine():
                        pass
                    case TokenBracket(open=False, type=BracketType.CURLY):
                        if end_on_curly:
                            break
                        else:
                            raise ParserError.from_type(ParserErrorType.UNOPENED_BRACKET, tok.location)
                    case TokenBracket(open=False):
                        raise ParserError.from_type(ParserErrorType.UNOPENED_BRACKET, tok.location)
                    case TokenKeyword(keyword=Keyword.THEN):
                        raise ParserError.from_type(ParserErrorType.UNEXPECTED_THEN, tok.location)
                    case TokenComma():
                        raise ParserError.from_type(ParserErrorType.UNEXPECTED_COMMA, tok.location)
                    case _:
                        raise ParserError.from_type(ParserErrorType.UNEXPECTED_TOKEN, tok.location)
            else:
                match it.peak():
                    case TokenBracket(open=False, type=BracketType.CURLY):
                        if end_on_curly:
                            break
                    case _:
                        raise ParserError.from_type(ParserErrorType.UNEXPECTED_TOKEN, it.peak().location)
    return nodes


def parse_expr(it: PeakableTokenIterator, bp_in: float = 0.0, ignore_new_line: bool = False) -> Optional[ASTNode]:
    if ignore_new_line:
        while isinstance(it.peak(), TokenNewLine):
            if it.is_empty():  # empty PeakableTokenIterator will endlessly return TokenNewLine on peak()
                return None
            it.next()

    match it.peak():
        case TokenBracket(open=False, type=BracketType.CURLY):
            return None

    if it.is_empty():
        return None

    lhs: ASTNode

    match lhs_token := it.next():
        case TokenNumberLiteral() | TokenName() | TokenStringLiteral() | TokenBoolLiteral() | TokenBuildInType():
            lhs = ASTNodeValue(lhs_token)
        case TokenKeyword(keyword=Keyword.VARARGS):
            lhs = ASTNodeVarArgs(lhs_token)
        case TokenOperator(op=Operator.POINTER) if isinstance(nxt := it.peak(),
                                                              TokenKeyword) and nxt.keyword == Keyword.CONSTANT:
            it.next()
            bp_right = UnaryBindingPower[Operator.CONSTANT_POINTER]
            rhs = parse_expr(it, bp_right, ignore_new_line)
            if rhs is None:
                return None
            lhs = ASTNodeUnary(TokenOperator(lhs_token.location, op=Operator.CONSTANT_POINTER), rhs)
        case (TokenOperator(op=Operator.PLUS) | TokenOperator(op=Operator.MINUS)
              | TokenOperator(op=Operator.ADDRESS_OFF) | TokenOperator(op=Operator.POINTER)
              | TokenOperator(op=Operator.NOT) | TokenOperator(op=Operator.BITWISE_NOT)):
            bp_right = UnaryBindingPower[lhs_token.op]
            rhs = parse_expr(it, bp_right, ignore_new_line)
            if rhs is None:
                return None
            lhs = ASTNodeUnary(lhs_token, rhs)
        case (TokenKeyword(keyword=Keyword.RETURN) | TokenKeyword(keyword=Keyword.CONSTANT)
              | TokenKeyword(keyword=Keyword.VARIABLE) | TokenKeyword(keyword=Keyword.LET)
              | TokenKeyword(keyword=Keyword.DEFER) | TokenKeyword(keyword=Keyword.DISTINCT)):
            child = parse_expr(it, 0.0, ignore_new_line)
            return ASTNodeStatement(lhs_token, child)
        case TokenKeyword(keyword=Keyword.IF):
            if (condition := parse_expr(it)) is None:
                raise ParserError.from_type(ParserErrorType.EXPECTED_CONDITION, lhs_token.location)
            match then_token := it.peak():
                case TokenKeyword(keyword=Keyword.THEN):
                    pass
                case _:
                    raise ParserError.from_type(ParserErrorType.EXPECTED_THEN, it.peak().location)
            it.next()
            if (body := parse_expr(it)) is None:
                raise ParserError.from_type(ParserErrorType.EXPECTED_EXPRESSION, then_token.location)
            match else_token := it.peak():
                case TokenKeyword(keyword=Keyword.ELSE):
                    it.next()
                    if (else_body := parse_expr(it)) is None:
                        raise ParserError.from_type(ParserErrorType.EXPECTED_EXPRESSION, else_token.location)
                    lhs = ASTNodeIf(lhs_token, lhs_token.location.span(condition.get_location()), condition, body,
                                    else_token.location, else_body)
                case TokenNewLine():
                    lhs = ASTNodeIf(lhs_token, lhs_token.location.span(condition.get_location()), condition, body,
                                    None, None)
                case _:
                    raise ParserError.from_type(ParserErrorType.EXPECTED_END, else_token.location)
        case TokenKeyword(keyword=Keyword.WHILE):
            if (condition := parse_expr(it)) is None:
                raise ParserError.from_type(ParserErrorType.EXPECTED_CONDITION, lhs_token.location)
            match curly_bracket := it.peak():
                case TokenBracket(open=True, type=BracketType.CURLY):
                    pass
                case _:
                    raise ParserError.from_type(ParserErrorType.EXPECTED_BODY_WHILE, curly_bracket.location)
            it.next()
            nodes = parse_list_of_exprs(it, end_on_curly=True)
            match it.peak():
                case TokenBracket(open=False, type=BracketType.CURLY):
                    it.next()
                case _:
                    raise ParserError.from_type(ParserErrorType.UNCLOSED_BRACKET, curly_bracket.location)
            lhs = ASTNodeWhile(lhs_token, lhs_token.location.span(condition.get_location()), condition, nodes)
        case TokenKeyword(keyword=Keyword.CAST) | TokenKeyword(keyword=Keyword.TRANSMUTE):
            match bracket := it.peak():
                case TokenBracket(open=True, type=BracketType.ROUND):
                    it.next()
                    nodes, span = parse_tuple_like(it, bracket)
                    assert it.last is not None
                    lhs = ASTNodeTupleLike(lhs_token, lhs_token.location.span(span), nodes, None)
                case _:
                    raise ParserError.from_type(ParserErrorType.EXPECTED_BRACKET, it.peak().location)
        case TokenKeyword(keyword=Keyword.RECORD):
            match it.peak():
                case TokenBracket(open=True, type=BracketType.CURLY):
                    it.next()
                case _:
                    raise ParserError.from_type(ParserErrorType.EXPECTED_BODY_RECORD, it.peak().location)
            nodes = parse_list_of_exprs(it, end_on_curly=True)
            match end_token := it.peak():
                case TokenBracket(open=False, type=BracketType.CURLY):
                    it.next()
                case _:
                    raise ParserError.from_type(ParserErrorType.UNCLOSED_BRACKET, end_token.location)
            lhs = ASTNodeRecord(lhs_token, nodes, lhs_token.location.span(end_token.location))
        case TokenBracket(open=True, type=BracketType.ROUND):
            nodes, span = parse_tuple_like(it, lhs_token)
            if len(nodes) == 0:
                assert it.last is not None
                raise ParserError.from_type(ParserErrorType.EMPTY_TUPLE, lhs_token.location.span(span))
            elif len(nodes) == 1:
                lhs = nodes[0]
            else:
                assert it.last is not None
                lhs = ASTNodeTupleLike(lhs_token, lhs_token.location.span(span), nodes, None)
        case TokenBracket(open=True, type=BracketType.SQUARE):
            nodes, span = parse_tuple_like(it, lhs_token)
            assert it.last is not None
            lhs = ASTNodeTupleLike(lhs_token, lhs_token.location.span(span), nodes, None)
        case TokenBracket(open=True, type=BracketType.CURLY):
            nodes = parse_list_of_exprs(it, end_on_curly=True)
            match end_token := it.peak():
                case TokenBracket(open=False, type=BracketType.CURLY):
                    it.next()
                case _:
                    raise ParserError.from_type(ParserErrorType.UNCLOSED_BRACKET, end_token.location)
            lhs = ASTNodeScope(lhs_token, nodes, lhs_token.location.span(end_token.location))
        case TokenKeyword(keyword=Keyword.PROCEDURE) | TokenKeyword(keyword=Keyword.EXTERNAL):
            if lhs_token.keyword == Keyword.EXTERNAL:
                if isinstance(procedure := it.peak(), TokenKeyword) and procedure.keyword == Keyword.PROCEDURE:
                    it.next()
                    external = True
                else:
                    raise ParserError.from_type(ParserErrorType.EXTERNAL_FREESTANDING, lhs_token.location)
            else:
                external = False
            match open_bracket := it.peak():
                case TokenBracket(open=True, type=BracketType.ROUND):
                    it.next()
                    arguments, _ = parse_tuple_like(it, open_bracket)
                case _:
                    raise ParserError.from_type(ParserErrorType.EXPECTED_BRACKET, open_bracket.location)
            match arrow := it.peak():
                case TokenOperator(op=Operator.ARROW):
                    it.next()
                    if (type_expr := parse_expr(it, 0.0, ignore_new_line)) is None:
                        raise ParserError.from_type(ParserErrorType.EXPECTED_EXPRESSION, arrow.location)
                case _:
                    type_expr = None
            assert it.last is not None
            header_location = lhs_token.location.span(it.last.location)
            if external:
                assert it.last is not None
                lhs = ASTNodeProcedureStub(lhs_token, header_location, arguments, type_expr)
            else:
                match it.peak():
                    case TokenBracket(open=True, type=BracketType.CURLY):
                        it.next()
                    case _:
                        raise ParserError.from_type(ParserErrorType.EXPECTED_BODY, it.peak().location)
                proc_body = parse_list_of_exprs(it, end_on_curly=True)
                match end_token := it.peak():
                    case TokenBracket(open=False, type=BracketType.CURLY):
                        it.next()
                    case _:
                        raise ParserError.from_type(ParserErrorType.UNCLOSED_BRACKET, end_token.location)
                lhs = ASTNodeProcedure(lhs_token, header_location, arguments, type_expr, proc_body)
        case _:
            raise ParserError.from_type(ParserErrorType.EXPECTED_ATOM, lhs_token.location)

    while True:
        match op_token := it.peak():
            case TokenOperator(op=Operator.POINTER):
                bp_left = UnaryBindingPower[op_token.op]
                if bp_left < bp_in:
                    break
                it.next()
                lhs = ASTNodeUnaryRight(op_token, lhs)
            case TokenOperator():
                bp_left, bp_right = OperatorBindingPower[op_token.op]
                if bp_left < bp_in:
                    break
                it.next()
                if (rhs := parse_expr(it, bp_right, ignore_new_line)) is None:
                    return None
                lhs = ASTNodeBinary(op_token, lhs, rhs)
            case TokenBracket(open=True, type=BracketType.CURLY):
                break
            case TokenBracket(open=True, type=bracket_type):
                if CallBindingPower < bp_in:
                    break
                it.next()
                nodes, span = parse_tuple_like(it, op_token)
                lhs = ASTNodeTupleLike(op_token, lhs_token.location.span(span), nodes, lhs)
            case (TokenBracket(open=False) | TokenComma() | TokenKeyword(keyword=Keyword.THEN)
                  | TokenKeyword(keyword=Keyword.ELSE)):
                break
            case TokenNewLine():
                if not ignore_new_line:
                    break
                elif it.is_empty():
                    return None
                else:
                    it.next()
            case _:
                # This is an array type
                if (isinstance(lhs, ASTNodeTupleLike) and isinstance(lhs.token, TokenBracket)
                        and lhs.token.type == BracketType.SQUARE):
                    if (rhs := parse_expr(it, UnaryBindingPower[Operator.POINTER], ignore_new_line)) is None:
                        return None
                    lhs = ASTNodeArrayType(lhs.token, lhs.location.span(rhs.get_location()), lhs.children, rhs)
                else:
                    raise ParserError.from_type(ParserErrorType.EXPECTED_OPERATOR, op_token.location)

    return lhs


def parse_tuple_like(it: PeakableTokenIterator, token: TokenBracket) -> Tuple[List[ASTNode], CodeLocation]:
    def skip_new_lines():
        while isinstance(it.peak(), TokenNewLine):
            if it.is_empty():
                raise ParserError.from_type(ParserErrorType.UNCLOSED_BRACKET, token.location)
            it.next()

    nodes: List[ASTNode] = []
    skip_new_lines()
    assert it.last is not None
    start_location = it.last.location
    end_location = start_location
    match it.peak():
        case TokenBracket(open=False, type=token.type):
            end_token = it.next()
            return [], it.last.location.span(end_token.location)
        case _:
            pass
    while True:
        if (n := parse_expr(it, bp_in=0, ignore_new_line=True)) is not None:
            nodes.append(n)
            match tok := it.peak():
                case TokenBracket(open=False, type=token.type):
                    end_location = it.next().location
                    break
                case TokenComma():
                    it.next()
                    # handle trailing comma
                    skip_new_lines()
                    match it.peak():
                        case TokenBracket(open=False, type=token.type):
                            it.next()
                            break
                        case _:
                            pass
                case _:
                    raise ParserError.from_type(ParserErrorType.UNEXPECTED_TUPLE_TERMINATOR, tok.location)
        else:
            raise ParserError.from_type(ParserErrorType.UNCLOSED_BRACKET, token.location)
    return nodes, start_location.span(end_location)
