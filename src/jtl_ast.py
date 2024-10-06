# file name must be jtl_ast and not ast as not to collide with internal python file ast.py
from typing import Iterable

from ast_types import *
from lexer_types import *
from errors import ParserError, ParserErrorType
from utils import PeakableIterator


class PeakableTokenIterator(PeakableIterator):
    def __init__(self, it: Iterable[Token]):
        super().__init__(it, TokenNewLine(CodeLocation("SENTINEL", 0, 0, 1, "")))

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
        case(TokenOperator(op=Operator.PLUS) | TokenOperator(op=Operator.MINUS)
             | TokenOperator(op=Operator.ADDRESS_OFF) | TokenOperator(op=Operator.POINTER)):
            bp_right = UnaryBindingPower[lhs_token.op]
            rhs = parse_expr(it, bp_right, ignore_new_line)
            if rhs is None:
                return None
            lhs = ASTNodeUnary(lhs_token, rhs)
        case (TokenKeyword(keyword=Keyword.RETURN) | TokenKeyword(keyword=Keyword.CONSTANT)
              | TokenKeyword(keyword=Keyword.VARIABLE) | TokenKeyword(keyword=Keyword.LET)):
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
                    lhs = ASTNodeIf(lhs_token, condition, body, else_body)
                case TokenNewLine():
                    lhs = ASTNodeIf(lhs_token, condition, body, None)
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
            match end_token := it.peak():
                case TokenBracket(open=False, type=BracketType.CURLY):
                    it.next()
                case _:
                    raise ParserError.from_type(ParserErrorType.UNCLOSED_BRACKET, curly_bracket.location)
            lhs = ASTNodeWhile(lhs_token, condition, nodes)
        case TokenKeyword(keyword=Keyword.RECORD):
            match bracket := it.peak():
                case TokenBracket(open=True, type=BracketType.ROUND):
                    it.next()
                    lhs = parse_tuple_like(it, bracket, ASTNode(lhs_token))
                case _:
                    raise ParserError.from_type(ParserErrorType.EXPECTED_BRACKET, it.peak().location)
        case TokenBracket(open=True, type=BracketType.ROUND):
            if (tmp := parse_expr(it, ignore_new_line=True)) is None:
                raise ParserError.from_type(ParserErrorType.UNCLOSED_BRACKET, lhs_token.location)
            else:
                lhs = tmp
            match it.next():
                case TokenBracket(open=False, type=BracketType.ROUND):
                    pass
                case _:
                    raise ParserError.from_type(ParserErrorType.UNCLOSED_BRACKET, lhs_token.location)
        case TokenBracket(open=True, type=BracketType.SQUARE):
            lhs = parse_tuple_like(it, lhs_token, None)
        case TokenBracket(open=True, type=BracketType.CURLY):
            nodes = parse_list_of_exprs(it, end_on_curly=True)
            match end_token := it.peak():
                case TokenBracket(open=False, type=BracketType.CURLY):
                    it.next()
                case _:
                    raise ParserError.from_type(ParserErrorType.UNCLOSED_BRACKET, end_token.location)
            lhs = ASTNodeScope(lhs_token, nodes)
        case TokenKeyword(keyword=Keyword.PROCEDURE):
            match open_bracket := it.peak():
                case TokenBracket(open=True, type=BracketType.ROUND):
                    it.next()
                    tuple_like = parse_tuple_like(it, open_bracket, None)
                case _:
                    raise ParserError.from_type(ParserErrorType.EXPECTED_BRACKET, open_bracket.location)
            match colon := it.peak():
                case TokenOperator(op=Operator.COLON):
                    it.next()
                    if (type_expr := parse_expr(it, 0.0, ignore_new_line)) is None:
                        raise ParserError.from_type(ParserErrorType.EXPECTED_EXPRESSION, colon.location)
                case _:
                    type_expr = None
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
            lhs = ASTNodeProcedure(lhs_token, tuple_like, type_expr, proc_body)
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
                lhs = parse_tuple_like(it, op_token, lhs)
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
                raise ParserError.from_type(ParserErrorType.EXPECTED_OPERATOR, op_token.location)

    return lhs


def parse_tuple_like(it: PeakableTokenIterator, token: TokenBracket, parent: Optional[ASTNode]) -> ASTNodeTupleLike:
    def skip_new_lines():
        while isinstance(it.peak(), TokenNewLine):
            if it.is_empty():
                raise ParserError.from_type(ParserErrorType.UNCLOSED_BRACKET, token.location)
            it.next()

    nodes: List[ASTNode] = []
    skip_new_lines()
    match it.peak():
        case TokenBracket(open=False, type=token.type):
            it.next()
            return ASTNodeTupleLike(token, nodes, parent)
        case _:
            pass
    while True:
        if (n := parse_expr(it, bp_in=0, ignore_new_line=True)) is not None:
            nodes.append(n)
            match tok := it.peak():
                case TokenBracket(open=False, type=token.type):
                    it.next()
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
    return ASTNodeTupleLike(token, nodes, parent)
