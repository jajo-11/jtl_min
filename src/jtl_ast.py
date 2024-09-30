# file name must be jtl_ast and not ast as not to collide with internal python file ast.py
import unittest
from typing import Tuple, Iterable

from ast_types import *
from lexer_types import *
from errors import ParserError, JTLError, LexerError, ParserErrorType
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
    nodes: List[ASTNode] = []
    assert (len(tokens) > 0), "Parser called on empty token list, need at least a NewLineToken"
    it = PeakableTokenIterator(tokens)
    while not it.is_empty():
        # skip empty lines
        if isinstance(it.peak(), TokenNewLine):
            it.next()
        else:
            if (n := parse_expr(it)) is not None:
                nodes.append(n)
                match tok := it.peak():
                    case TokenNewLine():
                        pass
                    case TokenBracket(open=False):
                        raise ParserError.from_type(ParserErrorType.UNOPENED_BRACKET, tok.location)
                    case _:
                        raise ParserError.from_type(ParserErrorType.UNEXPECTED_TOKEN, tok.location)
    return nodes


def parse_expr(it: PeakableTokenIterator, bp_in: float = 0.0, bracket: bool = False) -> Optional[ASTNode]:
    if bracket:
        while isinstance(it.peak(), TokenNewLine):
            if it.is_empty():  # empty PeakableTokenIterator will endlessly return TokenNewLine on peak()
                return None
            it.next()

    if it.is_empty():
        return None

    match lhs_token := it.next():
        case TokenName(name="if"):
            ...
        case TokenLiteral() | TokenName():
            lhs: ASTNode = ASTNodeValue(lhs_token, Type())
        case TokenOperator(op=Operator.PLUS) | TokenOperator(op=Operator.MINUS):
            bp_right = UnaryBindingPower[lhs_token.op]
            rhs = parse_expr(it, bp_right, bracket)
            if rhs is None:
                return None
            lhs = ASTNodeUnary(lhs_token, Type(), rhs)
        case TokenBracket(open=True, type=BracketType.ROUND):
            if (tmp := parse_expr(it, bracket=True)) is None:
                raise ParserError.from_type(ParserErrorType.UNCLOSED_BRACKET, lhs_token.location)
            else:
                lhs = tmp
            match it.next():
                case TokenBracket(open=False, type=BracketType.ROUND):
                    pass
                case _:
                    raise ParserError.from_type(ParserErrorType.UNCLOSED_BRACKET, lhs_token.location)
        case TokenBracket(open=True, type=BracketType.SQUARE):
            if (new_node := parse_tuple_like(it, lhs_token, None)) is not None:
                lhs = new_node
            else:
                return None
        case _:
            raise ParserError.from_type(ParserErrorType.EXPECTED_ATOM, lhs_token.location)

    while True:
        match op_token := it.peak():
            case TokenOperator():
                bp_left, bp_right = OperatorBindingPower[op_token.op]
                if bp_left < bp_in:
                    break
                it.next()
                if (rhs := parse_expr(it, bp_right, bracket)) is None:
                    return None
                lhs = ASTNodeBinary(op_token, Type(), lhs, rhs)
            case TokenBracket(open=True, type=BracketType.CURLY):
                ...
            case TokenBracket(open=True, type=bracket_type):
                if CallBindingPower < bp_in:
                    break
                it.next()
                if (new_node := parse_tuple_like(it, op_token, lhs)) is not None:
                    lhs = new_node
                else:
                    return None
            case TokenBracket(open=False):
                break
            case TokenNewLine():
                if not bracket:
                    break
                elif it.is_empty():
                    return None
                else:
                    it.next()
            case TokenComma():
                break
            case _:
                raise ParserError.from_type(ParserErrorType.EXPECTED_OPERATOR, op_token.location)

    return lhs


def parse_tuple_like(it: PeakableTokenIterator, token: TokenBracket, parent: Optional[ASTNode]) -> Optional[ASTNode]:
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
            return ASTNodeTupleLike(token, Type(), nodes, parent)
        case _:
            pass
    while True:
        if (n := parse_expr(it, bp_in=0, bracket=True)) is not None:
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
    return ASTNodeTupleLike(token, Type(), nodes, parent)
