from typing import List, Tuple, Iterable, Optional

from ast_types import ASTNode
from errors import parser_error
from lexer_types import *

class PeakableIterator:
    def __init__(self, it: Iterable[Token]) -> None:
        self.it = iter(it)
        self.last: Optional[Token] = None
        self.next_element: Optional[Token] = None

    def __iter__(self):
        return self

    def next(self) -> Token:
        return self.__next__()

    def __next__(self) -> Token:
        if self.next_element is not None:
            tmp = self.next_element
            self.next_element = None
            return tmp
        else:
            return next(self.it)

    def peak(self) -> Token:
        try:
            if self.next_element is None:
                self.next_element = next(self.it)
            return self.next_element
        except StopIteration:
            assert(isinstance(self.last, NewLineToken))
            return self.last


def parse_tokens(tokens: List[Token]) -> Tuple[List[ASTNode], bool]:
    nodes: List[ASTNode] = []
    it = PeakableIterator(tokens)
    return nodes, True


def parse_expr(it: PeakableIterator) -> Tuple[ASTNode, bool]:
    match lhs := it.next():
        case LiteralToken() | NameToken():
            pass
        case _:
            parser_error("Expected Atom", "Expected a Literal or Name at the beginning of an expression",
                         lhs.location)
            return [], False
    return nodes, True

