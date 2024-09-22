from typing import List, Tuple

import unicodedata as ud

from errors import lexer_error
from lexer_types import *


def lex_file(file_name: str, file_contents: str) -> Tuple[List[Token], bool]:
    """Generate Tokens from a string

    Args:
        file_name: string used as file name in debug info
        file_contents: content of a source file as string

    Returns:
        List of Token objects representing the source file
        boolean indicating success or failure
    """
    def location(length:int = 1) -> CodeLocation:
        """Generate CodeLocation object representing the current location

        Args:
            length: length of token (default 1)

        Returns:
            Current location of the lexer as CodeLocation object
        """
        return CodeLocation(file_name, line_nr, col_nr, length)

    file_lines = file_contents.splitlines()
    tokens: List[Token] = []
    for line_nr, line in enumerate(file_lines):
        it = enumerate(line)
        try:
            col_nr, char = next(it)
            while True:
                if ud.category(char) == 'Zs':
                    pass
                elif ud.category(char) in {'Ll', 'Lu', 'Lo'}:
                    loc = location()
                    try:
                        while ud.category(char) in {'Ll', 'Lu', 'Lo', 'Nd'}:
                            col_nr, char = next(it)
                    except StopIteration:
                        col_nr += 1
                        loc.length = col_nr - loc.col
                        tokens.append(NameToken(loc, line[loc.col:col_nr]))
                        raise StopIteration
                    loc.length = col_nr - loc.col
                    tokens.append(NameToken(loc, line[loc.col:col_nr]))
                    continue
                elif ud.category(char) == 'Nd':
                    loc = location()
                    try:
                        while ud.category(char) in {'Nd'}:
                            col_nr, char = next(it)
                    except StopIteration:
                        col_nr += 1
                        loc.length = col_nr - loc.col
                        tokens.append(LiteralToken(loc, int(line[loc.col:col_nr])))
                        raise StopIteration
                    loc.length = col_nr - loc.col
                    tokens.append(LiteralToken(loc, int(line[loc.col:col_nr])))
                    continue
                elif ud.category(char) == 'Ps':
                    tokens.append(BracketToken(location(), True))
                elif ud.category(char) == 'Pe':
                    tokens.append(BracketToken(location(), False))
                elif char == "=":
                    tokens.append(OperatorToken(location(), Operator.ASSIGNMENT))
                elif char == "+":
                    tokens.append(OperatorToken(location(), Operator.PLUS))
                elif char == "-":
                    tokens.append(OperatorToken(location(), Operator.MINUS))
                elif char == "*":
                    tokens.append(OperatorToken(location(), Operator.TIMES))
                elif char == "/":
                    tokens.append(OperatorToken(location(), Operator.DIVIDE))
                elif char == "%":
                    tokens.append(OperatorToken(location(), Operator.MODULO))
                else:
                    lexer_error("Invalid character", f"'{char}' is not a valid character",
                                location())
                    return [], False
                col_nr, char = next(it)
        except StopIteration:
            pass
        tokens.append(NewLineToken(location()))
    return tokens, True