from typing import Optional, List

import unicodedata as ud

from errors import LexerError, LexerErrorType
from lexer_types import *
from utils import PeakableIterator


def lex_file(file_name: str, file_contents: str) -> List[Token]:
    """Generate Tokens from a string

    Always returns at least one NewLineToken

    Args:
        file_name: string used as file name in debug info
        file_contents: content of a source file as string

    Returns:
        List of Token objects representing the source file

    Raises:
        LexerError if invalid tokens are in file contents
    """

    def location(length: int = 1, start_col: Optional[int] = None) -> CodeLocation:
        """Generate CodeLocation object representing the current location

        Args:
            length: length of token (default 1)
            start_col: column index of current token (default to location of token in it)

        Returns:
            Current location of the lexer as CodeLocation object
        """
        if it.last is None:
            return CodeLocation(file_name, line_nr, 0, length, "")
        if start_col is None:
            if it.last is None:
                return CodeLocation(file_name, line_nr, 0, length, "")
            else:
                start_col = it.last[0]
        return CodeLocation(file_name, line_nr, start_col, length, line)

    def append_multi_char_op(secondaries: List[str], operator_none: Operator, operators: List[Operator]):
        nxt = it.peak()[1]
        for secondary, operator in zip(secondaries, operators):
            if nxt == secondary:
                it.next()
                tokens.append(TokenOperator(location(2, col_nr), operator))
                return
        else:
            tokens.append(TokenOperator(location(), operator_none))


    file_lines = file_contents.splitlines()

    # ensures the parser gets a NewLineToken
    if len(file_lines) == 0:
        return [TokenNewLine(CodeLocation(file_name, 0, 0, 1, ""))]

    tokens: List[Token] = []
    multi_line_comment = False
    for line_nr, line in enumerate(file_lines):
        it = PeakableIterator(enumerate(line), (len(line), "\n"))
        for col_nr, char in it:
            if multi_line_comment:
                if char == "*" and it.peak()[1] == "/":
                    it.next()
                    multi_line_comment = False
            elif ud.category(char) == 'Zs':
                pass
            elif ud.category(char) in {'Ll', 'Lu', 'Lo'} or char == "_":
                loc = location()
                while ud.category(it.peak()[1]) in {'Ll', 'Lu', 'Lo', 'Nd'} or it.peak()[1] == "_":
                    col_nr, char = it.next()
                loc.length = col_nr - loc.col + 1
                name = line[loc.col:col_nr + 1]
                match name:
                    case "var":
                        tokens.append(TokenKeyword(loc, Keyword.VARIABLE))
                    case "let":
                        tokens.append(TokenKeyword(loc, Keyword.LET))
                    case "const":
                        tokens.append(TokenKeyword(loc, Keyword.CONSTANT))
                    case "and":
                        tokens.append(TokenOperator(loc, Operator.AND))
                    case "or":
                        tokens.append(TokenOperator(loc, Operator.OR))
                    case "not":
                        tokens.append(TokenOperator(loc, Operator.NOT))
                    case "xor":
                        tokens.append(TokenOperator(loc, Operator.BITWISE_XOR))
                    case "if":
                        tokens.append(TokenKeyword(loc, Keyword.IF))
                    case "else":
                        tokens.append(TokenKeyword(loc, Keyword.ELSE))
                    case "then":
                        tokens.append(TokenKeyword(loc, Keyword.THEN))
                    case "proc":
                        tokens.append(TokenKeyword(loc, Keyword.PROCEDURE))
                    case "return":
                        tokens.append(TokenKeyword(loc, Keyword.RETURN))
                    case "record":
                        tokens.append(TokenKeyword(loc, Keyword.RECORD))
                    case "for":
                        tokens.append(TokenKeyword(loc, Keyword.FOR))
                    case "while":
                        tokens.append(TokenKeyword(loc, Keyword.WHILE))
                    case "inout":
                        tokens.append(TokenKeyword(loc, Keyword.INOUT))
                    case "owned":
                        tokens.append(TokenKeyword(loc, Keyword.OWNED))
                    case "ref":
                        tokens.append(TokenKeyword(loc, Keyword.REFERENCE))
                    case "in":
                        tokens.append(TokenKeyword(loc, Keyword.IN))
                    case "cast":
                        tokens.append(TokenKeyword(loc, Keyword.CAST))
                    case "transmute":
                        tokens.append(TokenKeyword(loc, Keyword.TRANSMUTE))
                    case "distinct":
                        tokens.append(TokenKeyword(loc, Keyword.DISTINCT))
                    case "defer":
                        tokens.append(TokenKeyword(loc, Keyword.DEFER))
                    case "true":
                        tokens.append(TokenBoolLiteral(loc, True))
                    case "false":
                        tokens.append(TokenBoolLiteral(loc, False))
                    case "int":
                        tokens.append(TokenBuildInType(loc, BuildInType.INT))
                    case "bool":
                        tokens.append(TokenBuildInType(loc, BuildInType.BOOL))
                    case "str":
                        tokens.append(TokenBuildInType(loc, BuildInType.STRING))
                    case "char":
                        tokens.append(TokenBuildInType(loc, BuildInType.CHAR))
                    case "i8":
                        tokens.append(TokenBuildInType(loc, BuildInType.I8))
                    case "i16":
                        tokens.append(TokenBuildInType(loc, BuildInType.I16))
                    case "i32":
                        tokens.append(TokenBuildInType(loc, BuildInType.I32))
                    case "i64":
                        tokens.append(TokenBuildInType(loc, BuildInType.I64))
                    case "isize":
                        tokens.append(TokenBuildInType(loc, BuildInType.ISIZE))
                    case "usize":
                        tokens.append(TokenBuildInType(loc, BuildInType.USIZE))
                    case "u8":
                        tokens.append(TokenBuildInType(loc, BuildInType.U8))
                    case "u16":
                        tokens.append(TokenBuildInType(loc, BuildInType.U16))
                    case "u32":
                        tokens.append(TokenBuildInType(loc, BuildInType.U32))
                    case "u64":
                        tokens.append(TokenBuildInType(loc, BuildInType.U64))
                    case "uint":
                        tokens.append(TokenBuildInType(loc, BuildInType.UINT))
                    case "f32":
                        tokens.append(TokenBuildInType(loc, BuildInType.F32))
                    case "f64":
                        tokens.append(TokenBuildInType(loc, BuildInType.F64))
                    case "type":
                        tokens.append(TokenBuildInType(loc, BuildInType.TYPE))
                    case _:
                        tokens.append(TokenName(loc, name))
            elif ud.category(char) == 'Nd':
                loc = location()
                while ud.category(it.peak()[1]) in {'Nd'}:
                    col_nr, char = it.next()
                if it.peak()[1] == ".":
                    finish_parsing_float(it, line, loc, tokens)
                else:
                    loc.length = col_nr - loc.col + 1
                    tokens.append(TokenNumberLiteral(loc, int(line[loc.col:col_nr + 1])))
            elif char == "\"":
                loc = location()
                while it.peak()[1] != "\"":
                    col_nr, char = it.next()
                loc.length = col_nr - loc.col + 1
                if it.peak()[1] != "\"":
                    raise LexerError.from_type(LexerErrorType.UNTERMINATED_STRING, loc)
                col_nr, char = it.next()
                tokens.append(TokenStringLiteral(loc, line[loc.col + 1:col_nr]))
            elif char == "(":
                tokens.append(TokenBracket(location(), True, BracketType.ROUND))
            elif char == ")":
                tokens.append(TokenBracket(location(), False, BracketType.ROUND))
            elif char == "[":
                tokens.append(TokenBracket(location(), True, BracketType.SQUARE))
            elif char == "]":
                tokens.append(TokenBracket(location(), False, BracketType.SQUARE))
            elif char == "{":
                tokens.append(TokenBracket(location(), True, BracketType.CURLY))
            elif char == "}":
                tokens.append(TokenBracket(location(), False, BracketType.CURLY))
            elif char == "=":
                append_multi_char_op(["="], Operator.ASSIGNMENT, [Operator.EQUAL])
            elif char == "+":
                tokens.append(TokenOperator(location(), Operator.PLUS))
            elif char == "-":
                append_multi_char_op([">"], Operator.MINUS, operators=[Operator.ARROW])
            elif char == "*":
                tokens.append(TokenOperator(location(), Operator.TIMES))
            elif char == "/":
                if it.peak()[1] == "/":
                    break
                elif it.peak()[1] == "*":
                    multi_line_comment = True
                    for _, c in it:
                        if c == '*' and it.peak()[1] == '/':
                            it.next()
                            multi_line_comment = False
                    break
                else:
                    tokens.append(TokenOperator(location(), Operator.DIVIDE))
            elif char == "%":
                tokens.append(TokenOperator(location(), Operator.MODULO))
            elif char == ",":
                tokens.append(TokenComma(location()))
            elif char == ".":
                if ud.category(it.peak()[1]) in {'Nd'}:
                    loc = location()
                    finish_parsing_float(it, line, loc, tokens)
                else:
                    tokens.append(TokenOperator(location(), Operator.DOT))
            elif char == ":":
                tokens.append(TokenOperator(location(), Operator.COLON))
            elif char == "@":
                tokens.append(TokenOperator(location(), Operator.ADDRESS_OFF))
            elif char == "^":
                tokens.append(TokenOperator(location(), Operator.POINTER))
            elif char == "<":
                append_multi_char_op(["=", "<"], Operator.LESS,
                                     [Operator.LESSEQUAL, Operator.SHIFT_LEFT])
            elif char == ">":
                append_multi_char_op(["=", ">"], Operator.GREATER,
                                     [Operator.GREATEREQUAL, Operator.SHIFT_RIGHT])
            elif char == "!":
                if it.peak()[1] == "=":
                    it.next()
                    tokens.append(TokenOperator(location(2, col_nr), Operator.NOTEQUAL))
                else:
                    raise LexerError.from_type(LexerErrorType.UNIMPLEMENTED, location())
            elif char == "|":
                tokens.append(TokenOperator(location(), Operator.BITWISE_OR))
            elif char == "&":
                tokens.append(TokenOperator(location(), Operator.BITWISE_AND))
            elif char == "~":
                tokens.append(TokenOperator(location(), Operator.BITWISE_NOT))
            else:
                raise LexerError.from_type(LexerErrorType.INVALID, location())
        if not multi_line_comment:
            tokens.append(TokenNewLine(location(start_col=len(line))))
    return tokens


def finish_parsing_float(it: PeakableIterator, line: str, loc: CodeLocation, tokens: List[Token]):
    col_nr, char = it.next()
    while ud.category(it.peak()[1]) in {'Nd'}:
        col_nr, char = it.next()
    loc.length = col_nr - loc.col + 1
    tokens.append(TokenNumberLiteral(loc, float(line[loc.col:col_nr + 1])))
