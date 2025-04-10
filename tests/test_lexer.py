import unittest

from errors import LexerError, LexerErrorType
from lexer import lex_file
from lexer_types import *


class TestLexer(unittest.TestCase):
    def test_basic(self):
        test_content = "\nABC 123 * + - = */%sum123()32sd_a.\n^&:true false"
        tokens = lex_file("test_file", test_content)
        line_str = list(map(lambda x: x.strip(), test_content.splitlines()))
        expected_tokens = [
            TokenNewLine(location=CodeLocation("test_file", 0, 0, 0, 0)),
            TokenName(location=CodeLocation("test_file", 1, 1, 0, 3), name="ABC"),
            TokenNumberLiteral(location=CodeLocation("test_file", 1, 1, 4, 7), value=123),
            TokenOperator(location=CodeLocation("test_file", 1, 1, 8, 9), op=Operator.TIMES),
            TokenOperator(location=CodeLocation("test_file", 1, 1, 10, 11), op=Operator.PLUS),
            TokenOperator(location=CodeLocation("test_file", 1, 1, 12, 13), op=Operator.MINUS),
            TokenOperator(location=CodeLocation("test_file", 1, 1, 14, 15), op=Operator.ASSIGNMENT),
            TokenOperator(location=CodeLocation("test_file", 1, 1, 16, 17), op=Operator.TIMES),
            TokenOperator(location=CodeLocation("test_file", 1, 1, 17, 18), op=Operator.DIVIDE),
            TokenOperator(location=CodeLocation("test_file", 1, 1, 18, 19), op=Operator.MODULO),
            TokenName(location=CodeLocation("test_file", 1, 1, 19, 25), name="sum123"),
            TokenBracket(location=CodeLocation("test_file", 1, 1, 25, 26), open=True, type=BracketType.ROUND),
            TokenBracket(location=CodeLocation("test_file", 1, 1, 26, 27), open=False, type=BracketType.ROUND),
            TokenNumberLiteral(location=CodeLocation("test_file", 1, 1, 27, 29), value=32),
            TokenName(location=CodeLocation("test_file", 1, 1, 29, 33), name="sd_a"),
            TokenOperator(location=CodeLocation("test_file", 1, 1, 33, 34), op=Operator.DOT),
            TokenNewLine(location=CodeLocation("test_file", 1, 1, 34, 35)),
            TokenOperator(location=CodeLocation("test_file", 2, 2, 0, 1), op=Operator.POINTER),
            TokenOperator(location=CodeLocation("test_file", 2, 2, 1, 2), op=Operator.BITWISE_AND),
            TokenOperator(location=CodeLocation("test_file", 2, 2, 2, 3), op=Operator.COLON),
            TokenBoolLiteral(location=CodeLocation("test_file", 2, 2, 3, 7), value=True),
            TokenBoolLiteral(location=CodeLocation("test_file", 2, 2, 8, 13), value=False),
            TokenNewLine(location=CodeLocation("test_file", 2, 2, 13, 14)),
        ]
        for x, y in zip(expected_tokens, tokens):
            self.assertEqual(x, y)

    def test_multi_char_op(self):
        test_content = "< not<=or>=and!====>"
        tokens = lex_file("test_file", test_content)
        expected_tokens = [
            TokenOperator(CodeLocation("test_file", 0, 0, 0, 1), Operator.LESS),
            TokenOperator(CodeLocation("test_file", 0, 0, 2, 5), Operator.NOT),
            TokenOperator(CodeLocation("test_file", 0, 0, 5, 7), Operator.LESSEQUAL),
            TokenOperator(CodeLocation("test_file", 0, 0, 7, 9), Operator.OR),
            TokenOperator(CodeLocation("test_file", 0, 0, 9, 11), Operator.GREATEREQUAL),
            TokenOperator(CodeLocation("test_file", 0, 0, 11, 14), Operator.AND),
            TokenOperator(CodeLocation("test_file", 0, 0, 14, 16), Operator.NOTEQUAL),
            TokenOperator(CodeLocation("test_file", 0, 0, 16, 18), Operator.EQUAL),
            TokenOperator(CodeLocation("test_file", 0, 0, 18, 19), Operator.ASSIGNMENT),
            TokenOperator(CodeLocation("test_file", 0, 0, 19, 20), Operator.GREATER),
            TokenNewLine(location=CodeLocation("test_file", 0, 0, 20, 21)),
        ]
        for x, y in zip(expected_tokens, tokens):
            self.assertEqual(x, y)

    def test_invalid_char(self):
        """This does not testa all invalid chars just one"""
        test_content = "nameπ😄!a"
        with self.assertRaises(LexerError) as context:
            lex_file("test_file", test_content)
        self.assertEqual(LexerErrorType.INVALID.value[0], context.exception.title)
        self.assertEqual(LexerErrorType.INVALID.value[1], context.exception.message)
        self.assertEqual(5, context.exception.location.col_start)

    def test_unimplemented_operator(self):
        """This does not testa all invalid chars just one"""
        test_content = "nameπ!a"
        with self.assertRaises(LexerError) as context:
            lex_file("test_file", test_content)
        self.assertEqual(LexerErrorType.UNIMPLEMENTED.value[0], context.exception.title)
        self.assertEqual(LexerErrorType.UNIMPLEMENTED.value[1], context.exception.message)
        self.assertEqual(5, context.exception.location.col_start)

    def test_keywords(self):
        test_content = " ".join(map(lambda x: x.value, iter(Keyword)))
        tokens = lex_file("test_file", test_content)
        col = 0
        for kwd, tkn in zip(Keyword, tokens):
            self.assertEqual(kwd, tkn.keyword)
            self.assertEqual(tkn.location.file_name, "test_file")
            self.assertEqual(tkn.location.line_start, 0)
            self.assertEqual(tkn.location.line_stop, 0)
            self.assertEqual(tkn.location.col_start, col)
            self.assertEqual(tkn.location.col_stop, col + len(kwd.value))
            col += 1 + len(kwd.value)

    def test_build_in_types(self):
        test_content = " ".join(map(lambda x: x.value, iter(BuildInType)))
        tokens = lex_file("test_file", test_content)
        col = 0
        for bit, tkn in zip(BuildInType, tokens):
            self.assertEqual(bit, tkn.type)
            self.assertEqual(tkn.location.file_name, "test_file")
            self.assertEqual(tkn.location.line_start, 0)
            self.assertEqual(tkn.location.line_stop, 0)
            self.assertEqual(tkn.location.col_start, col)
            self.assertEqual(tkn.location.col_stop, col + len(bit.value))
            col += 1 + len(bit.value)

    def test_numbers(self):
        test_content = "just_a_name123 123 123.456 123.456.789"
        tokens = lex_file("test_file", test_content)
        expected_tokens = [
            TokenName(CodeLocation("test_file", 0, 0, 0, 14), name="just_a_name123"),
            TokenNumberLiteral(CodeLocation("test_file", 0, 0, 15, 18), value=123),
            TokenNumberLiteral(CodeLocation("test_file", 0, 0, 19, 26), value=123.456),
            TokenNumberLiteral(CodeLocation("test_file", 0, 0, 27, 34), value=123.456),
            TokenNumberLiteral(CodeLocation("test_file", 0, 0, 34, 38), value=0.789),
        ]
        for x, y in zip(expected_tokens, tokens):
            self.assertEqual(x, y)
            if isinstance(x, TokenNumberLiteral):
                self.assertEqual(type(x.value), type(y.value))


if __name__ == '__main__':
    unittest.main()
