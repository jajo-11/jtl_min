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
            TokenNewLine(location=CodeLocation("test_file", 0, 0, 1, "")),
            TokenName(location=CodeLocation("test_file", 1, 0, 3, line_str[1]), name="ABC"),
            TokenNumberLiteral(location=CodeLocation("test_file", 1, 4, 3, line_str[1]), value=123),
            TokenOperator(location=CodeLocation("test_file", 1, 8, 1, line_str[1]), op=Operator.TIMES),
            TokenOperator(location=CodeLocation("test_file", 1, 10, 1, line_str[1]), op=Operator.PLUS),
            TokenOperator(location=CodeLocation("test_file", 1, 12, 1, line_str[1]), op=Operator.MINUS),
            TokenOperator(location=CodeLocation("test_file", 1, 14, 1, line_str[1]), op=Operator.ASSIGNMENT),
            TokenOperator(location=CodeLocation("test_file", 1, 16, 1, line_str[1]), op=Operator.TIMES),
            TokenOperator(location=CodeLocation("test_file", 1, 17, 1, line_str[1]), op=Operator.DIVIDE),
            TokenOperator(location=CodeLocation("test_file", 1, 18, 1, line_str[1]), op=Operator.MODULO),
            TokenName(location=CodeLocation("test_file", 1, 19, 6, line_str[1]), name="sum123"),
            TokenBracket(location=CodeLocation("test_file", 1, 25, 1, line_str[1]), open=True, type=BracketType.ROUND),
            TokenBracket(location=CodeLocation("test_file", 1, 26, 1, line_str[1]), open=False, type=BracketType.ROUND),
            TokenNumberLiteral(location=CodeLocation("test_file", 1, 27, 2, line_str[1]), value=32),
            TokenName(location=CodeLocation("test_file", 1, 29, 4, line_str[1]), name="sd_a"),
            TokenOperator(location=CodeLocation("test_file", 1, 33, 1, line_str[1]), op=Operator.DOT),
            TokenNewLine(location=CodeLocation("test_file", 1, 34, 1, line_str[1])),
            TokenOperator(location=CodeLocation("test_file", 2, 0, 1, line_str[2]), op=Operator.POINTER),
            TokenOperator(location=CodeLocation("test_file", 2, 1, 1, line_str[2]), op=Operator.ADDRESS_OFF),
            TokenOperator(location=CodeLocation("test_file", 2, 2, 1, line_str[2]), op=Operator.COLON),
            TokenBoolLiteral(location=CodeLocation("test_file", 2, 3, 4, line_str[2]), value=True),
            TokenBoolLiteral(location=CodeLocation("test_file", 2, 8, 5, line_str[2]), value=False),
            TokenNewLine(location=CodeLocation("test_file", 2, 13, 1, line_str[2])),
        ]
        for x, y in zip(expected_tokens, tokens):
            self.assertEqual(x, y)

    def test_multi_char_op(self):
        test_content = "< not<=or>=and!====>"
        tokens = lex_file("test_file", test_content)
        expected_tokens = [
            TokenOperator(CodeLocation("test_file", 0, 0, 1, test_content), Operator.LESS),
            TokenOperator(CodeLocation("test_file", 0, 2, 3, test_content), Operator.NOT),
            TokenOperator(CodeLocation("test_file", 0, 5, 2, test_content), Operator.LESSEQUAL),
            TokenOperator(CodeLocation("test_file", 0, 7, 2, test_content), Operator.OR),
            TokenOperator(CodeLocation("test_file", 0, 9, 2, test_content), Operator.GREATEREQUAL),
            TokenOperator(CodeLocation("test_file", 0, 11, 3, test_content), Operator.AND),
            TokenOperator(CodeLocation("test_file", 0, 14, 2, test_content), Operator.NOTEQUAL),
            TokenOperator(CodeLocation("test_file", 0, 16, 2, test_content), Operator.EQUAL),
            TokenOperator(CodeLocation("test_file", 0, 18, 1, test_content), Operator.ASSIGNMENT),
            TokenOperator(CodeLocation("test_file", 0, 19, 1, test_content), Operator.GREATER),
            TokenNewLine(location=CodeLocation("test_file", 0, 20, 1, test_content)),
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
        self.assertEqual(5, context.exception.location.col)

    def test_unimplemented_operator(self):
        """This does not testa all invalid chars just one"""
        test_content = "nameπ!a"
        with self.assertRaises(LexerError) as context:
            lex_file("test_file", test_content)
        self.assertEqual(LexerErrorType.UNIMPLEMENTED.value[0], context.exception.title)
        self.assertEqual(LexerErrorType.UNIMPLEMENTED.value[1], context.exception.message)
        self.assertEqual(5, context.exception.location.col)

    def test_keywords(self):
        test_content = " ".join(map(lambda x: x.value, iter(Keyword)))
        tokens = lex_file("test_file", test_content)
        col = 0
        for kwd, tkn in zip(Keyword, tokens):
            self.assertEqual(kwd, tkn.keyword)
            self.assertEqual(tkn.location.file_name, "test_file")
            self.assertEqual(tkn.location.line, 0)
            self.assertEqual(tkn.location.col, col)
            self.assertEqual(tkn.location.length, len(kwd.value))
            self.assertEqual(tkn.location.line_str, test_content)
            col += 1 + len(kwd.value)

    def test_build_in_types(self):
        test_content = " ".join(map(lambda x: x.value, iter(BuildInType)))
        tokens = lex_file("test_file", test_content)
        col = 0
        for bit, tkn in zip(BuildInType, tokens):
            self.assertEqual(bit, tkn.type)
            self.assertEqual(tkn.location.file_name, "test_file")
            self.assertEqual(tkn.location.line, 0)
            self.assertEqual(tkn.location.col, col)
            self.assertEqual(tkn.location.length, len(bit.value))
            self.assertEqual(tkn.location.line_str, test_content)
            col += 1 + len(bit.value)

    def test_numbers(self):
        test_content = "just_a_name123 123 123.456 123.456.789"
        tokens = lex_file("test_file", test_content)
        expected_tokens = [
            TokenName(CodeLocation("test_file", 0, 0, 14, test_content), name="just_a_name123"),
            TokenNumberLiteral(CodeLocation("test_file", 0, 15, 3, test_content), value=123),
            TokenNumberLiteral(CodeLocation("test_file", 0, 19, 7, test_content), value=123.456),
            TokenNumberLiteral(CodeLocation("test_file", 0, 27, 7, test_content), value=123.456),
            TokenNumberLiteral(CodeLocation("test_file", 0, 34, 4, test_content), value=0.789),
        ]
        for x, y in zip(expected_tokens, tokens):
            self.assertEqual(x, y)
            if isinstance(x, TokenNumberLiteral):
                self.assertEqual(type(x.value), type(y.value))


if __name__ == '__main__':
    unittest.main()
