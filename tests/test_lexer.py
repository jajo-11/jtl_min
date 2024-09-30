import unittest

from errors import LexerError, LexerErrorType
from lexer import lex_file
from lexer_types import *


class TestLexer(unittest.TestCase):
    def test_basic(self):
        test_content = "\nABC 123 * + - = */%sum123()32sda."
        tokens = lex_file("test_file", test_content)
        line_str = test_content.strip()
        expected_tokens = [
            TokenNewLine(location=CodeLocation("test_file", 0, 0, 1, "")),
            TokenName(location=CodeLocation("test_file", 1, 0, 3, line_str), name="ABC"),
            TokenLiteral(location=CodeLocation("test_file", 1, 4, 3, line_str), value=123),
            TokenOperator(location=CodeLocation("test_file", 1, 8, 1, line_str), op=Operator.TIMES),
            TokenOperator(location=CodeLocation("test_file", 1, 10, 1, line_str), op=Operator.PLUS),
            TokenOperator(location=CodeLocation("test_file", 1, 12, 1, line_str), op=Operator.MINUS),
            TokenOperator(location=CodeLocation("test_file", 1, 14, 1, line_str), op=Operator.ASSIGNMENT),
            TokenOperator(location=CodeLocation("test_file", 1, 16, 1, line_str), op=Operator.TIMES),
            TokenOperator(location=CodeLocation("test_file", 1, 17, 1, line_str), op=Operator.DIVIDE),
            TokenOperator(location=CodeLocation("test_file", 1, 18, 1, line_str), op=Operator.MODULO),
            TokenName(location=CodeLocation("test_file", 1, 19, 6, line_str), name="sum123"),
            TokenBracket(location=CodeLocation("test_file", 1, 25, 1, line_str), open=True, type=BracketType.ROUND),
            TokenBracket(location=CodeLocation("test_file", 1, 26, 1, line_str), open=False, type=BracketType.ROUND),
            TokenLiteral(location=CodeLocation("test_file", 1, 27, 2, line_str), value=32),
            TokenName(location=CodeLocation("test_file", 1, 29, 3, line_str), name="sda"),
            TokenOperator(location=CodeLocation("test_file", 1, 32, 1, line_str), op=Operator.DOT),
            TokenNewLine(location=CodeLocation("test_file", 1, 33, 1, line_str)),
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


if __name__ == '__main__':
    unittest.main()
