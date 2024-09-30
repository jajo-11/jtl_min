import unittest

from errors import ParserError, ParserErrorType
from jtl_ast import parse_tokens


class TestParser(unittest.TestCase):
    def test_basic_expession(self):
        from lexer import lex_file

        tokens = lex_file("test_file", "sum: int = 3 * (1 +\n 2) * -2")
        exprs = parse_tokens(tokens)
        self.assertEqual(1, len(exprs))
        self.assertIsNotNone(exprs[0])
        self.assertEqual(str(exprs[0]), "(= (: sum int) (* (* 3 (+ 1 2)) (- 2)))")

    def test_tuple_like(self):
        from lexer import lex_file

        tokens = lex_file("test_file",
                          """abc = [1, 2,
                           3]
                          function(abc
                          , abc)
                          function2()
                          45 + -rand(0, 1)
                          point.len()""")
        exprs = parse_tokens(tokens)
        self.assertEqual(5, len(exprs))
        expected_results = [
            "(= abc ([] None [1 2 3]))",
            "(() function [abc abc])",
            "(() function2 [])",
            "(+ 45 (- (() rand [0 1])))",
            "(() (. point len) [])"
        ]
        self.assertListEqual(list(map(str, exprs)), expected_results)

    def test_no_operands(self):
        from lexer import lex_file

        def cmp_exception(pet: ParserErrorType, col: int, content: str):
            tokens = lex_file("test_file", content)
            with self.assertRaises(ParserError) as context:
                parse_tokens(tokens)
            pe = context.exception
            self.assertEqual(col, pe.location.col)
            self.assertEqual(pet.value[0], pe.title)
            self.assertEqual(pet.value[1], pe.message)

        cmp_exception(ParserErrorType.EXPECTED_ATOM, 3, "a +")
        cmp_exception(ParserErrorType.UNOPENED_BRACKET, 1, "a)")
        cmp_exception(ParserErrorType.UNCLOSED_BRACKET, 1, "a(")
        cmp_exception(ParserErrorType.UNCLOSED_BRACKET, 1, "a(a*b")
        cmp_exception(ParserErrorType.EXPECTED_ATOM, 0, ")")
        cmp_exception(ParserErrorType.EXPECTED_ATOM, 1, "+")


if __name__ == '__main__':
    unittest.main()
