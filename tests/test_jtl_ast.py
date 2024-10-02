import unittest

from errors import ParserError, ParserErrorType
from jtl_ast import parse_tokens
from lexer import lex_file


class TestParser(unittest.TestCase):
    def test_basic_expession(self):

        tokens = lex_file("test_file", "sum: int = 3 * (1 +\n 2) * -2")
        exprs = parse_tokens(tokens)
        self.assertEqual(1, len(exprs))
        self.assertIsNotNone(exprs[0])
        self.assertEqual(str(exprs[0]), "(= (: sum int) (* (* 3 (+ 1 2)) (<- - 2)))")

    def test_pointers(self):
        tokens = lex_file("test_file", "let sum_ptr: ^int = &1\n"
                                       "let sum: int = sum_ptr^")
        exprs = parse_tokens(tokens)
        self.assertEqual(2, len(exprs))
        self.assertEqual(str(exprs[0]), "(let (= (: sum_ptr (<- ^ int)) (<- & 1)))")
        self.assertEqual(str(exprs[1]), "(let (= (: sum int) (-> ^ sum_ptr)))")

    def test_tuple_like(self):

        tokens = lex_file("test_file",
                          """abc = [1, 2,
                           3]
                          function(abc
                          , abc,)
                          function2()
                          45 + -rand(0, 1)
                          point.len()""")
        exprs = parse_tokens(tokens)
        self.assertEqual(5, len(exprs))
        expected_results = [
            "(= abc ([] None [1 2 3]))",
            "(() function [abc abc])",
            "(() function2 [])",
            "(+ 45 (<- - (() rand [0 1])))",
            "(() (. point len) [])"
        ]
        self.assertListEqual(list(map(str, exprs)), expected_results)

    def test_no_operands(self):

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

    def test_if_and_scope(self):

        tokens = lex_file("test_file", """
            if sum <= 1 then {
                print(sum)
                sum = (sum + 1) * sum
                {
                    a = 1 * 2
                }
            }

            no = if sum > 0 and 1 == 1 then sum else -sum""")

        expected_result = ["(if (<= sum 1)\n"
                           " body:\n"
                           "   #(() print [sum])\n"
                           "   #(= sum (* (+ sum 1) sum))\n"
                           "   ##(= a (* 1 2))\n"
                           " else:\n"
                           "   None\n"
                           ")",
                           "(= no (if (and (> sum 0) (== 1 1))\n"
                           " body:\n"
                           "   sum\n"
                           " else:\n"
                           "   (<- - sum)\n"
                           "))"]
        result = list(map(str, (parse_tokens(tokens))))
        self.assertEqual(expected_result, result)

    def test_while(self):
        tokens = lex_file("test_file",
                          "while i < 10 {i = i + 1}")
        expected_result = "\n".join([
            "(while (< i 10)",
            " body:",
            "   #(= i (+ i 1))",
            ")",
        ])

        result = str(parse_tokens(tokens)[0])
        self.assertEqual(expected_result, result)

    def test_procedure(self):
        tokens = lex_file("test_file",
                          "const fn = proc() {\n"
                          "print(\"hi\")\n"
                          "}\n")
        expected_result = "\n".join([
            "(const (= fn (proc (() None []) None",
            " body:",
            "   #(() print [\"hi\"])",
            ")))"
        ])

        result = str(parse_tokens(tokens)[0])
        self.assertEqual(expected_result, result)

        tokens = lex_file("test_file",
                          "const fn = proc(a: int): ^int {\n"
                          "    return malloc(a * size_of(int))\n"
                          "}\n")
        expected_result = "\n".join([
            "(const (= fn (proc (() None [(: a int)]) (<- ^ int)",
            " body:",
            "   #(return (() malloc [(* a (() size_of [int]))]))",
            ")))"
        ])

        result = str(parse_tokens(tokens)[0])
        self.assertEqual(expected_result, result)

    def test_record(self):
        tokens = lex_file("test_file",
                          "const vec3 = record(x, y, z: float)")
        expected_result = \
            "(const (= vec3 (() TokenKeyword(location=test_file:1:14, keyword=record) [x y (: z float)])))"

        result = str(parse_tokens(tokens)[0])
        self.assertEqual(expected_result, result)


if __name__ == '__main__':
    unittest.main()
