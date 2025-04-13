import unittest

import elaborate
from errors import ElaborationError, ElaborationErrorType
from jtl_ast import parse_tokens
from lexer import lex_file


def test_for_exception(test: unittest.TestCase, src: str, exception: ElaborationErrorType):
    tokens = lex_file("test_file", src)
    exprs = parse_tokens(tokens)
    with test.assertRaises(ElaborationError) as context:
        _ = elaborate.elaborate_module(exprs)

    e: ElaborationError = context.exception
    title, message = exception.value
    test.assertEqual(e.title, title)
    test.assertEqual(e.message, message)


class TestElaborationErrors(unittest.TestCase):

    def test_const_field(self):
        src = """
        const rec = record {
            let z: int
        }

        const main = proc() {
               var c = rec(1)
               c.z = 2     
        }
        """
        test_for_exception(self, src, ElaborationErrorType.UN_ASSIGNABLE)

    def test_const_record(self):
        src = """
        const rec = record {var z: int}
        const main = proc() {
            const c = rec(1)
            c.z = 2
        }
        """
        test_for_exception(self, src, ElaborationErrorType.UN_ASSIGNABLE)

    def test_const_record_mut_ptr(self):
        src = """
        const rec = record {var z: int}
        const main = proc() {
            const c = rec(1)
            var p = @var c.z
        }
        """
        test_for_exception(self, src, ElaborationErrorType.MUTABLE_ADDRESS_OF_CONST)

    def test_const_record_field_mut_ptr(self):
        src = """
        const rec = record {let z: int}
        const main = proc() {
            var c = rec(1)
            var p = @var c.z
        }
        """
        test_for_exception(self, src, ElaborationErrorType.MUTABLE_ADDRESS_OF_CONST)

    def test_assign_nonsense(self):
        # TODO: This error message is poor
        src = """
        const rec = record {var z: int}
        const main = proc() {
            var c = rec(1)
            rec = @var c.z
        }
        """
        test_for_exception(self, src, ElaborationErrorType.UN_ASSIGNABLE)


if __name__ == "__main__":
    unittest.main()
