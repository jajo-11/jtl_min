import unittest

import elaborate
from ir import IRUnit
from jtl_ast import parse_tokens
from lexer import lex_file

def eval_and_get_saved_ir(file_name: str) -> (str, str):
    with open(file_name) as f:
        src = f.read()

    src_lines = src.splitlines()
    start_index = 0
    stop_index = 0
    for i, line in enumerate(src_lines):
        if line == ">>>":
            start_index = i
        elif line == "<<<":
            stop_index = i
    target_ir = "\n".join(src_lines[start_index + 1:stop_index])

    tokens = lex_file("test_file", src)
    exprs = parse_tokens(tokens)
    global_scope = elaborate.elaborate_module(exprs)
    unit = IRUnit(global_scope)
    # my editor removes trailing whitespaces on save so the ir saved in test is missing some spaces
    ir_striped = "\n".join(map(lambda x: x.rstrip(), str(unit).splitlines()))
    return target_ir, ir_striped


class TestIr(unittest.TestCase):
    def test_binary_ops(self):
        target_ir, ir_striped = eval_and_get_saved_ir("../test_files/binary_ops.jtl")
        self.assertEqual(target_ir, ir_striped)

    def test_expression(self):
        target_ir, ir_striped = eval_and_get_saved_ir("../test_files/expression.jtl")
        self.assertEqual(target_ir, ir_striped)

    def test_if(self):
        target_ir, ir_striped = eval_and_get_saved_ir("../test_files/if.jtl")
        self.assertEqual(target_ir, ir_striped)

    def test_loop(self):
        target_ir, ir_striped = eval_and_get_saved_ir("../test_files/loop.jtl")
        self.assertEqual(target_ir, ir_striped)

    def test_procedure(self):
        target_ir, ir_striped = eval_and_get_saved_ir("../test_files/procedure.jtl")
        self.assertEqual(target_ir, ir_striped)

    def test_pointers(self):
        target_ir, ir_striped = eval_and_get_saved_ir("../test_files/pointers.jtl")
        self.assertEqual(target_ir, ir_striped)