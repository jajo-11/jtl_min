import io
import unittest
from io import StringIO

import elaborate
from ir import IRContext
from jtl_ast import parse_tokens
from lexer import lex_file

def eval_and_get_saved_ir(file_name: str) -> (str, str):
    with open(file_name) as f:
        src = f.read()

    src_lines = src.splitlines()
    start_index = 0
    stop_index = 0
    for i, line in enumerate(src_lines):
        if line == ">>>ir":
            start_index = i
        elif line == "ir<<<":
            stop_index = i
    target_ir = "\n".join(src_lines[start_index + 1:stop_index])

    tokens = lex_file(file_name, src)
    exprs = parse_tokens(tokens)
    global_scope = elaborate.elaborate_module(exprs)
    ctx = IRContext(global_scope)
    unit = ctx.lower_unit()

    ir_buffer = io.StringIO()
    unit.write(ir_buffer)
    ir_buffer.seek(0, io.SEEK_SET)
    # my editor removes trailing whitespaces on save so the ir saved in test is missing some spaces
    ir_striped = "\n".join(map(lambda x: x.rstrip(), ir_buffer.readlines()))
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

    def test_records(self):
        target_ir, ir_striped = eval_and_get_saved_ir("../test_files/records.jtl")
        self.assertEqual(target_ir, ir_striped)

    def test_external_function(self):
        target_ir, ir_striped = eval_and_get_saved_ir("../test_files/external_function.jtl")
        self.assertEqual(target_ir, ir_striped)