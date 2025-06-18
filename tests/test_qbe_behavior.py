import io
import sys
import unittest
import subprocess

import elaborate
from backends.qbe import write_qbe
from ir import IRContext
from jtl_ast import parse_tokens
from lexer import lex_file


def compile_and_run(test: unittest.TestCase, file_name: str) -> (str, str):
    with open(file_name) as f:
        src = f.read()

    src_lines = src.splitlines()
    start_index_stdout = 0
    stop_index_stdout = 0
    start_index_stderr = 0
    stop_index_stderr = 0
    target_return_value = None
    for i, line in enumerate(src_lines):
        if line == ">>>stdout":
            start_index_stdout = i
        elif line == "stdout<<<":
            stop_index_stdout = i
        if line == ">>>stderr":
            start_index_stderr = i
        elif line == "stderr<<<":
            stop_index_stderr = i
        if line.startswith("[Return Value]"):
            target_return_value = int(line.split("]")[1])
    target_std_output = "\n".join(src_lines[start_index_stdout + 1:stop_index_stdout])
    target_err_output = "\n".join(src_lines[start_index_stderr + 1:stop_index_stderr])

    tokens = lex_file(file_name, src)
    exprs = parse_tokens(tokens)
    global_scope = elaborate.elaborate_module(exprs)
    ctx = IRContext(global_scope)
    unit = ctx.lower_unit()

    if sys.platform != "linux":
        test.skipTest("Can only be run on Linux platform")

    with open("/tmp/test.ssa", "w", newline="\n") as f:
        write_qbe(unit, f)

    cp = subprocess.run(["qbe", "/tmp/test.ssa", "-o", "/tmp/test.s"], capture_output=True)
    test.assertEqual(cp.returncode, 0, f"qbe failed with error {cp.stderr}")
    test.assertEqual(cp.stdout.decode(), "")
    test.assertEqual(cp.stderr.decode(), "")

    cp = subprocess.run(["cc", "/tmp/test.s", "-o", "/tmp/test.exe"], capture_output=True)
    test.assertEqual(cp.returncode, 0, f"cc failed with error {cp.stderr}")
    test.assertEqual(cp.stdout.decode(), "")
    test.assertEqual(cp.stderr.decode(), "")

    cp = subprocess.run(["/tmp/test.exe"], capture_output=True)
    test.assertEqual(target_return_value, cp.returncode)
    test.assertEqual(target_std_output, cp.stdout.decode())
    test.assertEqual(target_err_output, cp.stderr.decode())

class TestBehavior(unittest.TestCase):
    def test_binary_ops(self):
        compile_and_run(self, "../test_files/binary_ops.jtl")

    def test_external_function(self):
        compile_and_run(self, "../test_files/external_function.jtl")

    def test_if(self):
        compile_and_run(self, "../test_files/if.jtl")

    def test_pointers(self):
        compile_and_run(self, "../test_files/pointers.jtl")

    def test_procedure(self):
        compile_and_run(self, "../test_files/procedure.jtl")

    def test_records(self):
        compile_and_run(self, "../test_files/records.jtl")

    def test_arrays(self):
        compile_and_run(self, "../test_files/arrays.jtl")

    def test_pass_array_by_value(self):
        compile_and_run(self, "../test_files/pass_array_by_value.jtl")

    def test_pass_records_by_value(self):
        compile_and_run(self, "../test_files/pass_records_by_value.jtl")

    def test_pass_ragged_record_array(self):
        compile_and_run(self, "../test_files/ragged_record_array.jtl")

    def test_halfs_and_bytes(self):
        compile_and_run(self, "../test_files/halfs_and_bytes.jtl")

if __name__ == "__main__":
    unittest.main()