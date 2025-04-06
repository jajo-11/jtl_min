import sys
import os

from backends.qbe import write_qbe
from ir import IRContext
from jtl_ast import parse_tokens
from errors import simple_error
from lexer import lex_file
from elaborate import elaborate_module

if __name__ == '__main__':
    if len(sys.argv) != 2:
        simple_error('Need file to compile')
        sys.exit(1)

    with open(sys.argv[1], 'r', encoding="utf-8", errors="strict") as f:
        file_contents = f.read()

    print("Tokenizing...")
    tokens = lex_file(sys.argv[1], file_contents)
    # pprint(tokens)

    print("Parsing tokens...")
    ast_nodes = parse_tokens(tokens)
    for node in ast_nodes:
        print(node)

    print("Elaborating...")
    global_scope = elaborate_module(ast_nodes)

    print("Generating IR...")
    ctx = IRContext(global_scope)
    ir = ctx.lower_unit()
    ir.write(sys.stdout)

    print("Generating QBE IR...")
    with open("../out/out.qbe", "w", newline="\n") as f:
        write_qbe(ir, f)

    if sys.platform == "linux":
        os.system("qbe -o ../out/out.s ../out/out.qbe && cc ../out/out.s -o ../out/a.out")
    else:
        print("Skipping compiling on non Linux platform")

    print("Done")