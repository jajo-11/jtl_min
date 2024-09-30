import sys
from pprint import pprint

from jtl_ast import parse_tokens
from errors import simple_error
from lexer import lex_file

if __name__ == '__main__':
    if len(sys.argv) != 2:
        simple_error('Need file to compile')
        sys.exit(1)

    with open(sys.argv[1], 'r', encoding="utf-8", errors="strict") as f:
        file_contents = f.read()

    tokens = lex_file(sys.argv[1], file_contents)

    pprint(tokens)

    ast_nodes = parse_tokens(tokens)

    for node in ast_nodes:
        print(node)
