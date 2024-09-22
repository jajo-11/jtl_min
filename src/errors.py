from lexer_types import CodeLocation


def simple_error(msg: str):
    print(f"Error: {msg}")

def lexer_error(title: str, msg: str, code_location: CodeLocation):
    print(f"[Lexer Error] {title} [{code_location}]:\n{msg}\n")

def parser_error(title: str, msg: str, code_location: CodeLocation):
    print(f"[Parser Error] {title} [{code_location}]:\n{msg}\n")