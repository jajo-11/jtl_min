from dataclasses import dataclass, field
from typing import Optional, Dict, List, Self

from ast_types import ASTNode, ASTNodeProcedure
from typing_types import Name, TypeTable, Record


@dataclass(slots=True)
class NameIndex:
    """
    Generate index for names used to identify Names across Scope
    """
    counter: int = 0

    def next(self) -> int:
        self.counter += 1
        return self.counter


class Scope:
    def __init__(self, parent: Self | TypeTable):
        if isinstance(parent, Scope):
            self.parent: Optional[Scope] = parent
            self.type_table: TypeTable = self.parent.type_table
            self.name_index: NameIndex = self.parent.name_index
        else:
            self.parent = None
            self.type_table = parent
            self.name_index = NameIndex()
        self.names: Dict[str, Name] = {}
        self.nodes: List[ASTNode] = []
        self.delayed_elaborate: List[ASTNodeProcedure] = []

    def lookup(self, name: str) -> Optional[Name]:
        # todo maybe give indication if found in same scope?
        if (name_obj := self.names.get(name, None)) is None:
            if self.parent is None:
                return None
            else:
                return self.parent.lookup(name)
        else:
            return name_obj
