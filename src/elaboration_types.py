from dataclasses import dataclass, field
from typing import Optional, Dict, List

from ast_types import ASTNode, ASTNodeProcedure
from typing_types import Name, TypeTable, Record


@dataclass(slots=True)
class Scope:
    parent: Optional['Scope']
    names: Dict[str, Name]
    nodes: List[ASTNode]
    type_table: TypeTable
    delayed_elaborate: List[ASTNodeProcedure] = field(default_factory=list)

    def lookup(self, name: str) -> Optional[Name]:
        # todo maybe give indication if found in same scope?
        if (name_obj := self.names.get(name, None)) is None:
            if self.parent is None:
                return None
            else:
                return self.parent.lookup(name)
        else:
            return name_obj