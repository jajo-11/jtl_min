from dataclasses import dataclass, field
from typing import Optional, Dict, List, Self, Any

from ast_types import ASTNode, ASTNodeProcedure, ASTNodeRecord
from errors import ElaborationError, ElaborationErrorType
from lexer_types import CodeLocation
from typing_types import TypeTable, Mutability, TypeGroup, TypeRecordType


@dataclass(slots=True)
class UniqueIndexer:
    """
    Generate index for names used to identify Names and Records across Scope
    """
    counter: int = 0

    def next(self) -> int:
        self.counter += 1
        return self.counter


@dataclass(slots=True)
class Name:
    id: int
    name: str
    declaration_location: CodeLocation
    type: int
    type_table: "TypeTable"
    mut: Mutability = Mutability.ONCE

    def __str__(self):
        return (f"{self.mut.value} {self.name}: {self.type_table.get(self.type)}"
                f" @ {self.declaration_location}")

    def __hash__(self):
        return self.id

    def __eq__(self, other):
        if isinstance(other, Name):
            return self.id == other.id
        else:
            return False


class Scope:
    def __init__(self, parent: Self | TypeTable):
        if isinstance(parent, Scope):
            self.parent: Optional[Scope] = parent
            self.type_table: TypeTable = self.parent.type_table
            self.unique_indexer: UniqueIndexer = self.parent.unique_indexer # I do not remember why this is here lol
        else:
            self.parent = None
            self.type_table = parent
            self.unique_indexer = UniqueIndexer()
        self.names: Dict[str, Name] = {}
        self.body: List[ASTNode] = []
        self.procedures: Dict[Name, ASTNodeProcedure] = {}
        self.record_types: Dict[Name, Record] = {}

    def lookup(self, name: str) -> Optional[Name]:
        # todo maybe give indication if found in same scope?
        if (name_obj := self.names.get(name)) is None:
            if self.parent is None:
                return None
            else:
                return self.parent.lookup(name)
        else:
            return name_obj

    def lookup_record(self, name: Name):
        if (record_obj := self.record_types.get(name)) is None:
            if self.parent is None:
                return None
            else:
                return self.parent.lookup_record(name)
        else:
            return record_obj

@dataclass(slots=True)
class Record:
    id: int
    node: ASTNodeRecord
    name: str = "unnamed-record"
    # TODO: if a field has the constant mutability it shall be accessible from the Type and any instance
    fields: Dict[str, Name] = field(default_factory=dict)
    # methods: Dict[str, Name] = field(default_factory=dict)
    _size: int = -1
    _offsets: Dict[str, int] = field(default_factory=dict)
    _alignment: int = -1


    def get_size(self) -> int:
        if self._size == -1:
            self._size = -2
            offset = 0
            max_alignment = 0
            for fn, f in self.fields.items():
                ft = f.type_table.get(f.type)
                # these should not be possible
                assert ft.info.group not in {TypeGroup.UNDEFINED, TypeGroup.RETURNS, TypeGroup.NO_VALUE}
                # these might be necessary later but skip for now
                assert ft.info.group not in {TypeGroup.PROCEDURE, TypeGroup.TYPE}
                if ft.info.group == TypeGroup.RECORD:
                    assert isinstance(ft, TypeRecordType)
                    field_size = ft.record.get_size()
                    alignment = ft.record.get_alignment()
                else:
                    assert ft.info.size is not None
                    alignment = ft.info.size
                    field_size = ft.info.size
                max_alignment = max(max_alignment, alignment)
                alignment_remainder = offset % alignment
                if alignment_remainder != 0:
                    offset += alignment - alignment_remainder
                self._offsets[fn] = offset
                offset += field_size
            self._alignment = max_alignment
            self._size = offset
        elif self._size == -2:
            # TODO it would be really nice to show how it is cyclic (try catch and save path in exception?)
            raise ElaborationError.from_type(ElaborationErrorType.CYCLIC_RECORD, self.node.token.location)
        return self._size

    def get_offsets(self) -> Dict[str, int]:
        if self._size == -1:
            _ = self.get_size()
        return self._offsets

    def get_alignment(self) -> int:
        if self._size == -1:
            _ = self.get_size()
        return self._alignment

    def __str__(self) -> str:
        inner = ', '.join(map(lambda x: f'{x.mut.value} {x.name}: {x.type_table.get(x.type)}', self.fields.values()))
        return f"Record ({inner})"

    def __hash__(self) -> int:
        return self.id

    def __eq__(self, other: Any) -> bool:
        if isinstance(other, Record):
            return self.id == other.id
        else:
            return False