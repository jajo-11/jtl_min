from typing import Optional, Iterable


class PeakableIterator[T]:
    def __init__(self, it: Iterable[T], empty_value: T) -> None:
        self.it = iter(it)
        self.last: Optional[T] = None
        self.next_element: Optional[T] = None
        self._empty: bool = False
        self.empty_value = empty_value

    def __iter__(self):
        return self

    def next(self) -> T:
        return self.__next__()

    def __next__(self) -> T:
        if self.next_element is None:
            try:
                self.last = next(self.it)
                return self.last
            except StopIteration:
                self._empty = True
                raise StopIteration
        else:
            self.last = self.next_element
            self.next_element = None
            return self.last

    def peak(self) -> T:
        try:
            if self.next_element is None:
                self.next_element = next(self.it)
            return self.next_element
        except StopIteration:
            self._empty = True
            return self.empty_value

    def is_empty(self) -> bool:
        self.peak()
        return self._empty