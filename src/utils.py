from typing import Optional, Iterable


class PeakableIterator[T]:
    def __init__(self, it: Iterable[T], empty_value: T) -> None:
        self.it = iter(it)
        self.last: Optional[T] = None
        self.next_element: Optional[T] = None
        self.empty_value = empty_value
        self._empty: bool = False
        self._moved = True

    def __iter__(self):
        return self

    def next(self) -> T:
        return self.__next__()

    def has_moved(self) -> bool:
        """
        Returns `True` if the iterator has moved since last `has_moved` call, `False` otherwise.
        First call always returns `True`
        """
        if self._moved:
            self._moved = False
            return True
        else:
            return False

    def __next__(self) -> T:
        self._moved = True
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