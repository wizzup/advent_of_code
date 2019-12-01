"""
https://adventofcode.com/2019/day/1
"""

from typing import List


def get_lines():
    xs = []

    def loop():
        try:
            i = int(input())
            xs.append(i)
            loop()
        except EOFError:
            return

    loop()
    return xs


def fuel(i: int) -> int:
    """
    >>> [fuel(i) for i in [12,14,1969,100756]]
    [2, 2, 654, 33583]
    """
    return i // 3 - 2


def part_1(xs: List[int]) -> int:
    return sum([fuel(x) for x in xs])


def part_2(xs: List[int]) -> int:
    ss = []
    for x in xs:
        s = fuel(x)
        while s > 0:
            ss.append(s)
            s = fuel(s)

    return sum(ss)


def main():
    xs = get_lines()
    print(part_1(xs))
    print(part_2(xs))


if __name__ == '__main__':
    main()
