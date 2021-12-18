
from itertools import permutations
from math import ceil, floor
from dataclasses import dataclass


@dataclass
class SnailfishNumber:
    value: int
    left: 'SnailfishNumber'
    right: 'SnailfishNumber'
    position: int

    def is_regular(self):
        return self.value is not None

    def __add__(self, other):
        if not isinstance(other, self.__class__):
            return NotImplemented
        return SnailfishNumber(value=None, left=self, right=other, position=None)

    def get_max_nested_position(self):

        if self.is_regular():
            return self.position

        if self.right.is_regular():
            return self.right.position

        return self.right.get_max_nested_position()

    def get_number_from_position(self, position: int):

        if self.position == position:
            return self
        
        if self.left:
            search_left = self.left.get_number_from_position(position)
            if search_left:
                return search_left

        if self.right:
            search_right = self.right.get_number_from_position(position)
            if search_right:
                return search_right
        
        return None

    def __str__(self):

        if self.is_regular():
            return str(self.value)

        if self.left:
            return f"[{self.left}, {self.right}]"

    def calculate_magnitude(self):

        if self.value is not None:
            return self.value

        return 3 * self.left.calculate_magnitude() + 2 * self.right.calculate_magnitude()


def build_snailfish_number(n, position=0):

    if isinstance(n, int):
        return SnailfishNumber(value=n, left=None, right=None, position=position)

    left = n[0]
    right = n[1]

    if isinstance(left, int):
        left = SnailfishNumber(value=left, left=None, right=None, position=position + 1)
    else:
        left = build_snailfish_number(left, position=position + 1)

    max_left_position = left.get_max_nested_position()

    if isinstance(right, int):
        right = SnailfishNumber(value=right, left=None, right=None, position=max_left_position + 1)
    else:
        right = build_snailfish_number(right, position=max_left_position + 1)

    return SnailfishNumber(value=None, left=left, right=right, position=position)


def find_exploding_number(n: SnailfishNumber, depth=0):

    if n is None:
        return None

    if depth == 4 and not n.is_regular():
        if n.left.is_regular() and n.right.is_regular():
            return n

    to_explode = find_exploding_number(n.left, depth=depth + 1)
    if to_explode:
        return to_explode

    to_explode = find_exploding_number(n.right, depth=depth + 1)
    if to_explode:
        return to_explode

    return None


def find_splitting_number(n: SnailfishNumber):

    if n is None:
        return None

    if n.is_regular() and n.value >= 10:
        return n

    to_split = find_splitting_number(n.left)
    if to_split:
        return to_split

    to_split = find_splitting_number(n.right)
    if to_split:
        return to_split

    return None


def split(splitting_number: SnailfishNumber):

    if not splitting_number:
        return

    left = floor(splitting_number.value / 2)
    right = ceil(splitting_number.value / 2)

    splitting_number.value = None
    splitting_number.left = SnailfishNumber(value=left, left=None, right=None, position=None)
    splitting_number.right = SnailfishNumber(value=right, left=None, right=None, position=None)


def find_first_value_to_left(n: SnailfishNumber, exploding_position: int):

    position = exploding_position - 1
    while position >= 0:
        number = n.get_number_from_position(position)
        if number and number.is_regular():
            return number
        position -= 1
    return None


def find_first_value_to_right(n: SnailfishNumber, exploding_position: int):

    position = exploding_position + 1
    while position <= n.get_max_nested_position():
        number = n.get_number_from_position(position)
        if number and number.is_regular():
            return number
        position += 1
    return None


def explode(n: SnailfishNumber, exploding_number: SnailfishNumber):

    if not exploding_number:
        return
    closest_left_number = find_first_value_to_left(n, exploding_number.left.position)
    closest_right_number = find_first_value_to_right(n, exploding_number.right.position)

    if closest_left_number:
        closest_left_number.value += exploding_number.left.value
    if closest_right_number:
        closest_right_number.value += exploding_number.right.value

    exploding_number.value = 0
    exploding_number.left = None
    exploding_number.right = None


def refresh_number(n: SnailfishNumber):
    return build_snailfish_number(eval(str(n)))


def reduce_number(n: SnailfishNumber):

    exploding_number = find_exploding_number(n)
    if exploding_number:
        explode(n, exploding_number)
        n = refresh_number(n)
        n = reduce_number(n)

    splitting_number = find_splitting_number(n)
    if splitting_number:
        split(splitting_number)
        n = refresh_number(n)
        n = reduce_number(n)

    return n


def reduce_numbers(numbers):
    n = numbers[0]
    sfn = build_snailfish_number(eval(n))
    for number in numbers[1:]:
        sfn2 = build_snailfish_number(eval(number))
        sfn = refresh_number(sfn + sfn2)
        sfn = reduce_number(sfn)
    return sfn


def find_largest_sum_of_two_numbers(numbers):

    number_combinations = permutations(numbers, 2)
    largest_sum = 0
    for n1, n2 in number_combinations:
        sfn1 = build_snailfish_number(eval(n1))
        sfn2 = build_snailfish_number(eval(n2))
        sfn = refresh_number(sfn1 + sfn2)
        sfn = reduce_number(sfn)
        magnitude = sfn.calculate_magnitude()
        if magnitude > largest_sum:
            largest_sum = magnitude
    return largest_sum


def main():
    with open("data.txt") as f:
        numbers = [l.replace("\n", "") for l in f.readlines()]

    # part 1
    sfn = reduce_numbers(numbers)
    print(sfn)
    print(sfn.calculate_magnitude())

    # part 2
    print(find_largest_sum_of_two_numbers(numbers))




if __name__ == "__main__":
    main()
