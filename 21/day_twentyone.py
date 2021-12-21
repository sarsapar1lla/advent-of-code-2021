
from dataclasses import dataclass


@dataclass
class Die:
    value: int = 0
    faces: int = 100
    rolls: int = 0

    def roll(self):
        self.rolls += 1
        self.value = 1 if self.value == 100 else self.value + 1
        return self.value


@dataclass
class Player:
    position: int
    score: int = 0

    @property
    def has_won(self):
        return self.score >= 1000

    def move(self, die: Die):
        total_move = 0
        for _ in range(3):
            total_move += die.roll()

        new_position = (self.position + total_move) % 10
        if new_position == 0:
            self.position = 10
        else:
            self.position = new_position
        self.score += self.position


def play(p1: Player, p2: Player, die: Die):

    while not p1.has_won and not p2.has_won:
        p1.move(die)
        if p1.has_won:
            break
        p2.move(die)

    if p1.has_won:
        return p2.score * die.rolls
    else:
        return p1.score * die.rolls


def main():
    p1 = Player(6)
    p2 = Player(4)
    die = Die()
    # p1.move(die)
    # p2.move(die)
    total = play(p1, p2, die)
    print(total)


if __name__ == "__main__":
    main()
