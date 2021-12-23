
import numpy as np
from numpy.lib.index_tricks import index_exp


def create_burrow(lines):
    longest = max(len(l) for l in lines)
    normalised_lines = []
    for line in lines:
        if len(line) == longest:
            normalised_lines.append(line)
            continue
        missing = longest - len(line)
        for _ in range(missing):
            line += " "
        normalised_lines.append(line)
    return np.array([list(l) for l in normalised_lines])


def find_amphopods(burrow):
    amphopods = []
    for idx, elem in np.ndenumerate(burrow):
        if elem not in "ABCD":
            continue
        amphopods.append(idx)
    return amphopods


def find_rooms(burrow):
    amphopods = "ABCD"
    rooms = []
    for idx, elem in np.ndenumerate(burrow):
        if elem not in amphopods:
            continue
        x, y = idx
        if burrow[x+1][y] not in amphopods:
            continue
        room = [(x, y), (x+1, y)]
        rooms.append(room)
    return rooms


def find_target_room(amphopod, rooms):
    amphopods = "ABCD"
    room_index = amphopods.index(amphopod)
    return rooms[room_index]


def find_possible_moves(burrow, position, searched=None):
    if not searched:
        searched = set()
    searched.add(position)
    x, y = position
    adjacent = {(x-1, y), (x+1, y), (x, y-1), (x, y+1)}
    possible_moves = set()
    for i, j in {a for a in adjacent if a not in searched}:
        try:
            value = burrow[i][j]
        except IndexError:
            continue
        if value == ".":
            possible_moves.add((i, j))
            possible_moves.update(find_possible_moves(burrow, (i, j), searched))
    return possible_moves


def is_in_room(position, rooms):
    if any(position in room for room in rooms):
        return True
    return False


def is_in_hallway(position):
    if position[0] == 1 and 1 <= position[1] <= 11:
        return True
    return False


def is_outside_room(position, rooms):
    if not is_in_hallway(position):
        return False
    for room in rooms:
        entrance = room[0]
        if position[1] == entrance[1]:
            return True
    return False


def find_energy_costs(burrow):
    rooms = find_rooms(burrow)
    positions = find_amphopods(burrow)
    for position in positions:
        x, y = position
        amphopod = burrow[x][y]
        target_room = find_target_room(amphopod, rooms)
        possible_moves = find_possible_moves(burrow, position)
        for move in possible_moves:
            if is_outside_room(move, rooms):
                continue  # can't move to position directly outside room
            if is_in_hallway(position) and move not in target_room:
                continue  # can't move from hallway into a room which isn't the target room
            


def main():
    with open("test.txt") as f:
        lines = [l.replace("\n", "") for l in f.readlines()]
    burrow = create_burrow(lines)
    print(burrow)
    rooms = find_rooms(burrow)
    print(find_possible_moves(burrow, (3, 9)))



if __name__ == "__main__":
    main()
