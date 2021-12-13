

import numpy as np


def show_dots(dots):
    max_x = max(dot[1] for dot in dots)
    max_y = max(dot[0] for dot in dots)
    grid = np.full((max_x+1, max_y+1), fill_value=".")
    for y, x in dots:
        grid[x][y] = "#"

    print('\n'.join(''.join(x for x in y) for y in grid))


def fold(dots, direction, idx):

    if direction == "x":
        max_idx = max(dot[0] for dot in dots)
        to_flip = [dot for dot in dots if dot[0] >= idx]
        folded_dots = []
        for dot in to_flip:
            x, y = dot
            new_x = max_idx - x
            new_dot = (new_x, y)
            folded_dots.append(new_dot)
    else:
        max_idx = max(dot[1] for dot in dots)
        to_flip = [dot for dot in dots if dot[1] >= idx]
        folded_dots = []
        for dot in to_flip:
            x, y = dot
            new_y = max_idx - y
            new_dot = (x, new_y)
            folded_dots.append(new_dot)

    new_dots = list(set([dot for dot in dots if dot not in to_flip] + folded_dots))  # remove duplicate dots

    return new_dots


def get_instruction(instruction):
    direction, idx = instruction.replace("fold along ", "").split("=")
    return direction, int(idx)


def complete_folds(dots, instructions):

    for instruction in instructions:
        direction, idx = get_instruction(instruction)
        dots = fold(dots, direction, idx)

    return dots


def main():
    with open("data.txt") as f:
        lines = [l.replace("\n", "") for l in f.readlines()]
    instructions = [s for s in lines if s.startswith("fold along ")]
    dots = [(int(s.split(",")[0]), int(s.split(",")[1])) for s in lines if s not in instructions and s]
    dots = complete_folds(dots, instructions)
    show_dots(dots)


if __name__ == "__main__":
    main()
