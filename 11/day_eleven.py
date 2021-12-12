
import numpy as np


def get_adjacent_octopodes(x, y):
    return [
        (i, j) for i in range(x-1, x+2) 
        for j in range(y-1, y+2) 
    ]


def step(grid: np.ndarray, to_charge=None, flashed=None, total_flashes=0):

    max_x, max_y = grid.shape

    if to_charge is None:
        to_charge = [index for index, _ in np.ndenumerate(grid)]

    if flashed is None:
        flashed = []

    for x, y in to_charge:
        if (x, y) in flashed:
            continue
        new_value = grid[x][y] + 1
        grid[x][y] = new_value
        if new_value > 9:
            total_flashes += 1
            flashed.append((x, y))
            adjacent = get_adjacent_octopodes(x, y)
            extra_charges = [
                (i, j) for i, j in adjacent
                if 0 <= i <= (max_x - 1)
                and 0 <= j <= (max_y - 1)
                and (i, j) not in flashed  # can only flash once per step
            ]
            grid, flashed, total_flashes = step(grid=grid, to_charge=extra_charges, flashed=flashed, total_flashes=total_flashes)
            grid[x][y] = 0  # reset charge after flash

    return grid, flashed, total_flashes


def all_flashed(grid):
    if np.all(grid == 0):
        return True
    return False


def main():
    with open("data.txt") as f:
        grid = np.array([
            [c for c in line if c != "\n"] for line in f.readlines()
        ], dtype=int)
        total_flashes = 0
        all_flashed_step = None
        for i in range(0, 600):
            grid, _, total_flashes = step(grid=grid, total_flashes=total_flashes)
            if all_flashed(grid):
                all_flashed_step = i + 1
                break
        print(total_flashes)
        print(grid)
        print(all_flashed_step)
        


if __name__ == "__main__":
    main()
