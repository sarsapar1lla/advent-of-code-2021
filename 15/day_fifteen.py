
import numpy as np
from numpy.core.numeric import full


def get_possible_moves(point):
    x, y = point
    up = (x + 1, y)
    down = (x - 1, y)
    left = (x, y - 1)
    right = (x, y + 1)
    return [up, down, left, right]


def build_path(current_node, previous_nodes):
    path = [current_node]
    while current_node in previous_nodes:
        current_node = previous_nodes[current_node]
        path.append(current_node)
    return path


def find_path(start, end, cavern: np.ndarray):
    to_explore = {i for i, _ in np.ndenumerate(cavern)}
    previous_nodes = {}
    risk = {start: 0}

    while to_explore:
        candidate_nodes = {k: risk[k] for k in (risk.keys() & to_explore)}
        current_node = min(candidate_nodes, key=candidate_nodes.get)
        if current_node == end:
            return build_path(current_node, previous_nodes)

        to_explore.remove(current_node)
        possible_nodes = [
            (i, j) for i, j in get_possible_moves(current_node)
            if (i, j) in to_explore
        ]
        for neighbour_node in possible_nodes:
            x, y = neighbour_node
            neighbour_risk = risk[current_node] + cavern[x][y]
            current_neighbour_risk = risk.get(neighbour_node, np.Infinity)
            if neighbour_risk < current_neighbour_risk:
                previous_nodes[neighbour_node] = current_node
                risk[neighbour_node] = neighbour_risk


def get_start_and_end(cavern):
    start = (0, 0)
    x, y = cavern.shape
    return start, (x-1, y-1)


def score_path(path, cavern):
    score = 0
    for point in reversed(path):
        if point == (0, 0):
            continue
        x, y = point
        score += cavern[x][y]
    return score


def show_path(path):
    max_x = max(point[0] for point in path)
    max_y = max(point[1] for point in path)
    grid = np.full((max_x+1, max_y+1), fill_value=".")
    for x, y in path:
        grid[x][y] = "#"

    print('\n'.join(''.join(x for x in y) for y in grid))


def get_full_cavern(cavern: np.ndarray):

    def increase_risk(idx, current):
        i, j = idx
        tiles_traversed = np.floor(i / width) + np.floor(j / height)
        new_risk = int(current + tiles_traversed)
        if new_risk > 9:
            new_risk -= 9
        return new_risk

    width, height = cavern.shape
    full_cavern = np.tile(cavern, (5, 5))
    new_risks = [increase_risk(idx, risk) for idx, risk in np.ndenumerate(full_cavern)]
    new_width = width * 5
    new_cavern = np.array([new_risks[x:x+new_width] for x in range(0, len(new_risks), new_width)])
    return new_cavern


def main():
    with open("data.txt") as f:
        lines = [l.replace("\n", "") for l in f.readlines()]
    cavern = np.array([[int(i) for i in line] for line in lines])
    cavern = get_full_cavern(cavern)
    start, end = get_start_and_end(cavern)
    path = find_path(start, end, cavern)
    # show_path(path)
    print(score_path(path, cavern))


if __name__ == "__main__":
    main()
