
import numpy as np


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


def find_path(start, end, cavern):

    to_explore = [(i, j) for i in range(len(cavern)) for j in range(len(cavern[0]))]
    previous_nodes = {}
    risk = {start: 0}

    while to_explore:
        unexplored_risks = {k: v for k, v in risk.items() if k in to_explore}
        current_node = min(unexplored_risks, key=unexplored_risks.get)
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
            current_neighbour_risk = risk.get(neighbour_node, None)
            if current_neighbour_risk is None or neighbour_risk < current_neighbour_risk:
                previous_nodes[neighbour_node] = current_node
                risk[neighbour_node] = neighbour_risk


def get_start_and_end(cavern):
    start = (0, 0)
    end = (len(cavern) - 1, len(cavern[0]) - 1)
    return start, end


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


def main():
    with open("data.txt") as f:
        lines = [l.replace("\n", "") for l in f.readlines()]
    cavern = [[int(i) for i in line] for line in lines]
    start, end = get_start_and_end(cavern)
    path = find_path(start, end, cavern)
    # print(path)
    # show_path(path)
    print(score_path(path, cavern))


if __name__ == "__main__":
    main()
