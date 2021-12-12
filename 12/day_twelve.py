

def find_paths(start, connections, visited=None, small_cave_visited_twice=False):
    if visited is None:
        visited = ["start"]
    possible_connections = [e for s, e in connections if s == start] + [s for s, e in connections if e == start]
    paths = []
    if not possible_connections:
        raise ValueError(f"No possible connections from cave {start}.")
    for e in possible_connections:
        path = []
        if start == "start":
            path.append(start)
        visited_on_path = [] + visited
        small_cave_visited_twice_on_path = small_cave_visited_twice
        if e == "start":
            continue  # don't return to the start cave
        if e == e.lower() and e in visited_on_path and small_cave_visited_twice_on_path:
            continue  # don't visit small caves more than once
        if e == e.lower() and e in visited_on_path:
            small_cave_visited_twice_on_path = True
        path.append(e)
        if e == "end":
            paths.append(path)
            continue  # path is complete
        visited_on_path.append(e)
        try:
            resulting_paths = find_paths(
                start=e, 
                connections=connections, 
                visited=visited_on_path, 
                small_cave_visited_twice=small_cave_visited_twice_on_path
            )
            resulting_paths = [path + p for p in resulting_paths]
        except ValueError:
            continue  # skip connections which lead to a dead end
        paths.extend(resulting_paths)

    return paths


def main():
    with open("data.txt") as f:
        connections = [c.replace("\n", "").split("-") for c in f.readlines()]

    print(len(find_paths("start", connections)))


if __name__ == "__main__":
    main()
