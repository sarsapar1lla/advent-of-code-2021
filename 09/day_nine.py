

def create_array(data):
    array = []
    for line in data:
        line = line.replace("\n", "")
        elements = [int(i) for i in list(line)]
        array.append(elements)
    return array


def find_adjacent_points(point, array):
    x, y = point
    up = (x-1, y) if x > 0 else None
    down = (x+1, y) if x < len(array) - 1 else None
    left = (x, y-1) if y > 0 else None
    right = (x, y+1) if y < len(array[0]) -1 else None
    return [p for p in [up, down, left, right] if p is not None]


def is_low_point(point, array):

    x, y = point
    value = array[x][y]
    adjacent_points = find_adjacent_points(point, array)
    adjacent_values = [array[i][j] for i, j in adjacent_points]

    return all(value < i for i in adjacent_values)


def find_pool_from_point(point, array, checked_points = None):

    pool = [point]
    x, y = point

    if checked_points is None:
        checked_points = [point]
    else:
        checked_points.append(point)

    if array[x][y] == 9:  # 9s are not part of any pool
        return []
    
    adjacent_points = [p for p in find_adjacent_points(point, array) if p not in checked_points]
    for adjacent_point in adjacent_points:
        pool += find_pool_from_point(adjacent_point, array, checked_points)
    
    return list(set(pool))  # remove any duplicate points


def find_pools(array):

    pools = []
    for i, _ in enumerate(array):
        for j, _ in enumerate(array[0]):
            point = (i, j)
            if any(point in pool for pool in pools):
                continue  # point already pooled
            pool = find_pool_from_point(point, array)
            if pool:
                pools.append(pool)

    return pools



def multiply_largest_pools(pools, array):

    pool_sizes = sorted([len(pool) for pool in pools], reverse=True)  # get largest pools first
    top_three_pools = pool_sizes[:3]
    return top_three_pools[0] * top_three_pools[1] * top_three_pools[2]


def find_low_points(array):
    low_points = []
    for i, _ in enumerate(array):
        for j, _ in enumerate(array[0]):
            if is_low_point((i, j), array):
                low_points.append((i, j))
    return low_points


def calculate_risk_level(point, array):
    x, y = point
    value = array[x][y]
    return value + 1


def main():
    with open("data.txt") as f:
        data = f.readlines()
        array = create_array(data)
        low_points = find_low_points(array)
        total_risk = 0
        for low_point in low_points:
            total_risk += calculate_risk_level(low_point, array)
        print(total_risk)
        pools = find_pools(array)
        print(multiply_largest_pools(pools, array))


if __name__ == "__main__":
    main()
