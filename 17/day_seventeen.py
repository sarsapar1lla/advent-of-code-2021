
import re


def simulate_step(x, y, vx, vy):
    x += vx
    y += vy
    if vx > 0: 
        vx -= 1
    elif vx < 0:
        vx += 1
    vy -= 1
    return (x, y, vx, vy)


def test_trajectory(initial_velocity, target_area):
    x, y = 0, 0
    vx, vy = initial_velocity
    x_min, x_max, y_min, y_max = target_area
    in_target_area = x_min <= x <= x_max and y_min <= y <= y_max
    if initial_velocity == (0, 0) and not in_target_area:
        return False, None  # will never reach the target area
    max_y = 0
    step_count = 0
    while not in_target_area and step_count < 500:
        new_x, new_y, vx, vy = simulate_step(x, y, vx, vy)
        if new_y > max_y:
            max_y = new_y
        in_target_area = x_min <= new_x <= x_max and y_min <= new_y <= y_max
        if in_target_area:
            return True, max_y
        
        if (x <= x_min and new_x >= x_max) or (x >= x_max and new_x <= x_min):
            return False, None  # trajectory doesn't end in the target area
        if (y <= y_min and new_y >= y_max) or (y >= y_max and new_y <= y_min):
            return False, None  # trajectory doesn't end in the target area
        x, y = new_x, new_y
        step_count += 1

    return False, None


def try_trajectories(target_area):
    velocities = [(i, j) for i in range(0, 1000) for j in range(-1000, 1000)]
    current_max = 0
    ends_in_area = 0
    for velocity in velocities:
        lands_in_target, max_y = test_trajectory(velocity, target_area)
        if lands_in_target:
            ends_in_area += 1
            if max_y > current_max:
                current_max = max_y
    return current_max, ends_in_area


def main():
    with open("data.txt") as f:
        goal = f.readline()
    print(goal)
    match = re.search(r"x=(-?[0-9]+)..(-?[0-9]+), y=(-?[0-9]+)..(-?[0-9]+)", goal)
    if not match:
        raise ValueError("Invalid goal.")
    x_min, x_max = int(match.group(1)), int(match.group(2))
    y_min, y_max = int(match.group(3)), int(match.group(4))
    target_area = (x_min, x_max, y_min, y_max)
    print(target_area)
    print(try_trajectories(target_area))


if __name__ == "__main__":
    main()
