

def get_possible_roll_combinations():
    totals = []
    for i in {1, 2, 3}:
        for j in {1, 2, 3}:
            for k in {1, 2, 3}:
                total = i + j + k
                totals.append(total)
    return {t: totals.count(t) for t in set(totals)}


def play(initial_state):
    winning_score = 21
    roll_combinations = get_possible_roll_combinations()
    game_states = {initial_state: 1}
    winning_states = {k: v for k, v in game_states.items() if k[2] >= winning_score or k[3] >= winning_score}
    incomplete_states = {k: v for k, v in game_states.items() if k not in winning_states}
    while incomplete_states:
        for state, occurances in incomplete_states.items():  # player one move
            p1, p2, s1, s2, p1_to_move = state
            if not p1_to_move:
                continue
            for roll, count in roll_combinations.items():
                new_p1 = (p1 + roll) % 10
                if new_p1 == 0: new_p1 = 10
                new_s1 = s1 + new_p1
                new_game_state = (new_p1, p2, new_s1, s2, False)
                if new_game_state in game_states:
                    game_states[new_game_state] += occurances * count
                else:
                    game_states[new_game_state] = occurances * count
            del game_states[state]
        winning_states = {k: v for k, v in game_states.items() if k[2] >= winning_score or k[3] >= winning_score}
        incomplete_states = {k: v for k, v in game_states.items() if k not in winning_states}
        for state, occurances in incomplete_states.items():  # player two move
            p1, p2, s1, s2, p1_to_move = state
            if p1_to_move:
                continue
            for roll, count in roll_combinations.items():
                new_p2 = (p2 + roll) % 10
                if new_p2 == 0: new_p2 = 10
                new_s2 = s2 + new_p2
                new_game_state = (p1, new_p2, s1, new_s2, True)
                if new_game_state in game_states:
                    game_states[new_game_state] += occurances * count
                else:
                    game_states[new_game_state] = occurances * count
            del game_states[state]
        winning_states = {k: v for k, v in game_states.items() if k[2] >= winning_score or k[3] >= winning_score}
        incomplete_states = {k: v for k, v in game_states.items() if k not in winning_states}

    p1_wins = sum(v for k, v in winning_states.items() if k[2] >= winning_score)
    p2_wins = sum(v for k, v in winning_states.items() if k[3] >= winning_score)
    print(p1_wins)
    print(p2_wins)

    if p1_wins > p2_wins:
        return p1_wins
    return p2_wins


def main():
    initial_state = (6, 4, 0, 0, True)
    print(play(initial_state))


if __name__ == "__main__":
    main()
