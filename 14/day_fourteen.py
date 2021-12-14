
from copy import copy


def get_counts_from_template(template):
    return {c: template.count(c) for c in template}


def get_pairs_from_template(template):
    pairs_list = [i + j for i, j in zip(template, template[1:])]
    pairs = {pair: pairs_list.count(pair) for pair in set(pairs_list)}
    return pairs


def process_insertion_step(pairs, counts, insertions):
    insertion_pairs = [i[0] for i in insertions]
    new_pairs = copy(pairs)
    for pair in pairs:
        if pair not in insertion_pairs:
            continue
        total = pairs[pair]
        insertion = [i[1] for i in insertions if i[0] == pair][0]
        new_pair = pair[0] + insertion
        second_new_pair = insertion + pair[1]
        if insertion not in counts:
            counts[insertion] = total
        else:
            counts[insertion] += total
        if new_pair not in new_pairs:
            new_pairs[new_pair] = total
        else:
            new_pairs[new_pair] += total
        if second_new_pair not in new_pairs:
            new_pairs[second_new_pair] = total
        else:
            new_pairs[second_new_pair] += total
        new_pairs[pair] -= total

    return new_pairs, counts


def process_steps(pairs, counts, insertions, steps=0):
    
    for _ in range(steps):
        pairs, counts = process_insertion_step(pairs, counts, insertions)

    return pairs, counts


def score_counts(counts):
    scores = sorted([v for v in counts.values()], reverse=True)
    print(scores)
    most_common = scores[0]
    least_common = scores[-1]
    return most_common - least_common


def main():
    with open("data.txt") as f:
        lines = [l.replace("\n", "") for l in f.readlines() if l != "\n"]
    template = lines[0]
    insertions = [i.split(" -> ") for i in lines[1:]]
    pairs = get_pairs_from_template(template)
    counts = get_counts_from_template(template)
    _, ten_steps = process_steps(pairs, counts, insertions, steps=10)
    print(score_counts(ten_steps))
    counts = get_counts_from_template(template)
    _, forty_steps = process_steps(pairs, counts, insertions, steps=40)
    print(score_counts(forty_steps))


if __name__ == "__main__":
    main()
