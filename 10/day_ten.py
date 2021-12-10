
from statistics import median


def remove_complete_chunks(line):
    complete_chunks = ["()", "[]", "{}", "<>"]
    reduced_line = line
    for complete in complete_chunks:
        reduced_line = reduced_line.replace(complete, "")

    if any(c in reduced_line for c in complete_chunks):
        reduced_line = remove_complete_chunks(reduced_line)
    return reduced_line


def find_first_illegal_char(line):
    for c in line:
        if c in [")", "]", "}", ">"]:
            return c


def score_illegal_char(char):
    scores = {
        ")": 3,
        "]": 57,
        "}": 1197,
        ">": 25137
    }
    return scores[char]


def total_score_corrupted(lines):
    total = 0
    for line in lines:
        reduced_line = remove_complete_chunks(line)
        if not reduced_line:
            continue
        first_illegal_char = find_first_illegal_char(reduced_line)
        if first_illegal_char is None:
            continue
        total += score_illegal_char(first_illegal_char)
    return total


def complete_line(line):
    closing_braket = {
        "(": ")",
        "[": "]",
        "{": "}",
        "<": ">"
    }
    closing_brakets = ""
    for c in reversed(line):
        closing_brakets += closing_braket[c]
    return closing_brakets


def score_line_completion(closing_brakets):
    scores = {
        ")": 1,
        "]": 2,
        "}": 3,
        ">": 4
    }
    total = 0
    for c in closing_brakets:
        total *= 5
        total += scores[c]
    return total


def total_score_incomplete(lines):
    scores = []
    for line in lines:
        reduced_line = remove_complete_chunks(line)
        if not reduced_line:
            continue
        first_illegal_char = find_first_illegal_char(reduced_line)
        if first_illegal_char is not None:
            continue
        closing_brakets = complete_line(reduced_line)
        scores.append(score_line_completion(closing_brakets))
    return median(scores)


def main():
    with open("data.txt") as f:
        lines = [l.replace("\n", "") for l in f.readlines()]
        print(total_score_corrupted(lines))
        print(total_score_incomplete(lines))


if __name__ == "__main__":
    main()
