

digit_to_segments = {
    0: [0,1,2,4,5,6],
    1: [2,5],
    2: [0,2,3,4,6],
    3: [0,2,3,5,6],
    4: [1,2,3,5],
    5: [0,1,3,5,6],
    6: [0,1,3,4,5,6],
    7: [0,2,5],
    8: [0,1,2,3,4,5,6],
    9: [0,1,2,3,5,6]
}


segment_count_to_digits = {
    2: [1],
    3: [7],
    4: [4],
    5: [2,3,5],
    6: [0,6,9],
    7: [8]
}


def get_patterns_and_outputs(line: str):
    elements = line.split()
    patterns = elements[:len(elements) - 5]
    outputs = elements[len(elements) - 4:]
    return patterns, outputs


def find_candidate_segments(char, patterns):

    pattern_segments = []
    containing_char = [p for p in patterns if char in p]
    for pattern in containing_char:
        possible_digits = segment_count_to_digits[len(pattern)]
        for digit in possible_digits:
            segments = digit_to_segments[digit]
            pattern_segments.append(segments)
    return min(pattern_segments, key=len)



def find_mapping(patterns, mapping):

    unmapped_chars = [c for c in mapping if mapping[c] is None]
    available_segments = [i for i in [0,1,2,3,4,5,6] if i not in mapping.values()]

    for char in unmapped_chars:
        candidate_segments = [s for s in find_candidate_segments(char, patterns) if s in available_segments]
        if not candidate_segments:
            raise ValueError("No available segments")
        for segment in candidate_segments:
            mapping[char] = segment
            try:
                find_mapping(patterns, mapping)
                try:
                    for pattern in patterns:
                        map_pattern(mapping, pattern)
                except KeyError:
                    raise ValueError("Mapping is not valid")
                return mapping
            except ValueError:
                mapping[char] = None
                continue
        else:
            raise ValueError("None of the available segments leads to a solution")

    return mapping


def map_pattern(mapping, pattern):

    segments = sorted([mapping[c] for c in pattern])
    digit = list(digit_to_segments.keys())[list(digit_to_segments.values()).index(segments)]
    return digit


def map_output_values(mapping, outputs):

    output_string = ""
    for output in outputs:
        digit = map_pattern(mapping, output)
        output_string += str(digit)
    return int(output_string)


def main():

    with open("data.txt") as data:
        lines = [l.replace("\n", "") for l in data.readlines()]

    output_numbers = []
    for line in lines:
        mapping = {c: None for c in "abcdefg"}
        patterns, outputs = get_patterns_and_outputs(line=line)
        mapping = find_mapping(patterns, mapping)
        output_numbers.append(map_output_values(mapping, outputs))

    print(sum(output_numbers))
    

if __name__ == "__main__":
    main()
