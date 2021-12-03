test_input = ["00100", "11110", "10110", "10111", "10101", "01111", "00111", "11100", "10000", "11001", "00010", "01010"]

def part_one(report):
    length = len(report[0])
    gamma = ''
    epsilon = ''
    for i in range(length):
        chars = [line[i] for line in report]
        ones = [c for c in chars if c == '1']
        zeros = [c for c in chars if c == '0']
        if len(ones) > len(zeros):
            gamma += '1'
            epsilon += '0'
        else:
            gamma += '0'
            epsilon += '1'

    return int(gamma, 2) * int(epsilon, 2)

assert part_one(test_input) == 198

def part_two(report):
    length = len(report[0])
    gen_report = [line for line in report]
    scrub_report = [line for line in report]

    for i in range(length):
        chars = [line[i] for line in gen_report]
        ones = [c for c in chars if c == '1']
        zeros = [c for c in chars if c == '0']
        target = '0' if len(zeros) > len(ones) else '1'
        gen_report = [line for line in gen_report if line[i] == target]
        if len(gen_report) == 1:
            break

    for i in range(length):
        chars = [line[i] for line in scrub_report]
        ones = [c for c in chars if c == '1']
        zeros = [c for c in chars if c == '0']
        target = '1' if len(zeros) > len(ones) else '0'
        scrub_report = [line for line in scrub_report if line[i] == target]
        if len(scrub_report) == 1:
            break

    gen_rating = int(gen_report[0], 2)
    scrub_rating = int(scrub_report[0], 2)
    return gen_rating * scrub_rating

assert part_two(test_input) == 230

def read_input(filepath):
    with open(filepath) as f:
        return [line.strip() for line in f]

report = read_input('day03/input.txt')

print(part_one(report))
print(part_two(report))