input = [int(line) for line in open('./day01/input.txt').readlines()]

part_one = sum(y > x for (x, y) in zip(input[:-1], input[1:]))
print(part_one)

part_two = sum(y > x for (x, y) in zip(input[:-3], input[3:]))
print(part_two)
