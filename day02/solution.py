from functools import reduce
from operator import mul

directions = {'forward': (1, 0), 'down': (0, 1), 'up': (0, -1)}

def vector(direction, magnitude):
    return tuple(int(magnitude) * val for val in directions[direction])

def add(a, b):
    return (a[0] + b[0], a[1] + b[1])

def parse_aim(input):
    aim = 0
    for (d, a) in input:
        if d:
            yield (d, d * aim)
        aim += a

input = [vector(*line.split()) for line in open('./day02/input.txt').readlines()]

part_one = reduce(mul, reduce(add, input))
print(part_one)

part_two = reduce(mul, reduce(add, parse_aim(input)))
print(part_two)
