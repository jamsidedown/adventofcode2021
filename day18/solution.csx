using System;
using System.Text.RegularExpressions;

static string[] Parse(string filepath)
{
    return File.ReadAllLines(filepath);
}

static long PartOne(string[] snailNumbers)
{
    return Magnitude(AddReduce(snailNumbers));
}

static long PartTwo(string[] snailNumbers)
{
    var max = 0L;
    for (int i = 0; i < snailNumbers.Length; i++)
    {
        for (int j = 0; j < snailNumbers.Length; j++)
        {
            if (i == j)
                continue;

            var mag = Magnitude(AddReduce(new[] {snailNumbers[i], snailNumbers[j]}));
            if (mag > max)
                max = mag;
        }
    }

    return max;
}

static string AddReduce(string[] snailNumbers)
{
    var current = snailNumbers[0];
    foreach (var snailNumber in snailNumbers[1..])
    {
        current = Reduce(Add(current, snailNumber));
    }
    return current;
}

static Regex _pairPattern = new Regex(@"^(.*?)\[(\d+),(\d+)\](.*)$");

static long Magnitude(string snailNumber)
{
    var current = snailNumber;
    Match match;
    while ((match = _pairPattern.Match(current)).Success)
    {
        var head = match.Groups[1].Value;
        var first = long.Parse(match.Groups[2].Value);
        var second = long.Parse(match.Groups[3].Value);
        var tail = match.Groups[4].Value;
        var mag = 3 * first + 2 * second;
        current = $"{head}{mag}{tail}";
    }

    return long.Parse(current);
}

static string Add(string first, string second) => $"[{first},{second}]";

static Regex _numberPattern = new Regex(@"^\[(\d+),(\d+)\](.*)$");

static string Explode(string snailNumber)
{
    var depth = 0;

    for (var i = 0; i < snailNumber.Length; i++)
    {
        var c = snailNumber[i];

        if (c == '[')
            depth++;
        else if (c == ']')
            depth--;

        if (depth > 4)
        {
            var match = _numberPattern.Match(snailNumber[i..]);
            if (match.Success)
            {
                var groups = match.Groups;
                var first = int.Parse(groups[1].Value);
                var second = int.Parse(groups[2].Value);

                var head = snailNumber[..i];
                var tail = groups[3].Value;

                return $"{ExplodeLeft(head, first)}0{ExplodeRight(tail, second)}";
            }
        }
    }

    return snailNumber;
}

static Regex _rightmostPattern = new Regex(@"^(.*[^\d])?(\d+)(.*?)$");
static Regex _leftmostPattern = new Regex(@"^(.*?)(\d+)(.*)$");

static string ExplodeLeft(string snailPart, int add)
{
    var match = _rightmostPattern.Match(snailPart);
    if (match.Success)
    {
        var groups = match.Groups;
        var start = groups[1].Value;
        var end = groups[3].Value;

        var number = int.Parse(groups[2].Value);

        return $"{start}{number + add}{end}";
    }

    return snailPart;
}

static string ExplodeRight(string snailPart, int add)
{
    var match = _leftmostPattern.Match(snailPart);
    if (match.Success)
    {
        var groups = match.Groups;
        var start = groups[1].Value;
        var end = groups[3].Value;

        var number = int.Parse(groups[2].Value);

        return $"{start}{number + add}{end}";
    }

    return snailPart;
}

static Regex _splitPattern = new Regex(@"^(.*?)(\d{2,})(.*)$");

static string Split(string snailNumber)
{
    var match = _splitPattern.Match(snailNumber);
    if (match.Success)
    {
        var groups = match.Groups;
        var head = groups[1].Value;
        var tail = groups[3].Value;

        var number = int.Parse(groups[2].Value);

        return $"{head}{Split(number)}{tail}";
    }

    return snailNumber;
}

static string Split(int snailInt) => $"[{snailInt / 2},{(snailInt + 1) / 2}]";

static string Reduce(string snailNumber)
{
    var current = snailNumber;
    while (true)
    {
        var exploded = Explode(current);
        if (exploded != current)
        {
            current = exploded;
            continue;
        }

        var split = Split(current);
        if (split != current)
        {
            current = split;
            continue;
        }

        return current;
    }
}

var input = Parse("day18/input.txt");
Console.WriteLine(PartOne(input));
Console.WriteLine(PartTwo(input));
