#!/usr/bin/env python3
import sys
import re
import collections
def read_file(filename):
    try:
        with open(filename, 'r', encoding='UTF-8') as f:
            return f.read().lower()
    except FileNotFoundError:
        print("The file does not exist!")
        sys.exit(1)
def get_words(text):
    return re.findall(r'\b\w+\b', text)
def get_letters(text):
    return re.findall(r'\b\w', text)
def get_frequency(items):
    return collections.Counter(items)
def get_common_words(words, n=5):
    return get_frequency(words).most_common(n)
def get_successors(words, common_words):
    successors = collections.defaultdict(collections.Counter)
    for i, word in enumerate(words[:-1]):
        successors[word][words[i+1]] += 1
    return successors

def print_stats(words, letters, common_words, successors, output=None):
    output_data = []
    output_data.append(f"Total number of words: {len(words)}")
    output_data.append(f"Total number of unique words: {len(set(words))}")
    output_data.append(f"Alphabetic Letter frequencies Table (most common to least):")
    for letter, freq in get_frequency(letters).most_common():
        output_data.append(f"{letter}: {freq}")

    output_data.append("5 Most common words and their successors:")
    for word, freq in common_words:
        output_data.append(f"{word} ({freq} occurrences)")
        for successor, count in successors[word].most_common(3):
            output_data.append(f"-- {successor}, {count}")

    if output:
        with open(output, 'w') as f:
            f.write('\n'.join(output_data))
    else:
        print('\n'.join(output_data))
def text_stats(filename, output=None):
    text = read_file(filename)
    if text is None:
        sys.exit(1)

    words = get_words(text)
    letters = get_letters(text)
    common_words = get_common_words(words)
    successors = get_successors(words, common_words)

    print_stats(words, letters, common_words, successors, output)

if __name__ == "__main__":
    if len(sys.argv) < 2 or len(sys.argv) > 3:
        print("Usage: ./text_stats.py <input_filename> [output_filename]")
        sys.exit(1)
    elif len(sys.argv) == 2:
        text_stats(sys.argv[1])
    else:
        text_stats(sys.argv[1], sys.argv[2])
