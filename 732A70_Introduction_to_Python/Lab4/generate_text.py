#!/usr/bin/env python3
import sys
import random
import text_stats
def generate_text(filename, start_word, max_words, output=None):
    text = text_stats.read_file(filename)
    if text is None:
        return "The file does not exist!"

    words = text_stats.get_words(text)
    common_words = text_stats.get_common_words(words)
    successors = text_stats.get_successors(words, common_words)

    cur_word = start_word
    msg = cur_word
    word_count = 1

    while cur_word in successors and word_count < max_words:
        cur_word = random.choices(
            list(successors[cur_word].keys()),
            weights=list(successors[cur_word].values())
        )[0]
        msg += " " + cur_word
        word_count += 1

    if output:
        with open(output, 'w') as f:
            f.write(msg)
    else:
        print(msg)

if __name__ == "__main__":
    if len(sys.argv) < 4 or len(sys.argv) > 5:
        print("Please provide a filename, a starting word, and a maximum number of words as arguments.\n")
        print("Usage: ./generate_text.py <filename> <starting_word> <max_words>")
        sys.exit(1)
    elif len(sys.argv) == 4:
        generate_text(sys.argv[1], sys.argv[2], int(sys.argv[3]))
    else:
        generate_text(sys.argv[1], sys.argv[2], int(sys.argv[3]), sys.argv[4])
