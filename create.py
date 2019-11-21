#!/usr/bin/python3
import os
import re
from shutil import copyfile


def is_prompt(file_type):
    return file_type.lower() == "prompt"


if __name__ == '__main__':
    year = input('Year:\n')
    day = input('Day:\n')
    file_type = input('Prompt/Input/Output:\n').lower()
    if is_prompt(file_type):
        prompt_file = input('Prompt Link:\n')
        file_name = f'data/prompt-{year}-{day}.txt'
        copyfile(prompt_file, file_name)
    else:
        problem = input('Problem A/B:\n')
        prog = re.compile(f'^{file_type}-{year}-{day}-{problem}-([0-9]+).txt$')
        largest = -1
        for file_name in os.listdir('data'):
            match = prog.search(file_name)
            if match:
                num = int(match.group(1))
                largest = max(num, largest)
        next_num = largest + 1
        file_name = f'data/{file_type}-{year}-{day}-{problem}-{next_num}.txt'
        print(f'Data for {file_name}:\n')
        with open(file_name, 'w') as fp:
            i = 0
            while True:
                try:
                    s = input()
                    if i > 0:
                        fp.write('\n')
                    fp.write(s)
                except EOFError:
                    break
                i += 1
