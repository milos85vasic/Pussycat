import sys

import time
import threading
from termcolor import colored, cprint


def handle_line(line):
    if line:
        line = line.strip()
        if line.__contains__(" D/") or line.__contains__(" D "):
            line = colored(line, 'grey')
        if line.__contains__(" D/") or line.__contains__(" D "):
            line = colored(line, 'white')
        if line.__contains__(" I/") or line.__contains__(" I "):
            line = colored(line, 'cyan')
        if line.__contains__(" W/") or line.__contains__(" W "):
            line = colored(line, 'yellow')
        if line.__contains__(" E/") or line.__contains__(" E "):
            line = colored(line, 'red')
        cprint(line)
    else:
        print "--- !"


def worker():
    while True:
        line = sys.stdin.readline()
        if line:
            handle_line(line)
        else:
            time.sleep(1)
    return


t = threading.Thread(target=worker)
t.start()

sys.exit()
