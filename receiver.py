import os
import sys

import time
import threading

from termcolor import colored, cprint
from common import pussycat_filter_file

data = list()
refreshing = False
current_filter = ''


def print_line(line):
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


def receiver():
    while True:
        received = sys.stdin.readline().rstrip('\n')
        if received:
            data.append(received)
            if filter_ok(received) and not refreshing:
                print_line(received)
        else:
            time.sleep(1)
    return


def logcat_filter():
    global current_filter
    while True:
        if not os.path.isfile(pussycat_filter_file):
            if current_filter != '':
                current_filter = ''
                apply_filter()
        else:
            with open(pussycat_filter_file) as filter_file:
                filter_to_apply = filter_file.readlines()
                filter_file.close()
                if filter_to_apply == current_filter:
                    pass
                current_filter = filter_to_apply
                apply_filter()
    time.sleep(1)


def apply_filter():
    refreshing = True
    print(chr(27) + "[2J")
    for item in data:
        if filter_ok(item):
            print_line(item)
    refreshing = False


def filter_ok(reveived):
    if current_filter != '':
    return True


receiver = threading.Thread(target=receiver())
receiver.start()

data_filter = threading.Thread(target=logcat_filter())
data_filter.start()
