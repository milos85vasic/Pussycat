import os
import sys

import time
import threading

from termcolor import colored, cprint
from common import pussycat_filter_file


class Receiver:
    data = None
    refreshing = None
    current_filter = None

    def __init__(self):
        self.data = list
        self.refreshing = False
        self.current_filter = ''

    def print_line(self, line):
        out = None
        if line.__contains__(" D/") or line.__contains__(" D "):
            out = colored(line, 'grey')
        if line.__contains__(" D/") or line.__contains__(" D "):
            out = colored(line, 'white')
        if line.__contains__(" I/") or line.__contains__(" I "):
            out = colored(line, 'cyan')
        if line.__contains__(" W/") or line.__contains__(" W "):
            out = colored(line, 'yellow')
        if line.__contains__(" E/") or line.__contains__(" E "):
            out = colored(line, 'red')
        cprint(out)

    def receiver(self):
        while True:
            received = sys.stdin.readline().rstrip('\n')
            if received:
                self.data.append(received)
                if self.filter_ok(received) and not self.refreshing:
                    self.print_line(received)
            else:
                time.sleep(1)
        return

    def logcat_filter(self):
        global current_filter
        while True:
            if not os.path.isfile(pussycat_filter_file):
                if current_filter != '':
                    current_filter = ''
                    self.apply_filter()
            else:
                with open(pussycat_filter_file) as filter_file:
                    filter_to_apply = filter_file.readlines()
                    filter_file.close()
                    if filter_to_apply == current_filter:
                        pass
                    current_filter = filter_to_apply
                    self.apply_filter()
        time.sleep(1)

    def apply_filter(self):
        refreshing = True
        print(chr(27) + "[2J")
        for item in self.data:
            if self.filter_ok(item):
                self.print_line(item)
        refreshing = False

    def filter_ok(self, reveived):
        if current_filter != '':
            pass
        return True

    def start(self):
        receiver = threading.Thread(target=self.receiver())
        receiver.start()

        data_filter = threading.Thread(target=self.logcat_filter())
        data_filter.start()


receiver = Receiver()
receiver.start()
