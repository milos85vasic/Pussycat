import os
import sys

from multiprocessing import Process
import time
from termcolor import colored, cprint
from common import pussycat_filter_file


class Receiver:
    data = None
    refreshing = None
    current_filter = None

    def __init__(self):
        self.data = list()
        self.refreshing = False
        self.current_filter = ''

    def print_line(self, line):
        if line.__contains__(" D/") or line.__contains__(" D "):
            cprint(colored(line, 'grey'))
            return
        if line.__contains__(" D/") or line.__contains__(" D "):
            cprint(colored(line, 'white'))
            return
        if line.__contains__(" I/") or line.__contains__(" I "):
            cprint(colored(line, 'cyan'))
            return
        if line.__contains__(" W/") or line.__contains__(" W "):
            cprint(colored(line, 'yellow'))
            return
        if line.__contains__(" E/") or line.__contains__(" E "):
            cprint(colored(line, 'red'))
            return
        print line

    def receiver(self):
        while True:
            received = sys.stdin.readline()
            received = received.strip()
            if received:
                self.data.append(received)
                if not self.refreshing and self.filter_ok(received):
                    self.print_line(received)
            else:
                time.sleep(1)

    def logcat_filter(self):
        while True:
            if not os.path.isfile(pussycat_filter_file):
                if self.current_filter != '':
                    self.current_filter = ''
                    self.apply_filter()
            else:
                with open(pussycat_filter_file) as filter_file:
                    filter_to_apply = filter_file.readlines()
                    filter_file.close()
                    if filter_to_apply != self.current_filter:
                        self.current_filter = filter_to_apply
                        self.apply_filter()
            time.sleep(1)

    def apply_filter(self):
        self.refreshing = True
        print(chr(27) + "[2J")
        for item in enumerate(self.data):
            if self.filter_ok(item):
                self.print_line(item)
        self.refreshing = False

    def filter_ok(self, received):
        if self.current_filter != '':
            if self.current_filter.__contains__(","):
                # TODO: TBD.
                pass
            else:
                return received.__contains__(self.current_filter)
        return True

    def start(self):
        p1 = Process(target=self.receiver)
        p2 = Process(target=self.logcat_filter)
        p1.start()
        p2.start()
        p1.join()
        p2.join()


receiver = Receiver()
receiver.start()
