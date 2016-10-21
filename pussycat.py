import os
import sys

import subprocess

import signal

from common import pussycat_filter_file

receiver = None


def signal_handler(signal, frame):
    kill_receiver()
    print "\nBye, bye!"
    sys.exit(0)


def kill_receiver():
    if receiver:
        print "Killing data receiver."
        os.killpg(os.getpgid(receiver.pid), signal.SIGTERM)
    else:
        print "No data receiver running."


def apply_filter(filter_to_apply):
    clear_filter()
    with open(pussycat_filter_file, "w") as filter_file:
        filter_file.write(filter_to_apply.strip())
        filter_file.close()


def clear_filter():
    if os.path.isfile(pussycat_filter_file):
        os.remove(pussycat_filter_file)


signal.signal(signal.SIGINT, signal_handler)
if sys.argv:
    for arg in sys.argv:
        if arg.strip() == '--adb':
            receiver = subprocess.Popen("adb logcat | python receiver.py", stdin=subprocess.PIPE, shell=True)
        else:
            if arg.strip() != 'pussycat.py':
                receiver = subprocess.Popen("cat " + arg + " | python receiver.py", stdin=subprocess.PIPE, shell=True)

while True:
    received = sys.stdin.readline().rstrip('\n')
    if received == '@@stop':
        kill_receiver()
        break
    if received == '@@clear':
        print(chr(27) + "[2J")
        continue
    if received == '@@reset':
        clear_filter()
        continue
    else:
        apply_filter(received)

sys.exit()
