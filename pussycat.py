import os
import sys

import subprocess

import signal

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
    if received == 'stop':
        kill_receiver()
        break

sys.exit()
