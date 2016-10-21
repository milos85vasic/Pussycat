import sys

from termcolor import colored, cprint

if sys.argv:
    for arg in sys.argv:
        with open(arg) as f:
            for line in f:
                if line:
                    line = line.strip()
                    if line.__contains__(" D/"):
                        line = colored(line, 'grey')
                    if line.__contains__(" D/"):
                        line = colored(line, 'white')
                    if line.__contains__(" I/"):
                        line = colored(line, 'cyan')
                    if line.__contains__(" W/"):
                        line = colored(line, 'yellow')
                    if line.__contains__(" E/"):
                        line = colored(line, 'red')
                    cprint(line)
                else:
                    print "--- !"
else:
    sys.exit(1)
