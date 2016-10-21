import sys

from colors import Colors

if sys.argv:
    for arg in sys.argv:
        with open(arg) as f:
            for line in f:
                if line:
                    line = line.strip()
                    color = None
                    if line.__contains__(" I/"):
                        color = Colors.BLUE
                    if line.__contains__(" W/"):
                        color = Colors.WARNING
                    if line.__contains__(" E/"):
                        color = Colors.ERROR
                    if color:
                        print color + line + Colors.ENDC
                    else:
                        print line
                else:
                    print "--- !"
else:
    sys.exit(1)
