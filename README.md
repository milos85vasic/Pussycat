# Pussycat - Android logcat viewer V1.0

Sstatus: Under the development.

## Dependencies:

- termcolor:
```
sudo pip install termcolor
```

## Usage:

Live logcat data:
```
python pussycat.py --adb
```

Or offline logcat file:
```
python pussycat.py samples/logcat01.txt
```

## Commands:

When pussycat is running and displaying your logcat data we may pass commands or filtering rules.

- To stop and terminate pussycat:
```
--stop
```

- To filter content enter string + return