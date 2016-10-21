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
Live adb logcat is default if no argument passed!

Or offline logcat file:
```
python pussycat.py samples/logcat01.txt
```

## Commands:

When pussycat is running and displaying your logcat data we may pass commands or filtering rules.

- To stop and terminate pussycat:
```
@@stop
```

- To clear the current output on your screen:
```
@@clear
```

- To filter content enter filtering criteria, then press enter.

Single string criteria (each logcat item must contain string we pass):
```
Some word
```

Multi string criteria (each logcat item must contain all of the strings we pass):
```
Some word, something else, 3th criteria
```

- To clear applied filter:
```
@@reset
```