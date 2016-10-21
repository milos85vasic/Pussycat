# Pussycat - Android logcat viewer V1.0

Sstatus: Under the development.

## Dependencies:

- termcolor:
```
sudo pip install termcolor
```

## Usage:

Pass logcat data stream using pipe.

Live logcat data:
```
adb logcat | python pussycat.py
```

Or offline logcat file:
```
cat samples/logcat01.txt | python pussycat.py
```