# Pussycat - Android logcat viewer V1.0

Advanced Android logcat reading and filtering utility.

## Features

- Log colors
- Monitor live adb logcat
- Read and filter offline logcat files stored on your filesystem
- Advanced filtering
- Pause / resume console printing

## Dependencies:

- Make sure you have adb set to your system path!

## Usage:

Live logcat data:
```
java -jar Pussycat.jar --adb
```
or (live adb logcat is default if no argument passed)
```
java -jar Pussycat.jar
```

Offline logcat file:
```
java -jar Pussycat.jar ~/samples/logcat01.txt
```

## Commands:

When pussycat is running and displaying your logcat data we may pass commands or filtering rules.

- To stop and terminate pussycat:
```
@@exit
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
Some word && something else && 3th criteria
```

Multi string criteria (logcat item must contain at least on of the strings we pass):
```
Some word || something else || 3th criteria
```

Multi string criteria with negation:
```
Something && !Something else
```

Multi string criteria with negation:
```
!Something || !Something else
```

and so on.

- To clear applied filter:
```
@@reset
```

- To pause printing logs:
```
@@pause
```

- To resume printing logs:
```
@@resume
```

- To see current filtering rules just press enter.