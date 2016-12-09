package net.milosvasic.pussycat.android.data

import com.android.ddmlib.Log
import com.android.ddmlib.logcat.LogCatMessage
import java.util.*
import java.util.regex.Pattern


class LogCatMessageParser {

    private val terminalDumpPattern = Pattern.compile(
            "(\\d+-\\d+)\\s+(\\d+:\\d+:\\d+.\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\w)\\s+(.+?):(.+?)"
    )

    private val terminalDumpPatternCropped = Pattern.compile(
            "(\\d+-\\d+)\\s+(\\d+:\\d+:\\d+.\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\w)\\s+(.+?):"
    )

    private val patterns = listOf<Pattern>(
            terminalDumpPattern,
            terminalDumpPatternCropped
    )

//    private val terminalDumpPattern = Pattern.compile(
//            "^\\[\\s(\\d\\d-\\d\\d\\s\\d\\d:\\d\\d:\\d\\d\\.\\d+)" + "\\s+(\\d*):\\s*(\\S+)\\s([VDIWEAF])/(.*)\\]$"
//    )


    fun processLogLines(lines: Array<String>): List<LogCatMessage> {
        val messages = ArrayList<LogCatMessage>(lines.size)
        for (line in lines) {
            if (line.isEmpty()) {
                continue
            }
            var matched = false
            for (pattern in patterns) {
                val matcher = pattern.matcher(line.trim())
                if (matcher.matches()) {
                    matched = true
                    var logLevel = Log.LogLevel.getByLetterString(matcher.group(5))
                    if (logLevel == null && matcher.group(5) == "F") {
                        logLevel = Log.LogLevel.ASSERT
                    }
                    val logMessage = if (matcher.groupCount() == 7) {
                        matcher.group(7)
                    } else {
                        ""
                    }
                    val m = LogCatMessage(
                            logLevel,
                            matcher.group(3),
                            matcher.group(4),
                            matcher.group(6),
                            matcher.group(6),
                            matcher.group(2),
                            logMessage
                    )
                    messages.add(m)

//                    for (x in 0..matcher.groupCount()) {
//                        println(">>> ${matcher.group(x)}")
//                    }
                    break
                }
            }
            if (!matched) {
                val m = LogCatMessage(
                        Log.LogLevel.VERBOSE,
                        "?",
                        "?",
                        "?",
                        "?",
                        "?",
                        line
                )
                messages.add(m)
            }


//            if (matcher.matches()) {
//                println("MATCHING")

//                mCurTime = matcher.group(1)
//                mCurPid = matcher.group(2)
//                mCurTid = matcher.group(3)
//                mCurLogLevel = Log.LogLevel.getByLetterString(matcher.group(4))
//                mCurTag = matcher.group(5).trim { it <= ' ' }
//
//                if (mCurLogLevel == null && matcher.group(4) == "F") {
//                    mCurLogLevel = Log.LogLevel.ASSERT
//                }
//            } else {
//                println("NO MATCHES $line")
//                var pkgName = "" //$NON-NLS-1$
//                val pid = Ints.tryParse(mCurPid)
//                if (pid != null && device != null) {
//                    pkgName = device.getClientName(pid)
//                }
//                val m = LogCatMessage(mCurLogLevel, mCurPid, mCurTid,
//                        pkgName, mCurTag, mCurTime, line)
//                messages.add(m)
//            }
        }
        return messages
    }

}