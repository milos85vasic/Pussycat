package net.milosvasic.pussycat.android.data

import com.android.ddmlib.Log
import com.android.ddmlib.logcat.LogCatMessage
import java.util.*
import java.util.regex.Pattern


class LogCatMessageParser {

    private var mCurLogLevel: Log.LogLevel? = Log.LogLevel.WARN
    private var mCurPid = "?"
    private var mCurTid = "?"
    private var mCurTag = "?"
    private var mCurTime = "?:??"

    private val terminalDumpPattern = Pattern.compile(
            "(\\d+-\\d+)\\s+(\\d+:\\d+:\\d+.\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\w)\\s+(.*):\\s+(((.|\n)*?)|\\s+)"
    )

    private val terminalDumpPatternCropped = Pattern.compile(
            "(\\d+-\\d+)\\s+(\\d+:\\d+:\\d+.\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\w)\\s+(.*):"
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

                    for (x in 0..matcher.groupCount()) {
                        println(">>> ${matcher.group(x)}")
                    }
                    break
                }
            }
            if (!matched) {
                println("NO MATCHES $line")
            } else {

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