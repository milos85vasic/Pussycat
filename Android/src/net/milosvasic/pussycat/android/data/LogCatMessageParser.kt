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


    fun processLogLines(lines: Array<String>): Collection<LogCatMessage> {
        val messages = TreeMap<String, LogCatMessage>()
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
                            matcher.group(3).trim(),
                            matcher.group(4).trim(),
                            matcher.group(6).trim(),
                            matcher.group(6).trim(),
                            matcher.group(2).trim(),
                            logMessage.trim()
                    )
                    val identifier = "${m.time}_${m.pid}_${m.tid}"
                    val existing = messages[identifier]
                    if (existing != null) {
                        val newMessage = LogCatMessage(
                                m.logLevel,
                                m.pid,
                                m.tid,
                                m.appName,
                                m.tag,
                                m.time,
                                "${existing.message}\n\t${m.message}"
                        )
                        messages[identifier] = newMessage
                    } else {
                        messages.put(identifier, m)
                    }

//                    for (x in 0..matcher.groupCount()) {
//                        println(">>> ${matcher.group(x)}")
//                    }
                    break
                }
            }
            if (!matched) {
                println("Pussycat, log not matched: [ $line ]")
            }
        }
        return messages.values
    }

}