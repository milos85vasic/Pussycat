package net.milosvasic.pussycat.android.data

import com.android.ddmlib.Log
import com.android.ddmlib.logcat.LogCatMessage
import java.util.*
import java.util.regex.Matcher

class LogCatMessageParser {

    companion object {
        val TERMINAL_DUMP_PATTERN = "(\\d+-\\d+)\\s+(\\d+:\\d+:\\d+.\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\w)\\s+(.+?):(.+?)"
        val TERMINAL_DUMP_PATTERN_CROPPED = "(\\d+-\\d+)\\s+(\\d+:\\d+:\\d+.\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\w)\\s+(.+?):"
    }

    private val terminalDumpPattern = LogCatMessagePattern(TERMINAL_DUMP_PATTERN, object : LogCatMessageObtain {
        override fun getMessage(matcher: Matcher): LogCatMessage {
            var logLevel = Log.LogLevel.getByLetterString(matcher.group(5))
            if (logLevel == null && matcher.group(5) == "F") {
                logLevel = Log.LogLevel.ASSERT
            }
            val logMessage = if (matcher.groupCount() == 7) {
                matcher.group(7)
            } else {
                ""
            }
            return LogCatMessage(
                    logLevel,
                    matcher.group(3).trim(),
                    matcher.group(4).trim(),
                    matcher.group(6).trim(),
                    matcher.group(6).trim(),
                    matcher.group(2).trim(),
                    logMessage.trim()
            )
        }
    })
    private val terminalDumpPatternCropped = LogCatMessagePattern(TERMINAL_DUMP_PATTERN_CROPPED, object : LogCatMessageObtain {
        override fun getMessage(matcher: Matcher): LogCatMessage {
            var logLevel = Log.LogLevel.getByLetterString(matcher.group(5))
            if (logLevel == null && matcher.group(5) == "F") {
                logLevel = Log.LogLevel.ASSERT
            }
            return LogCatMessage(
                    logLevel,
                    matcher.group(3).trim(),
                    matcher.group(4).trim(),
                    matcher.group(6).trim(),
                    matcher.group(6).trim(),
                    matcher.group(2).trim(),
                    ""
            )
        }
    })

    private val patterns = listOf(
            terminalDumpPattern,
            terminalDumpPatternCropped
    )

    fun processLogLines(lines: Array<String>): Collection<LogCatMessage> {
        val messages = TreeMap<String, LogCatMessage>()
        for (line in lines) {
            if (line.isEmpty()) {
                continue
            }
            var matched = false
            for (logCatPattern in patterns) {
                val pattern = logCatPattern.compile()
                val matcher = pattern.matcher(line.trim())
                if (matcher.matches()) {
                    matched = true
                    val m = logCatPattern.getMessage(matcher)
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