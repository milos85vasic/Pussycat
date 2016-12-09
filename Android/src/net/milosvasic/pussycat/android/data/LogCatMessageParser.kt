package net.milosvasic.pussycat.android.data

import com.android.ddmlib.Log
import com.android.ddmlib.logcat.LogCatMessage
import java.util.*
import java.util.regex.Matcher

class LogCatMessageParser {

    var lastMessage: LogCatMessage? = null

    companion object {
        val UNKNOWN_VALUE = "unknown"
        val TERMINAL_DUMP_PATTERN = "(\\d+-\\d+)\\s+(\\d+:\\d+:\\d+.\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\w)\\s+(.+?):(.+?)"
        val TERMINAL_DUMP_PATTERN_CROPPED = "(\\d+-\\d+)\\s+(\\d+:\\d+:\\d+.\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\w)\\s+(.+?):"
        val ANDROID_STUDIO_DUMP_PATTERN = "(\\d+-\\d+)\\s+(\\d+:\\d+:\\d+.\\d+)\\s+(\\d+)-(\\d+).+?\\s+(\\w)/(.+?):(.+?)"
        val ANDROID_STUDIO_PATTERN_CROPPED = "(\\d+-\\d+)\\s+(\\d+:\\d+:\\d+.\\d+)\\s+(\\d+)-(\\d+).+?\\s+(\\w)/(.+?):"
        val ANDROID_STUDIO_PATTERN_STACKTRACE = "\\s+(.+?)"
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

    private val androidStudioDumpPattern = LogCatMessagePattern(ANDROID_STUDIO_DUMP_PATTERN, object : LogCatMessageObtain {
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
    private val androidStudioDumpPatternCropped = LogCatMessagePattern(ANDROID_STUDIO_PATTERN_CROPPED, object : LogCatMessageObtain {
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
    private val androidStudioDumpPatternStacktrace = LogCatMessagePattern(ANDROID_STUDIO_PATTERN_STACKTRACE, object : LogCatMessageObtain {
        override fun getMessage(matcher: Matcher): LogCatMessage {
            var logLevel = lastMessage?.logLevel
            if (logLevel == null) {
                logLevel = Log.LogLevel.VERBOSE
            }
            var pid = lastMessage?.pid
            if (pid == null) {
                pid = UNKNOWN_VALUE
            }
            var tid = lastMessage?.tid
            if (tid == null) {
                tid = UNKNOWN_VALUE
            }
            var appName = lastMessage?.appName
            if (appName == null) {
                appName = "app $UNKNOWN_VALUE"
            }
            var tag = lastMessage?.tag
            if (tag == null) {
                tag = "tag $UNKNOWN_VALUE"
            }
            var time = lastMessage?.time
            if (time == null) {
                time = "time $UNKNOWN_VALUE"
            }
            return LogCatMessage(
                    logLevel,
                    pid,
                    tid,
                    appName,
                    tag,
                    time,
                    matcher.group(1).trim()
            )
        }
    })

    private val patterns = listOf(
            terminalDumpPattern,
            terminalDumpPatternCropped,
            androidStudioDumpPattern,
            androidStudioDumpPatternCropped,
            androidStudioDumpPatternStacktrace
    )

    fun processLogLines(lines: Array<String>): Collection<LogCatMessage> {
        val messages = LinkedHashMap<String, LogCatMessage>()
        for (line in lines) {
            if (line.isEmpty()) {
                continue
            }
            var matched = false
            for (logCatPattern in patterns) {
                val pattern = logCatPattern.compile()
                val matcher = pattern.matcher(line)
                if (matcher.matches()) {
                    matched = true
                    val message = logCatPattern.getMessage(matcher)
                    val identifier = "${message.time}_${message.pid}_${message.tid}"
                    val existing = messages[identifier]
                    if (existing != null) {
                        val newMessage = LogCatMessage(
                                message.logLevel,
                                message.pid,
                                message.tid,
                                message.appName,
                                message.tag,
                                message.time,
                                "${existing.message}\n\t${message.message}"
                        )
                        messages[identifier] = newMessage
                    } else {
                        lastMessage = message
                        messages.put(identifier, message)
                    }
                    break
                }
            }
            if (!matched) {
                println("Pussycat: log not matched,\n\t$line\n")
            }
        }
        return messages.values
    }

}