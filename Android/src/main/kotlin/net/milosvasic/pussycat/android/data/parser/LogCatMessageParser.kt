package net.milosvasic.pussycat.android.data.parser

import com.android.ddmlib.Log
import com.android.ddmlib.logcat.LogCatMessage
import net.milosvasic.pussycat.android.data.AndroidLogCatMessage
import net.milosvasic.pussycat.content.Messages
import java.util.*
import java.util.regex.Matcher
import java.util.LinkedHashMap

class LogCatMessageParser {

    var lastMessage: AndroidLogCatMessage? = null
    private val listeners = HashSet<LogCatMessageParserMatcherListener>()

    companion object {
        val TERMINAL_DUMP_PATTERN = "(\\d+-\\d+)\\s+(\\d+:\\d+:\\d+.\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\w)\\s+(.+?):(.+?)"
        val TERMINAL_DUMP_PATTERN_CROPPED = "(\\d+-\\d+)\\s+(\\d+:\\d+:\\d+.\\d+)\\s+(\\d+)\\s+(\\d+)\\s+(\\w)\\s+(.+?):"
        val ANDROID_STUDIO_DUMP_PATTERN = "(\\d+-\\d+)\\s+(\\d+:\\d+:\\d+.\\d+)\\s+(\\d+)-(\\d+).+?\\s+(\\w)/(.+?):(.+?)"
        val ANDROID_STUDIO_PATTERN_CROPPED = "(\\d+-\\d+)\\s+(\\d+:\\d+:\\d+.\\d+)\\s+(\\d+)-(\\d+).+?\\s+(\\w)/(.+?):"
        val ANDROID_STUDIO_PATTERN_STACKTRACE = "\\s+(.+?)"
        val ACER_Z520_PATTERN = "(\\w)/(\\w+).+?(\\w+).:\\s+(.+?)"
        val ACER_Z520_PATTERN2 = "(\\w)/(\\s+).+?(\\w+).:\\s+(.+?)"
        val ACER_Z520_PATTERN3 = "(\\w)/(\\w+).+?(\\w+).:\\s+"
        val ACER_Z520_PATTERN4 = "(\\w)/(\\s+).+?(\\w+).:\\s+"
        val ACER_Z520_PATTERN5 = "(\\w)/\\Q.\\E(\\w+).+?(\\w+).:\\s+(.+?)"
        val ACER_Z520_PATTERN6 = "(\\w)/(\\s+)\\Q.\\E.+?(\\w+).:\\s+(.+?)"
        val DEFAULT_HEAD_PATTERN = "^\\[\\s(\\d\\d-\\d\\d\\s\\d\\d:\\d\\d:\\d\\d\\.\\d+)\\s+(\\d*):\\s*(\\S+)\\s([VDIWEAF])/(.*[^\\s])\\s+\\]$"

        fun getIdentifier(message: AndroidLogCatMessage): String {
            return "${message.time}_${message.pid}_${message.tid}"
        }
    }

    private val terminalDumpPattern = LogCatMessagePattern(TERMINAL_DUMP_PATTERN, object : LogCatMessageObtain {
        override fun getMessage(matcher: Matcher): AndroidLogCatMessage {
            var logLevel = Log.LogLevel.getByLetterString(matcher.group(5))
            if (logLevel == null && matcher.group(5) == "F") {
                logLevel = Log.LogLevel.ASSERT
            }
            val logMessage = if (matcher.groupCount() == 7) {
                matcher.group(7)
            } else {
                ""
            }
            return AndroidLogCatMessage(
                    logLevel,
                    Integer.valueOf(matcher.group(3).trim()),
                    Integer.valueOf(matcher.group(4).trim()),
                    matcher.group(6).trim(),
                    matcher.group(6).trim(),
                    matcher.group(2).trim(),
                    logMessage.trim()
            )
        }
    })
    private val terminalDumpPatternCropped = LogCatMessagePattern(TERMINAL_DUMP_PATTERN_CROPPED, object : LogCatMessageObtain {
        override fun getMessage(matcher: Matcher): AndroidLogCatMessage {
            var logLevel = Log.LogLevel.getByLetterString(matcher.group(5))
            if (logLevel == null && matcher.group(5) == "F") {
                logLevel = Log.LogLevel.ASSERT
            }
            return AndroidLogCatMessage(
                    logLevel,
                    Integer.valueOf(matcher.group(3).trim()),
                    Integer.valueOf(matcher.group(4).trim()),
                    matcher.group(6).trim(),
                    matcher.group(6).trim(),
                    matcher.group(2).trim(),
                    ""
            )
        }
    })

    private val androidStudioDumpPattern = LogCatMessagePattern(ANDROID_STUDIO_DUMP_PATTERN, object : LogCatMessageObtain {
        override fun getMessage(matcher: Matcher): AndroidLogCatMessage {
            var logLevel = Log.LogLevel.getByLetterString(matcher.group(5))
            if (logLevel == null && matcher.group(5) == "F") {
                logLevel = Log.LogLevel.ASSERT
            }
            val logMessage = if (matcher.groupCount() == 7) {
                matcher.group(7)
            } else {
                ""
            }
            return AndroidLogCatMessage(
                    logLevel,
                    Integer.valueOf(matcher.group(3).trim()),
                    Integer.valueOf(matcher.group(4).trim()),
                    matcher.group(6).trim(),
                    matcher.group(6).trim(),
                    matcher.group(2).trim(),
                    logMessage.trim()
            )
        }
    })
    private val androidStudioDumpPatternCropped = LogCatMessagePattern(ANDROID_STUDIO_PATTERN_CROPPED, object : LogCatMessageObtain {
        override fun getMessage(matcher: Matcher): AndroidLogCatMessage {
            var logLevel = Log.LogLevel.getByLetterString(matcher.group(5))
            if (logLevel == null && matcher.group(5) == "F") {
                logLevel = Log.LogLevel.ASSERT
            }
            return AndroidLogCatMessage(
                    logLevel,
                    Integer.valueOf(matcher.group(3).trim()),
                    Integer.valueOf(matcher.group(4).trim()),
                    matcher.group(6).trim(),
                    matcher.group(6).trim(),
                    matcher.group(2).trim(),
                    ""
            )
        }
    })
    private val androidStudioDumpPatternStacktrace = LogCatMessagePattern(ANDROID_STUDIO_PATTERN_STACKTRACE, object : LogCatMessageObtain {
        override fun getMessage(matcher: Matcher): AndroidLogCatMessage {
            var logLevel = lastMessage?.logLevel
            if (logLevel == null) {
                logLevel = Log.LogLevel.VERBOSE
            }
            var pid = lastMessage?.pid
            if (pid == null) {
                pid = -1
            }
            var tid = lastMessage?.tid
            if (tid == null) {
                tid = -1
            }
            var appName = lastMessage?.appName
            if (appName == null) {
                appName = ""
            }
            var tag = lastMessage?.tag
            if (tag == null) {
                tag = ""
            }
            var time = lastMessage?.time
            if (time == null) {
                time = ""
            }
            return AndroidLogCatMessage(
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

    val acerMessageObtain = object : LogCatMessageObtain {
        override fun getMessage(matcher: Matcher): AndroidLogCatMessage {
            var logLevel = Log.LogLevel.getByLetterString(matcher.group(1))
            if (logLevel == null && matcher.group(1) == "F") {
                logLevel = Log.LogLevel.ASSERT
            }
            val logMessage = if (matcher.groupCount() == 4) {
                matcher.group(4)
            } else {
                ""
            }
            return AndroidLogCatMessage(
                    logLevel,
                    Integer.valueOf(matcher.group(3).trim()),
                    -1,
                    matcher.group(2).trim(),
                    matcher.group(2).trim(),
                    "",
                    logMessage.trim()
            )
        }
    }

    private val acerZ520Pattern = LogCatMessagePattern(ACER_Z520_PATTERN, acerMessageObtain)
    private val acerZ520Pattern2 = LogCatMessagePattern(ACER_Z520_PATTERN2, acerMessageObtain)
    private val acerZ520Pattern3 = LogCatMessagePattern(ACER_Z520_PATTERN3, acerMessageObtain)
    private val acerZ520Pattern4 = LogCatMessagePattern(ACER_Z520_PATTERN4, acerMessageObtain)
    private val acerZ520Pattern5 = LogCatMessagePattern(ACER_Z520_PATTERN5, acerMessageObtain)
    private val acerZ520Pattern6 = LogCatMessagePattern(ACER_Z520_PATTERN6, acerMessageObtain)

    private val defaultLogCatMessageHeaderMessageObtain = DefaultLogCatMessageHeaderMessageObtain()
    private val defaultHeadPattern = DefaultLogCatMessageHeaderPattern(DEFAULT_HEAD_PATTERN, defaultLogCatMessageHeaderMessageObtain)

    private val patterns = listOf(
            terminalDumpPattern,
            terminalDumpPatternCropped,
            androidStudioDumpPattern,
            androidStudioDumpPatternCropped,
            androidStudioDumpPatternStacktrace,
            defaultHeadPattern,
            acerZ520Pattern,
            acerZ520Pattern2,
            acerZ520Pattern3,
            acerZ520Pattern4,
            acerZ520Pattern5,
            acerZ520Pattern6
    )

    fun processLogLines(lines: Array<String>): Collection<AndroidLogCatMessage> {
        val messages = Collections.synchronizedMap(LinkedHashMap<String, AndroidLogCatMessage>())
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
                    val identifier = getIdentifier(message)
                    val existing = messages[identifier]
                    if (existing != null) {
                        existing.appendToStacktrace(message.msg)
                        notify(true, existing)
                    } else {
                        lastMessage = message
                        messages.put(identifier, message)
                        notify(true, message)
                    }
                    break
                }
            }
            if (!matched) {
                val header = defaultHeadPattern.defaultMessageObtain.lastHeader
                if (header != null) {
                    val identifier = "${header.timestamp}_${header.pid}_${header.tid}_${header.appName}_${header.tag}"
                    val existing = messages[identifier]
                    if (existing != null) {
                        existing.appendToStacktrace(line)
                        notify(true, existing)
                    } else {
                        val message = AndroidLogCatMessage.getFrom(LogCatMessage(header, line))
                        lastMessage = message
                        messages.put(identifier, message)
                        notify(true, message)
                    }
                } else {
                    val message = AndroidLogCatMessage(Log.LogLevel.ERROR, -1, -1, Messages.PPE, Messages.PPE, Messages.PPE, "${Messages.PPE}: $line")
                    messages.put(System.currentTimeMillis().toString(), message)
                    notify(false, message)
                }
            }
        }
        return messages.values
    }

    fun subscribe(listener: LogCatMessageParserMatcherListener) {
        listeners.add(listener)
    }

    fun unsubscribe(listener: LogCatMessageParserMatcherListener) {
        listeners.remove(listener)
    }

    fun notify(success: Boolean, message: AndroidLogCatMessage) {
        for (listener in listeners) {
            listener.onMatch(success, message)
        }
    }

}