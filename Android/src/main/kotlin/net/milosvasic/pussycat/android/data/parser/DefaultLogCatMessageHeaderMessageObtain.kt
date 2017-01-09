package net.milosvasic.pussycat.android.data.parser

import com.android.ddmlib.Log
import com.android.ddmlib.logcat.LogCatHeader
import com.android.ddmlib.logcat.LogCatMessage
import com.android.ddmlib.logcat.LogCatTimestamp
import net.milosvasic.pussycat.android.data.AndroidLogCatMessage
import java.util.regex.Matcher


class DefaultLogCatMessageHeaderMessageObtain : LogCatMessageObtain {

    var lastHeader: LogCatHeader? = null

    override fun getMessage(matcher: Matcher): AndroidLogCatMessage {
        var pid = -1
        try {
            pid = Integer.parseInt(matcher.group(2))
        } catch (ignored: NumberFormatException) {
        }
        var tid = -1
        try {
            tid = Integer.decode(matcher.group(3))!!
        } catch (ignored: NumberFormatException) {
        }
        var logLevel: Log.LogLevel? = Log.LogLevel.getByLetterString(matcher.group(4))
        if (logLevel == null && matcher.group(4) == "F") {
            logLevel = Log.LogLevel.ASSERT
        }
        if (logLevel == null) {
            logLevel = Log.LogLevel.WARN
        }
        lastHeader = LogCatHeader(
                logLevel,
                pid,
                tid,
                matcher.group(5),
                matcher.group(5),
                LogCatTimestamp.fromString(matcher.group(1))
        )
        return AndroidLogCatMessage.getFrom(LogCatMessage(lastHeader, ""))
    }
}