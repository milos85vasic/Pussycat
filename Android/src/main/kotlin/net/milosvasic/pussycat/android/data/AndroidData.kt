package net.milosvasic.pussycat.android.data

import com.android.ddmlib.Log
import com.android.ddmlib.logcat.LogCatMessage
import net.milosvasic.pussycat.android.data.parser.LogCatMessageParser
import net.milosvasic.pussycat.core.common.DataFilter
import net.milosvasic.pussycat.core.data.StringData
import net.milosvasic.pussycat.logging.LOG_TYPE
import net.milosvasic.pussycat.utils.Text
import java.util.concurrent.CopyOnWriteArrayList

class AndroidData(filter: DataFilter<CopyOnWriteArrayList<AndroidLogCatMessage>, String>) : StringData<AndroidLogCatMessage>(filter) {

    private val parser = LogCatMessageParser()
    private var logLevel: Log.LogLevel? = null

    fun setLogLevel(level: Log.LogLevel) {
        logLevel = level
        apply(pattern)
    }

    fun clearLogLevel() {
        logLevel = null
        apply(pattern)
    }

    fun getLogLevel(): Log.LogLevel? {
        return logLevel
    }

    override fun addData(message: AndroidLogCatMessage) {
        if (!data.isEmpty()) {
            if (Text.isEmpty(message.msg)) {
                return
            }
            val identifier = getIdentifier(message)
            val existing = data.last();
            val existingIdentifier = getIdentifier(existing)
            if (existing != null && identifier == existingIdentifier) {
                existing.appendToStacktrace(message.msg)
            } else {
                data.add(message)
            }
        } else {
            data.add(message)
        }
    }

    fun addData(messages: Collection<LogCatMessage>) {
        for (message in messages) {
            addData(AndroidLogCatMessage.getFrom(message))
        }
    }

    fun addData(lines: Array<String>) {
        val messages = parser.processLogLines(lines)
        for (message in messages) {
            addData(message)
        }
    }

    override fun getIdentifier(message: AndroidLogCatMessage): String {
        return LogCatMessageParser.getIdentifier(message)
    }

    override fun getTag(message: AndroidLogCatMessage): LOG_TYPE? {
        return when (message.logLevel) {
            Log.LogLevel.DEBUG -> LOG_TYPE.DEBUG
            Log.LogLevel.INFO -> LOG_TYPE.INFORMATION
            Log.LogLevel.WARN -> LOG_TYPE.WARNING
            Log.LogLevel.ERROR -> LOG_TYPE.ERROR
            else -> LOG_TYPE.VERBOSE
        }
    }

    override fun AndroidLogCatMessage.containsIgnoreCase(word: String): Boolean {
        return msg.containsIgnoreCase(word) || appName.containsIgnoreCase(word) || tag.containsIgnoreCase(word)
    }

}