package net.milosvasic.pussycat.android.data

import com.android.ddmlib.Log
import com.android.ddmlib.logcat.LogCatMessage
import net.milosvasic.pussycat.core.common.DataFilter
import net.milosvasic.pussycat.core.data.StringData
import net.milosvasic.pussycat.logging.LOG_TYPE
import java.util.*

class AndroidData(filter: DataFilter<MutableMap<String, AndroidLogCatMessage>, String>) : StringData<AndroidLogCatMessage>(filter) {

    @Transient private var logLevel: Log.LogLevel? = null

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
        val identifier = getIdentifier(message)
        val existing = data[identifier]
        if (existing != null) {
            val newMessage = AndroidLogCatMessage(
                    message.logLevel,
                    message.pid,
                    message.tid,
                    message.appName,
                    message.tag,
                    message.time,
                    "${existing.msg}\n\t${message.msg}"
            )
            data[identifier] = newMessage
        } else {
            data.put(identifier, message)
        }
    }

    fun addData(messages: Collection<LogCatMessage>) {
        for (message in messages) {
            addData(AndroidLogCatMessage.getFrom(message))
        }
    }

    fun addData(lines: Array<String>) {
        val parser = LogCatMessageParser()
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