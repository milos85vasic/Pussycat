package net.milosvasic.pussycat.android.data

import com.android.ddmlib.Log
import com.android.ddmlib.logcat.LogCatMessage
import net.milosvasic.pussycat.core.common.DataFilter
import net.milosvasic.pussycat.core.data.StringData
import net.milosvasic.pussycat.logging.LOG_TYPE
import java.util.*

class AndroidData(filter: DataFilter<LinkedHashMap<String, LogCatMessage>, String>) : StringData<LogCatMessage>(filter) {

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

    override fun addData(message: LogCatMessage) {
        val identifier = getIdentifier(message)
        val existing = data[identifier]
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
            data[identifier] = newMessage
        } else {
            data.put(identifier, message)
        }
    }

    fun addData(messages: Collection<LogCatMessage>) {
        for (message in messages) {
            addData(message)
        }
    }

    fun addData(lines: Array<String>) {
        val parser = LogCatMessageParser()
        val messages = parser.processLogLines(lines)
        for (message in messages) {
            addData(message)
        }
    }

    override fun getIdentifier(message: LogCatMessage): String {
        return "${message.time}_${message.pid}_${message.tid}"
    }

    override fun getTag(message: LogCatMessage): LOG_TYPE? {
        return when (message.logLevel) {
            Log.LogLevel.DEBUG -> LOG_TYPE.DEBUG
            Log.LogLevel.INFO -> LOG_TYPE.INFORMATION
            Log.LogLevel.WARN -> LOG_TYPE.WARNING
            Log.LogLevel.ERROR -> LOG_TYPE.ERROR
            else -> LOG_TYPE.VERBOSE
        }
    }

    override fun LogCatMessage.containsIgnoreCase(word: String): Boolean {
        return message.containsIgnoreCase(word) || appName.containsIgnoreCase(word) || tag.containsIgnoreCase(word)
    }

}