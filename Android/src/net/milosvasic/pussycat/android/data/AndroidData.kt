package net.milosvasic.pussycat.android.data

import com.android.ddmlib.Log
import com.android.ddmlib.logcat.LogCatMessage
import com.android.ddmlib.logcat.LogCatMessageParser
import net.milosvasic.pussycat.core.common.DataFilter
import net.milosvasic.pussycat.core.data.StringData
import net.milosvasic.pussycat.logging.LOG_LEVEL
import java.util.concurrent.CopyOnWriteArrayList

class AndroidData(filter: DataFilter<CopyOnWriteArrayList<LogCatMessage>, String>) : StringData<LogCatMessage>(filter) {

    fun addData(lines: Array<String>) {
        val parser = LogCatMessageParser()
        val messages = parser.processLogLines(lines, null)
        data.addAll(messages)
    }

    fun addData(messages: Collection<LogCatMessage>) {
        data.addAll(messages)
    }

    override fun getTag(message: LogCatMessage): LOG_LEVEL? {
        return when (message.logLevel) {
            Log.LogLevel.DEBUG -> LOG_LEVEL.DEBUG
            Log.LogLevel.INFO -> LOG_LEVEL.INFORMATION
            Log.LogLevel.WARN -> LOG_LEVEL.WARNING
            Log.LogLevel.ERROR -> LOG_LEVEL.ERROR
            else -> LOG_LEVEL.VERBOSE
        }
    }

    override fun LogCatMessage.containsIgnoreCase(word: String): Boolean {
        return message.containsIgnoreCase(word)
    }
}