package net.milosvasic.pussycat.android.data

import com.android.ddmlib.Log
import com.android.ddmlib.logcat.LogCatMessage
import com.android.ddmlib.logcat.LogCatMessageParser
import net.milosvasic.pussycat.core.common.DataFilter
import net.milosvasic.pussycat.core.data.Data
import net.milosvasic.pussycat.core.data.OPERATOR
import net.milosvasic.pussycat.logging.LOG_LEVEL
import java.util.concurrent.CopyOnWriteArrayList

class AndroidData(filter: DataFilter<CopyOnWriteArrayList<LogCatMessage>, String>) : Data<LogCatMessage>(filter) {

    override fun addData(message: LogCatMessage) {
        data.add(message)
    }

    fun addData(lines: Array<String>) {
        val parser = LogCatMessageParser()
        val messages = parser.processLogLines(lines, null)
        data.addAll(messages)
    }

    fun addData(messages: Collection<LogCatMessage>) {
        data.addAll(messages)
    }

    override fun evaluate(message: LogCatMessage): Boolean {
        if (pattern.isEmpty()) {
            return true
        }
        if (evaluable(pattern)) {
            if (!evaluable(pattern, OPERATOR.OR)) {
                return evaluateAnd(line, pattern)
            } else {
                val elements = pattern.split(OPERATOR.OR.value)
                for (element in elements) {
                    val trimmed = element.trim()
                    if (evaluable(trimmed, OPERATOR.AND)) {
                        if (evaluateAnd(line, trimmed)) {
                            return true
                        }
                    } else {
                        if (evaluateOr(line, trimmed)) {
                            return true
                        }
                    }
                }
                return false
            }
        } else {
            if (pattern.startsWith(OPERATOR.NOT.value)) {
                val check = pattern.replace(OPERATOR.NOT.value, "")
                if (line.containsIgnoreCase(check)) {
                    return false
                }
            } else {
                if (!line.containsIgnoreCase(pattern)) {
                    return false
                }
            }
            return true
        }
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

}