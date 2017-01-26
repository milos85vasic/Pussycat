package net.milosvasic.pussycat.android.data

import com.android.ddmlib.Log
import com.android.ddmlib.logcat.LogCatMessage
import java.util.*

class AndroidLogCatMessage(
        val logLevel: Log.LogLevel,
        val pid: Int,
        val tid: Int,
        val appName: String,
        val tag: String,
        val time: String,
        val msg: String
) {

    private var stacktrace: MutableList<String>? = null

    companion object {
        val LENGTHS = Lengths

        fun getFrom(message: LogCatMessage): AndroidLogCatMessage {
            return AndroidLogCatMessage(
                    message.logLevel,
                    message.pid,
                    message.tid,
                    message.appName,
                    message.tag,
                    message.timestamp.toString(),
                    message.message
            )
        }
    }

    fun appendToStacktrace(stacktraceItem: String) {
        if (stacktrace == null) {
            stacktrace = mutableListOf<String>()
        }
        stacktrace?.add(stacktraceItem)
    }

    override fun toString(): String {
        return "$time: ${logLevel.priorityLetter}/$logLevel($pid): $msg"
    }

    object Lengths {
        val SPACING_DEFAULT = 15
        val SPACING_SHORT = 10
        val SPACING_LONG = 20
    }

}