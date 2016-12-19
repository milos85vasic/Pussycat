package net.milosvasic.pussycat.android.data

import com.android.ddmlib.Log
import com.android.ddmlib.logcat.LogCatMessage

class AndroidLogCatMessage(
        val logLevel: Log.LogLevel,
        val pid: Int,
        val tid: Int,
        val appName: String,
        val tag: String,
        val time: String,
        val msg: String
) {

    companion object {
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

    override fun toString(): String {
        return "$time: ${logLevel.priorityLetter}/$logLevel($pid): $msg"
    }

}