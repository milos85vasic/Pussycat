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
        val LENGTHS = Lengths()

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

    init {
        val pidLen = "$pid".length
        if (pidLen > LENGTHS.PID) {
            LENGTHS.PID = pidLen
        }

        val tidLen = "$tid".length
        if (tidLen > LENGTHS.TID) {
            LENGTHS.TID = tidLen
        }

        if (appName.length > LENGTHS.APP_NAME) {
            LENGTHS.APP_NAME = appName.length
        }

        if (tag.length > LENGTHS.TAG) {
            LENGTHS.TAG = tag.length
        }

        if (time.length > LENGTHS.TIME) {
            LENGTHS.TIME = time.length
        }
    }

    override fun toString(): String {
        return "$time: ${logLevel.priorityLetter}/$logLevel($pid): $msg"
    }

    class Lengths {
        var PID = 0
        var TID = 0
        var APP_NAME = 0
        var TAG = 0
        var TIME = 0
    }

}