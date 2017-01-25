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
            if (pidLen >= 12) {
                LENGTHS.PID = 12
            } else {
                LENGTHS.PID = pidLen
            }
        }

        val tidLen = "$tid".length
        if (tidLen > LENGTHS.TID) {
            if (tidLen >= 12) {
                LENGTHS.TID = 12
            } else {
                LENGTHS.TID = tidLen
            }
        }

        if (appName.length > LENGTHS.APP_NAME) {
            if (appName.length >= 15) {
                LENGTHS.APP_NAME = 15
            } else {
                LENGTHS.APP_NAME = appName.length
            }
        }

        if (tag.length > LENGTHS.TAG) {
            if (tag.length >= 15) {
                LENGTHS.TAG = 15
            } else {
                LENGTHS.TAG = tag.length
            }
        }

        if (time.length > LENGTHS.TIME) {
            if (time.length >= 15) {
                LENGTHS.TIME = 15
            } else {
                LENGTHS.TIME = time.length
            }
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