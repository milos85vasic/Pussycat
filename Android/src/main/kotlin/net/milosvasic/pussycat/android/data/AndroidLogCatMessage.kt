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
            if (pidLen >= LENGTHS.SPACING_DEFAULT) {
                LENGTHS.PID = LENGTHS.SPACING_DEFAULT
            } else {
                LENGTHS.PID = pidLen
            }
        }

        val tidLen = "$tid".length
        if (tidLen > LENGTHS.TID) {
            if (tidLen >= LENGTHS.SPACING_DEFAULT) {
                LENGTHS.TID = LENGTHS.SPACING_DEFAULT
            } else {
                LENGTHS.TID = tidLen
            }
        }

        if (appName.length > LENGTHS.APP_NAME) {
            if (appName.length >= LENGTHS.SPACING_LONG) {
                LENGTHS.APP_NAME = LENGTHS.SPACING_LONG
            } else {
                LENGTHS.APP_NAME = appName.length
            }
        }

        if (tag.length > LENGTHS.TAG) {
            if (tag.length >= LENGTHS.SPACING_LONG) {
                LENGTHS.TAG = LENGTHS.SPACING_LONG
            } else {
                LENGTHS.TAG = tag.length
            }
        }

        if (time.length > LENGTHS.TIME) {
            if (time.length >= LENGTHS.SPACING_LONG) {
                LENGTHS.TIME = LENGTHS.SPACING_LONG
            } else {
                LENGTHS.TIME = time.length
            }
        }
    }

    override fun toString(): String {
        return "$time: ${logLevel.priorityLetter}/$logLevel($pid): $msg"
    }

    class Lengths {
        val SPACING_DEFAULT = 20
        val SPACING_LONG = 30

        var PID = 0
        var TID = 0
        var APP_NAME = 0
        var TAG = 0
        var TIME = 0
    }

}