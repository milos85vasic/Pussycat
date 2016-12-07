package net.milosvasic.pussycat.android

import com.android.ddmlib.Log
import com.android.ddmlib.logcat.LogCatMessage
import net.milosvasic.pussycat.color.Color

class TerminalPussycat : AndroidPussycat() {

    override fun clear() {
        println(27.toChar() + "[2J")
    }

    override fun printLine(line: LogCatMessage) {
        if (paused.get()) {
            return
        }
        when (line.logLevel) {
            Log.LogLevel.VERBOSE -> {
                color = Color.WHITE
                println("$color$line${Color.RESET}")
                return
            }
            Log.LogLevel.DEBUG -> {
                color = Color.YELLOW
                println("$color$line${Color.RESET}")
                return
            }
            Log.LogLevel.INFO -> {
                color = Color.CYAN
                println("$color$line${Color.RESET}")
                return
            }
            Log.LogLevel.WARN -> {
                color = Color.PURPLE
                println("$color$line${Color.RESET}")
                return
            }
            Log.LogLevel.ERROR -> {
                color = Color.RED
                println("$color$line${Color.RESET}")
                return
            }
            else -> println("\t$color$line${Color.RESET}")
        }

    }

    override fun status() {
        logger.v(TAG, "Filter applied [ ${getFilter()} ]\n\n")
    }

}