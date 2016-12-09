package net.milosvasic.pussycat.android

import com.android.ddmlib.Log
import com.android.ddmlib.logcat.LogCatMessage
import net.milosvasic.pussycat.color.Color
import net.milosvasic.pussycat.core.COMMAND
import net.milosvasic.pussycat.events.EVENT
import net.milosvasic.pussycat.events.Events
import net.milosvasic.pussycat.utils.Text
import java.util.concurrent.atomic.AtomicBoolean

class TerminalPussycat : AndroidPussycat() {

    val run = AtomicBoolean(false)

    override fun start(args: Array<String>) {
        val shutdown = fun() {
            println("We are shutting down Pussycat.")
            run.set(false)
            System.`in`.close()
            println("Bye, bye!")
            System.exit(0)
        }

        val listener = object : Events {
            override fun onEvent(event: EVENT) {
                if (event == EVENT.STOP) {
                    unsubscribe(this)
                    shutdown.invoke()
                }
            }
        }

        val commands = Thread(Runnable {
            Thread.currentThread().name = "Commands thread"
            run.set(true)
            while (run.get()) {
                val line = readLine()
                if (line != null && !line.isEmpty()) {
                    if (line.startsWith("@@")) {
                        var cmdParam = ""
                        val cmdParams = mutableListOf<String>()
                        val rawParam = line.substring(2, line.lastIndex + 1).toUpperCase().trim()
                        if (rawParam.contains(" ")) {
                            val params = rawParam.split(" ")
                            cmdParam = params[0]
                            cmdParams.add(params[1])
                        } else {
                            cmdParam = rawParam
                        }
                        try {
                            execute(COMMAND.valueOf(cmdParam), Array(cmdParams.size, { i -> cmdParams[i] }))
                        } catch (e: IllegalArgumentException) {
                            filter(line)
                        }
                    } else {
                        filter(line)
                    }
                } else {
                    execute(COMMAND.STATUS)
                }
            }
        })

        val hook = Thread(Runnable {
            execute(COMMAND.STOP)
        })

        Runtime.getRuntime().addShutdownHook(hook)
        subscribe(listener)
        commands.start()

        var adb = true
        var file: String = ""
        for (arg in args) {
            var parsedArg: String
            try {
                parsedArg = getArgumentOption(arg.trim())
            } catch (e: Exception) {
                println("Error parsing arguments: $arg\n" + e.message)
                adb = false
                execute(COMMAND.STOP)
                break
            }
            when (parsedArg) {
                "--adb" -> adb = true
                "--filesystem" -> {
                    adb = false
                    file = getArgumentValue(arg)
                }
                else -> {
                }
            }
        }
        if (adb) {
            execute(COMMAND.LIVE)
        } else {
            execute(COMMAND.FILESYSTEM, arrayOf(file))
        }
    }

    fun getArgumentOption(arg: String): String {
        var occurances = 0
        for (char in arg) {
            if (char == '=') {
                occurances++
            }
        }
        if (occurances > 1) {
            throw IllegalArgumentException("'=' is contained on more than 1 places.")
        }
        if (occurances == 1) {
            return arg.substring(0, arg.indexOf('='))
        }
        return arg
    }

    fun getArgumentValue(arg: String): String {
        if (arg.contains('=')) {
            return arg.substring(arg.indexOf('=') + 1)
        }
        return arg
    }

    override fun clear() {
        println(27.toChar() + "[2J")
    }

    override fun printLine(text: String) {
        println(text)
    }

    override fun printLine(line: LogCatMessage) {
        var appName = line.appName
        if (Text.isEmpty(appName)) {
            appName = "---"
        }
        val message = "${line.time} [ pid: ${line.pid} tid: ${line.tid} ][ $appName ][ ${line.tag} ]: ${line.message}"
        if (paused.get()) {
            return
        }
        when (line.logLevel) {
            Log.LogLevel.VERBOSE -> {
                color = Color.WHITE
                println("$color$message${Color.RESET}")
                return
            }
            Log.LogLevel.DEBUG -> {
                color = Color.YELLOW
                println("$color$message${Color.RESET}")
                return
            }
            Log.LogLevel.INFO -> {
                color = Color.CYAN
                println("$color$message${Color.RESET}")
                return
            }
            Log.LogLevel.WARN -> {
                color = Color.PURPLE
                println("$color$message${Color.RESET}")
                return
            }
            Log.LogLevel.ERROR -> {
                color = Color.RED
                println("$color$message${Color.RESET}")
                return
            }
            else -> println("\t$color$message${Color.RESET}")
        }

    }

    override fun status() {
        logger.v(TAG, "Filter applied [ ${getFilter()} ]\n\n")
    }

}