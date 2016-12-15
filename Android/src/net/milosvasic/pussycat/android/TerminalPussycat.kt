package net.milosvasic.pussycat.android

import com.android.ddmlib.Log
import net.milosvasic.pussycat.android.command.ANDROID_COMMAND
import net.milosvasic.pussycat.android.data.AndroidLogCatMessage
import net.milosvasic.pussycat.color.Color
import net.milosvasic.pussycat.events.EVENT
import net.milosvasic.pussycat.events.Events
import net.milosvasic.pussycat.utils.Text
import java.util.concurrent.atomic.AtomicBoolean

class TerminalPussycat : AndroidPussycat() {

    val run = AtomicBoolean(false)

    override fun start(args: Array<String>) {
        val shutdown = fun() {
            printLine("We are shutting down Pussycat.")
            run.set(false)
            System.`in`.close()
            printLine("Bye, bye!")
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
                        var cmdParam: String
                        val cmdParams = mutableListOf<String>()
                        val rawParam = line.substring(2, line.lastIndex + 1).trim()
                        if (rawParam.contains(" ")) {
                            val params = rawParam.split(" ")
                            cmdParam = params[0].toUpperCase()
                            cmdParams.add(params[1])
                        } else {
                            cmdParam = rawParam.toUpperCase()
                        }
                        val cmd = ANDROID_COMMAND.get(cmdParam)
                        if (cmd != ANDROID_COMMAND.PARENT.UNKNOWN) {
                            execute(cmd, Array(cmdParams.size, { i -> cmdParams[i] }))
                        } else {
                            filter(line)
                        }
                    } else {
                        filter(line)
                    }
                } else {
                    execute(ANDROID_COMMAND.PARENT.STATUS)
                }
            }
        })

        val hook = Thread(Runnable {
            execute(ANDROID_COMMAND.PARENT.STOP)
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
                printLine("Error parsing arguments: $arg\n" + e.message)
                adb = false
                execute(ANDROID_COMMAND.PARENT.STOP)
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
            execute(ANDROID_COMMAND.PARENT.LIVE)
        } else {
            execute(ANDROID_COMMAND.PARENT.FILESYSTEM, arrayOf(file))
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
        printLine(27.toChar() + "[2J")
    }

    override fun printLine(text: String) {
        println(text)
    }

    override fun printLine(line: AndroidLogCatMessage) {
        var appName = line.appName
        if (Text.isEmpty(appName)) {
            appName = "-"
        }
        val message = "${line.time} [ pid: ${line.pid} tid: ${line.tid} ][ $appName ][ ${line.tag} ]: ${line.msg}"
        printLine(message, line.logLevel)
    }

    override fun printLine(text: String, logLevel: Log.LogLevel) {
        if (paused.get()) {
            return
        }
        when (logLevel) {
            Log.LogLevel.VERBOSE -> {
                color = Color.WHITE
                printLine("$color$text${Color.RESET}")
                return
            }
            Log.LogLevel.DEBUG -> {
                color = Color.YELLOW
                printLine("$color$text${Color.RESET}")
                return
            }
            Log.LogLevel.INFO -> {
                color = Color.CYAN
                printLine("$color$text${Color.RESET}")
                return
            }
            Log.LogLevel.WARN -> {
                color = Color.PURPLE
                printLine("$color$text${Color.RESET}")
                return
            }
            Log.LogLevel.ERROR -> {
                color = Color.RED
                printLine("$color$text${Color.RESET}")
                return
            }
            else -> printLine("\t$color$text${Color.RESET}")
        }
    }

    override fun status() {
        printLine("Pussycat [ filter: ${getPrintableFilterValue()} ][ log level: ${getPrintableLogLevelValue()} ]")
    }

    override fun getPrintableLogLevelValue(): String {
        val logLevel = data.getLogLevel()
        if (logLevel != null) {
            return logLevel.stringValue.toUpperCase()
        }
        return "NOT SET"
    }

    override fun getPrintableFilterValue(): String {
        val pattern = data.getFilterPattern()
        if (!Text.isEmpty(pattern)) {
            return pattern
        }
        return "NOT SET"
    }

}