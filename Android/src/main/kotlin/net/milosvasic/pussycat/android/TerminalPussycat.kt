package net.milosvasic.pussycat.android

import com.android.ddmlib.Log
import net.milosvasic.pussycat.Messages
import net.milosvasic.pussycat.android.command.ANDROID_COMMAND
import net.milosvasic.pussycat.android.data.AndroidLogCatMessage
import net.milosvasic.pussycat.color.Color
import net.milosvasic.pussycat.events.EVENT
import net.milosvasic.pussycat.events.Events
import net.milosvasic.pussycat.terminal.TerminalPrinter
import net.milosvasic.pussycat.utils.Text
import java.io.BufferedWriter
import java.io.FileDescriptor
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.AtomicBoolean

class TerminalPussycat : AndroidPussycat() {

    val run = AtomicBoolean(false)

    companion object {
        val TERMINAL_CLEAR = 27.toChar() + "[2J"
    }

    init {
        configuration.setExitRoutine(Runnable {
            printLine(Messages.PUSSYCAT_SHUTDOWN)
            run.set(false)
            System.`in`.close()
            printLine(Messages.BYE_BYE)
        })
        configuration.terminalPriner = TerminalPrinter()
    }

    override fun start(args: Array<String>) {
        val listener = object : Events {
            override fun onEvent(event: EVENT) {
                if (event == EVENT.STOP) {
                    unsubscribe(this)
                    configuration.getExitRoutine().run()
                }
            }
        }

        val commands = Thread(Runnable {
            Thread.currentThread().name = "Commands thread"
            run.set(true)
            while (run.get()) {
                var line: String? = null
                try {
                    line = readLine()
                } catch (e: Exception) {
                    printLine("Pussycat, ${Messages.ERROR_READING_LINE}: $e")
                }
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

    override fun clear() {
        printLine(TERMINAL_CLEAR)
    }

    override fun printLine(text: String) {
        configuration.terminalPriner?.printLine(text)
    }

    override fun printLine(line: AndroidLogCatMessage) {
        var appName = line.appName
        if (!Text.isEmpty(appName)) {
            appName = "[ $appName ]"
        }
        var time = line.time
        if (!Text.isEmpty(time)) {
            time = "$time "
        }
        var pid = ""
        if (line.pid > 0) {
            pid = "[ pid: ${line.pid} ]"
        }
        var tid = ""
        if (line.tid > 0) {
            tid = "[ tid: ${line.tid} ]"
        }
        var tag = ""
        if (!Text.isEmpty(line.tag)) {
            tag = "[ ${line.tag} ]"
        }
        val message = "$time$pid$tid$appName$tag: ${line.msg}"
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
        return Messages.NOT_SET
    }

    override fun getPrintableFilterValue(): String {
        val pattern = data.getFilterPattern()
        if (!Text.isEmpty(pattern)) {
            return pattern
        }
        return Messages.NOT_SET
    }

    override fun executeFilesystemRunnable(runnable: Runnable) {
        Thread(runnable).start()
    }

}