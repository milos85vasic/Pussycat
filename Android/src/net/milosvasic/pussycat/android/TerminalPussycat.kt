package net.milosvasic.pussycat.android

import com.android.ddmlib.Log
import com.android.ddmlib.logcat.LogCatMessage
import net.milosvasic.pussycat.color.Color
import net.milosvasic.pussycat.core.COMMAND
import net.milosvasic.pussycat.events.EVENT
import net.milosvasic.pussycat.events.Events
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
                        val cmdParam = line.substring(2, line.lastIndex + 1).toUpperCase().trim()
                        try {
                            execute(COMMAND.valueOf(cmdParam))
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

        if (args.isEmpty()) {
            execute(COMMAND.LIVE)
        } else {
            for (arg in args) {
                if (arg.trim() == "--adb") {
                    execute(COMMAND.LIVE)
                } else {
                    execute(COMMAND.FILESYSTEM, arg)
                }
            }
        }
    }

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