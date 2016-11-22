package net.milosvasic.pussycat.terminal

import net.milosvasic.pussycat.core.COMMAND
import net.milosvasic.pussycat.events.EVENT
import java.util.concurrent.atomic.AtomicBoolean
import net.milosvasic.pussycat.events.Events

/**
 * Main application entry point
 */
fun main(args: Array<String>) {

    val run = AtomicBoolean(false)
    val pussy = TerminalPussycat()

    val listener = object : Events {
        override fun onEvent(event: EVENT) {
            when (event) {
                EVENT.STOP -> println("STOPPING")
            }
        }
    }

    pussy.subscribe(listener)

    val commands = Thread(Runnable {
        Thread.currentThread().name = "Commands thread"
        run.set(true)
        while (run.get()) {
            val line = readLine()
            if (line != null && !line.isEmpty()) {
                if (line.startsWith("@@")) {
                    val cmdParam = line.substring(2, line.lastIndex + 1).toUpperCase().trim()
                    try {
                        pussy.execute(COMMAND.valueOf(cmdParam))
                    } catch (e: IllegalArgumentException) {
                        pussy.filter(line)
                    }
                } else {
                    pussy.filter(line)
                }
            } else {
                pussy.execute(COMMAND.STATUS)
            }
        }
    })

    val hook = Thread(Runnable {
        println("We are shutting down Pussycat.")
        pussy.execute(COMMAND.STOP)
        run.set(false)
        System.`in`.close()
        println("Bye, bye!")
        System.exit(0)
    })

    Runtime.getRuntime().addShutdownHook(hook)
    commands.start()

    if (args.isEmpty()) {
        pussy.execute(COMMAND.LIVE)
    } else {
        for (arg in args) {
            if (arg.trim() == "--adb") {
                pussy.execute(COMMAND.LIVE)
            } else {
                pussy.execute(COMMAND.FILESYSTEM, arg)
            }
        }
    }
}

