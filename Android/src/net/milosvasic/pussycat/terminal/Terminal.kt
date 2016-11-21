package net.milosvasic.pussycat.terminal

import net.milosvasic.pussycat.core.COMMAND
import java.util.concurrent.atomic.AtomicBoolean

/**
 * Main application entry point
 */
fun main(args: Array<String>) {

    val run = AtomicBoolean(false)
    val pussy = TerminalPussycat()

    Runtime.getRuntime().addShutdownHook(Thread(Runnable {
        run.set(false)
        System.exit(0)
    }))

    Thread(Runnable {
        Thread.currentThread().name = "Commands thread"
        run.set(true)
        while (run.get()) {
            var line = readLine()
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
    }).start()

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

