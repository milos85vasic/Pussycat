package net.milosvasic.pussycat.terminal

import net.milosvasic.pussycat.Pussycat
import net.milosvasic.pussycat.core.commands.COMMAND
import java.util.concurrent.atomic.AtomicBoolean

/**
 * Main application entry point
 */
fun main(args: Array<String>) {
    val pussy = Pussycat()
    val run = AtomicBoolean(true)

    Runtime.getRuntime().addShutdownHook(Thread(Runnable {
        run.set(false)
        System.exit(0)
    }))

    Thread(Runnable {
        Thread.currentThread().name = "Commands thread"
        while (run.get()) {
            val line = readLine()
            if (line != null && !line.isEmpty()) {
                try {
                    pussy.execute(COMMAND.valueOf(line))
                } catch (e: IllegalArgumentException) {
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

