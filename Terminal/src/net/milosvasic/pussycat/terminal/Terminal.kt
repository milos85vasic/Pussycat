package net.milosvasic.pussycat.terminal

import net.milosvasic.pussycat.Pussycat
import net.milosvasic.pussycat.logging.ConsoleLogger
import net.milosvasic.pussycat.core.commands.COMMAND
import java.io.File

val TAG = Pussycat::class
val logger = ConsoleLogger()


/**
 * Main application entry point
 */
fun main(args: Array<String>) {
    Runtime.getRuntime().addShutdownHook(Thread(Runnable { terminate() }))

    val pussy = Pussycat()
    Thread(Runnable {
        Thread.currentThread().name = "Commands thread"
        while (true) {
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

    Thread(Runnable {
        Thread.currentThread().name = "Commands thread"
        while (run) {
            val line = readLine()
            if (line != null && !line.isEmpty()) {
                when (line) {
                    COMMAND.STOP.value -> finish()
                    COMMAND.CLEAR.value -> pussy.clear()
                    COMMAND.RESET.value -> pussy.filter()
                    COMMAND.PAUSE.value -> pussy.pause()
                    COMMAND.RESUME.value -> pussy.resume()
                    else -> pussy.filter(line)
                }
            } else {
                println("--- PussycatLegacy, apply [ ${pussy.getFilter()} ] ---\n\n")
            }
        }
    }).start()
}

fun finish() {
    run = false
    pussy.stop()
}

fun terminate() {
    terminate(0)
}

fun terminate(status: Int) {
    println("${Color.YELLOW}Bye, bye!${Color.RESET}")
    System.exit(status)
}

fun start() {
    run.set(true)

}

fun stop() {
    run.set(false)
}
