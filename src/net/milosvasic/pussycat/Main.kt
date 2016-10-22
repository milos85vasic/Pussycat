package net.milosvasic.pussycat

import net.milosvasic.pussycat.color.Color
import net.milosvasic.pussycat.core.COMMAND
import net.milosvasic.pussycat.core.Pussycat
import net.milosvasic.pussycat.logging.ConsoleLogger
import java.io.File

var run = true
val TAG = Pussycat::class
val logger = ConsoleLogger()
val pussy: Pussycat = Pussycat()

/**
 * Main application entry point
 */
fun main(args: Array<String>) {
    Runtime.getRuntime().addShutdownHook(Thread(Runnable { terminate() }))
    if (args.isEmpty()) {
        pussy.live()
    } else {
        for (arg in args) {
            if (arg.trim() == "--adb") {
                pussy.live()
            } else {
                val logcat = File(arg)
                if (logcat.exists()) {
                    pussy.filesystem(logcat)
                } else {
                    logger.e(TAG, "Logcat: $arg does not exist")
                    terminate(1)
                }
            }
        }
    }

    Thread(Runnable {
        while (run) {
            val line = readLine()
            if (line != null && !line.isEmpty()) {
                when (line) {
                    COMMAND.EXIT.value -> finish()
                    COMMAND.CLEAR.value -> pussy.clear()
                    COMMAND.RESET.value -> pussy.filter()
                    COMMAND.PAUSE.value -> pussy.pause()
                    COMMAND.RESUME.value -> pussy.resume()
                    else -> pussy.filter(line)
                }
            } else {
                println("--- Pussycat, filter [ ${pussy.printFilter()} ] ---\n\n")
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