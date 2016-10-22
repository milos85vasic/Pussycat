package net.milosvasic.pussycat

import net.milosvasic.pussycat.core.COMMAND
import net.milosvasic.pussycat.core.Pussycat
import java.io.File

val pussy: Pussycat = Pussycat()
var run = true

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
                    println("Logcat: $arg does not exist")
                    terminate(1)
                }
            }
        }
    }

    while (run) {
        val line = readLine()
        if (line != null && !line.isEmpty()) {
            when (line) {
                COMMAND.CLEAR.value -> Runtime.getRuntime().exec("cls")
                COMMAND.RESET.value -> pussy.filter()
                COMMAND.STOP.value -> terminate()
                else -> pussy.filter(line)
            }
        } else {
            pussy.filter()
        }
    }
}

fun terminate() {
    terminate(0)
}

fun terminate(status: Int) {
    println("Bye, bye!")
    pussy.stop()
    System.exit(status)
}