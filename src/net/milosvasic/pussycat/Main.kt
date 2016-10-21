package net.milosvasic.pussycat

import net.milosvasic.pussycat.core.Pussycat
import java.io.File

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
                    println("Logcat: $arg does not exist")
                    System.exit(1)
                }
            }
        }
    }
}

fun terminate() {
    println("Bye, bye!")
    System.exit(0)
}