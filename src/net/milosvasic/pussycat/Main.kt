package net.milosvasic.pussycat

/**
 * Main application entry point
 */
fun main(args: Array<String>) {
    Runtime.getRuntime().addShutdownHook(Thread(Runnable { terminate() }))
    for (arg in args) {
        print("ARG ${arg}")
        if (arg.trim() == "--adb") {

        } else {

        }
    }
}

fun terminate() {
    print("Bye, bye!")
    System.exit(0)
}