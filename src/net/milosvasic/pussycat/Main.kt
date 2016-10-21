package net.milosvasic.pussycat

/**
 * Main application entry point
 */
fun main(args: Array<String>) {
    Runtime.getRuntime().addShutdownHook(Thread(Runnable { terminate() }))
}

fun terminate() {
    print("Bye, bye!")
    System.exit(0)
}