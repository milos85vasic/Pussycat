package net.milosvasic.pussycat.android

fun main(args: Array<String>) {

    for (arg in args) {
        if (arg.trim() == "--terminal") {
            val pussy = TerminalPussycat()
            pussy.start(args)
        } else {
            println("We are about to start GUI.")
            // TODO: Start GUI
        }
    }

}

