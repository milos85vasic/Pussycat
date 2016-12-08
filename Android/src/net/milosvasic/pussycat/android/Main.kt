package net.milosvasic.pussycat.android

fun main(args: Array<String>) {

    var terminal = false
    for (arg in args) {
        if (arg.trim() == "--terminal") {
            terminal = true
        }
    }

    if (terminal) {
        val pussy = TerminalPussycat()
        pussy.start(args)
    } else {
        println("We are about to start GUI.")
        // TODO: Start GUI
    }

}

