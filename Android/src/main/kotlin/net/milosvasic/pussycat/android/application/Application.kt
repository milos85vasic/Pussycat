package net.milosvasic.pussycat.android.application

import net.milosvasic.pussycat.android.TerminalPussycat
import net.milosvasic.pussycat.application.APPLICATION_TYPE

class Application() {
    fun start(args: Array<String>) : APPLICATION_TYPE {
        var terminal = APPLICATION_TYPE.GUI
        for (arg in args) {
            if (arg.trim() == "--terminal") {
                terminal = APPLICATION_TYPE.CLI
            }
        }

        if (terminal == APPLICATION_TYPE.CLI) {
            val pussy = TerminalPussycat()
            pussy.start(args)
        } else {
            // TODO: Start GUI
        }
        return terminal
    }
}