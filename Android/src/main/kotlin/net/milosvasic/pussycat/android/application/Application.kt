package net.milosvasic.pussycat.android.application

import net.milosvasic.pussycat.android.AndroidPussycat
import net.milosvasic.pussycat.android.TerminalPussycat
import net.milosvasic.pussycat.application.APPLICATION_TYPE
import net.milosvasic.pussycat.core.COMMAND

class Application() {

    var pussy: AndroidPussycat? = null

    fun start(args: Array<String>): APPLICATION_TYPE {
        var terminal = APPLICATION_TYPE.GUI
        for (arg in args) {
            if (arg.trim() == "--terminal") {
                terminal = APPLICATION_TYPE.CLI
            }
        }

        if (terminal == APPLICATION_TYPE.CLI) {
            pussy = TerminalPussycat()
            pussy?.start(args)
        } else {
            // TODO: Start GUI
        }
        return terminal
    }

    fun stop() {
        pussy?.execute(COMMAND.STOP)
    }

}