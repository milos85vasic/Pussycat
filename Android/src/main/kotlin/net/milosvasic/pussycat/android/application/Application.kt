package net.milosvasic.pussycat.android.application

import net.milosvasic.pussycat.android.AndroidPussycat
import net.milosvasic.pussycat.android.TerminalPussycat
import net.milosvasic.pussycat.application.APPLICATION_TYPE
import net.milosvasic.pussycat.application.ApplicationAbstract
import net.milosvasic.pussycat.core.COMMAND

open class Application() : ApplicationAbstract() {

    var pussy: AndroidPussycat? = null

    override fun start(args: Array<String>): APPLICATION_TYPE {
        var type = APPLICATION_TYPE.GUI
        for (arg in args) {
            if (arg.trim() == "--terminal") {
                type = APPLICATION_TYPE.CLI
            }
        }

        if (type == APPLICATION_TYPE.CLI) {
            pussy = TerminalPussycat()
            pussy?.start(args)
        } else {
            // TODO: Start GUI
        }
        return type
    }

    override fun stop() {
        pussy?.execute(COMMAND.STOP)
    }

}