package net.milosvasic.pussycat.android.application

import net.milosvasic.pussycat.android.AndroidPussycat
import net.milosvasic.pussycat.android.TerminalPussycat
import net.milosvasic.pussycat.application.APPLICATION_TYPE
import net.milosvasic.pussycat.application.ApplicationAbstract
import net.milosvasic.pussycat.core.COMMAND

open class Application(args: Array<String>) : ApplicationAbstract(args) {

    var type: APPLICATION_TYPE? = null
    var pussy: AndroidPussycat? = null

    init {
        type = APPLICATION_TYPE.GUI
        for (arg in args) {
            if (arg.trim() == "--terminal") {
                type = APPLICATION_TYPE.CLI
            }
        }
        if (type == APPLICATION_TYPE.CLI) {
            pussy = TerminalPussycat()
        } else {
            // TODO: Create gui instance.
        }
    }

    override fun start() {
        pussy?.start(args)
    }

    override fun stop() {
        pussy?.execute(COMMAND.STOP)
    }

}