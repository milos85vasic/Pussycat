package net.milosvasic.pussycat.android.application

import net.milosvasic.pussycat.android.AndroidPussycat
import net.milosvasic.pussycat.android.GuiPussycat
import net.milosvasic.pussycat.android.TerminalPussycat
import net.milosvasic.pussycat.application.APPLICATION_TYPE
import net.milosvasic.pussycat.application.ApplicationAbstract
import net.milosvasic.pussycat.application.ApplicationInformation
import net.milosvasic.pussycat.core.COMMAND
import net.milosvasic.pussycat.gui.theme.Darcula
import net.milosvasic.pussycat.gui.theme.ThemeManager

class Application(args: Array<String>) : ApplicationAbstract(args) {

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
            pussy = GuiPussycat(getApplicationInformation())
        }
    }

    override fun start() {
        pussy?.start(args)
    }

    override fun stop() {
        pussy?.execute(COMMAND.STOP)
    }

    override fun getApplicationInformation(): ApplicationInformation {
        return ApplicationInformation(
                "1.0.0",
                "Pussycat for Android",
                "http://pussycat.milosvasic.net",
                "Miloš Vasić"
        )
    }

}