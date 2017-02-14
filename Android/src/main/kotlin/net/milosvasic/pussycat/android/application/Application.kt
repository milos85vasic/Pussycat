package net.milosvasic.pussycat.android.application

import net.milosvasic.pussycat.android.AndroidPussycat
import net.milosvasic.pussycat.android.TerminalPussycat
import net.milosvasic.pussycat.android.WebGuiPussycat
import net.milosvasic.pussycat.application.APPLICATION_TYPE
import net.milosvasic.pussycat.application.ApplicationAbstract
import net.milosvasic.pussycat.application.ApplicationInformation
import net.milosvasic.pussycat.core.COMMAND
import net.milosvasic.pussycat.gui.theme.Darcula
import net.milosvasic.pussycat.os.OS

class Application(args: Array<String>) : ApplicationAbstract(args) {

    var port = 2507
    var type: APPLICATION_TYPE? = null
    var pussy: AndroidPussycat? = null

    init {
        information = ApplicationInformation(
                "1.0.0",
                "Pussycat for Android",
                "http://pussycat.milosvasic.net",
                "Miloš Vasić"
        )
        if (OS.isMacOS()) {
            System.setProperty("apple.awt.application.name", information.name)
        }
        type = APPLICATION_TYPE.GUI
        args.forEach {
            arg ->
            if (arg.trim() == "--terminal") {
                type = APPLICATION_TYPE.CLI
            } else
                if (arg.trim().startsWith("--port=")) {
                    port = arg.replace("--port=", "").toInt()
                }
        }
        if (type == APPLICATION_TYPE.CLI) {
            pussy = TerminalPussycat()
        } else {
            val theme = Darcula()
            pussy = WebGuiPussycat("127.0.0.1", port, information, theme)
        }
    }

    override fun start() {
        super.start()
        pussy?.start(args)
    }

    override fun stop() {
        super.stop()
        pussy?.execute(COMMAND.STOP)
    }

}