package net.milosvasic.pussycat.gui


import com.apple.eawt.Application
import net.milosvasic.pussycat.application.ApplicationInformation
import net.milosvasic.pussycat.gui.menu.PussycatMainMenu
import net.milosvasic.pussycat.gui.themes.Theme
import net.milosvasic.pussycat.os.OS

class PussycatMainWindow(theme: Theme, val information: ApplicationInformation) : PussycatWindow(theme) {

    val mainMenu = PussycatMainMenu(theme)

    init {
        title = "${information.name} V${information.version} by ${information.author}"
    }

    override fun initialize() {
        super.initialize()
        val osString = OS.getOS()
        if (osString.contains(OS.MACOS)) { // TODO: Remove negation for mac os
            System.setProperty("com.apple.mrj.application.apple.menu.about.name", information.name)
            System.setProperty("com.apple.macos.useScreenMenuBar", "true")
            System.setProperty("apple.laf.useScreenMenuBar", "true")
            val app = Application.getApplication()
            app.setAboutHandler {
                val aboutDialog  = PussycatAboutDialog(information, this)
                aboutDialog.open()
            }
            app.setDefaultMenuBar(mainMenu)
        } else {
            jMenuBar = mainMenu
        }
    }

}