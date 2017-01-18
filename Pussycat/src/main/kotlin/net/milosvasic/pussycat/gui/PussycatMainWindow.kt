package net.milosvasic.pussycat.gui


import com.apple.eawt.Application
import net.milosvasic.pussycat.application.ApplicationInformation
import net.milosvasic.pussycat.gui.menu.PussycatMainMenu
import net.milosvasic.pussycat.gui.menu.PussycatMenuBar
import net.milosvasic.pussycat.gui.themes.Theme
import net.milosvasic.pussycat.os.OS
import java.awt.BorderLayout
import java.awt.Toolkit

class PussycatMainWindow(theme: Theme, val information: ApplicationInformation) : PussycatWindow(theme) {

    init {
        title = "${information.name} V${information.version} by ${information.author}"
    }

    override fun initialize() {
        super.initialize()
        val screenSize = Toolkit.getDefaultToolkit().screenSize
        val barHeight = (screenSize.height / 100) * 3
        if (!OS.isMacOS()) { // TODO: Remove negation.
//            System.setProperty("com.apple.mrj.application.apple.menu.about.name", information.name)
//            System.setProperty("com.apple.mac.useScreenMenuBar", "true")
//            System.setProperty("apple.laf.useScreenMenuBar", "true")
//            val app = Application.getApplication()
//            app.setAboutHandler {
//                val aboutDialog  = PussycatAboutDialog(information, this)
//                aboutDialog.open()
//            }
//            app.setDefaultMenuBar(mainMenu)
        } else {
            val headerBar = PussycatMenuBar(screenSize.width, barHeight)
            add(headerBar, BorderLayout.PAGE_START)
        }
        val content = PussycatContent()
        val footerBar = PussycatMenuBar(screenSize.width, barHeight)
        add(content, BorderLayout.CENTER)
        add(footerBar, BorderLayout.PAGE_END)
    }

}