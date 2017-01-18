package net.milosvasic.pussycat.gui


import com.apple.eawt.Application
import net.milosvasic.pussycat.application.ApplicationInformation
import net.milosvasic.pussycat.gui.PussycatMainMenuBar
import net.milosvasic.pussycat.gui.PussycatBar
import net.milosvasic.pussycat.gui.themes.Theme
import net.milosvasic.pussycat.os.OS
import java.awt.*

class PussycatMainWindow(theme: Theme, val information: ApplicationInformation) : PussycatWindow(theme) {

    init {
        title = "${information.name} V${information.version} by ${information.author}"
    }

    override fun initialize() {
        super.initialize()
        val screenSize = Toolkit.getDefaultToolkit().screenSize
        val barHeight = (screenSize.height / 100) * 3
        val headerBar = PussycatBar(screenSize.width, barHeight)
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
            val mainMenuPanel = PussycatMainMenuBar(headerBar.width, barHeight)
            headerBar.preferredSize = Dimension(headerBar.width, barHeight * 2)
            headerBar.addChild(mainMenuPanel, BorderLayout.PAGE_START)
        }
        val content = PussycatContent()
        val footerBar = PussycatBar(screenSize.width, barHeight)
        addChild(headerBar, BorderLayout.PAGE_START)
        addChild(content, BorderLayout.CENTER)
        addChild(footerBar, BorderLayout.PAGE_END)
    }

    fun Container.addChild(comp: Component, constraints: Any){
        theme.apply(comp)
        addImpl(comp, constraints, -1)
    }

}