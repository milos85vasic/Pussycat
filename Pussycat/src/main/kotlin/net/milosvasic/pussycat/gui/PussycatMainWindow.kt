package net.milosvasic.pussycat.gui


import com.apple.eawt.Application
import net.milosvasic.pussycat.application.ApplicationInformation
import net.milosvasic.pussycat.gui.PussycatBar
import net.milosvasic.pussycat.gui.content.Labels
import net.milosvasic.pussycat.gui.themes.Themable
import net.milosvasic.pussycat.gui.themes.Theme
import net.milosvasic.pussycat.os.OS
import java.awt.*
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder

abstract class PussycatMainWindow(theme: Theme, val information: ApplicationInformation) : PussycatWindow(theme) {

    init {
        title = "${information.name} V${information.version} by ${information.author}"
    }

    override fun initialize() {
        super.initialize()
        val screenSize = Toolkit.getDefaultToolkit().screenSize
        val barHeight = (screenSize.height / 100) * 3
        val headerBar = PussycatBar(screenSize.width, barHeight)
        val mainMenuItems = getMainMenuItems()
        val mainMenuBar = PussycatMainMenuBar(theme, headerBar.width, barHeight, mainMenuItems)
        if (!OS.isMacOS()) { // TODO: Remove negation.
            System.setProperty("com.apple.mrj.application.apple.menu.about.name", information.name)
            System.setProperty("com.apple.mac.useScreenMenuBar", "true")
            System.setProperty("apple.laf.useScreenMenuBar", "true")
            val app = Application.getApplication()
            app.setAboutHandler {
                val aboutDialog  = PussycatAboutDialog(information, this)
                aboutDialog.open()
            }
            app.setDefaultMenuBar(mainMenuBar)
        } else {
            headerBar.preferredSize = Dimension(headerBar.width, barHeight * 2)
            val margin = EmptyBorder(10, 0, 0, 0)
            headerBar.border = CompoundBorder(headerBar.border, margin)
            headerBar.addChild(mainMenuBar, BorderLayout.NORTH)
        }
        val content = PussycatContent()
        val footerBar = PussycatBar(screenSize.width, barHeight)
        addChild(headerBar, BorderLayout.PAGE_START)
        addChild(content, BorderLayout.CENTER)
        addChild(footerBar, BorderLayout.PAGE_END)
    }

    fun Container.addChild(comp: Component, constraints: Any) {
        if(comp is Themable){
            comp.apply(theme)
        }
        add(comp, constraints)
    }

    abstract fun getMainMenuItems(): List<PussycatMenu>

}