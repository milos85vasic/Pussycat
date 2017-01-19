package net.milosvasic.pussycat.gui


import com.apple.eawt.Application
import net.milosvasic.pussycat.application.ApplicationInformation
import net.milosvasic.pussycat.os.OS
import java.awt.*
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder

abstract class PussycatMainWindow(val information: ApplicationInformation) : PussycatWindow() {

    init {
        title = "${information.name} V${information.version} by ${information.author}"
    }

    override fun initialize() {
        super.initialize()
        val screenSize = Toolkit.getDefaultToolkit().screenSize
        val barHeight = (screenSize.height / 100) * 3
        val headerBar = PussycatBar(screenSize.width, barHeight)
        val mainMenuBar = PussycatMenuBar(headerBar.width, barHeight)
        for (item in getMainMenuItems()) {
            mainMenuBar.add(item)
        }
        if (!OS.isMacOS()) { // TODO: Remove negation.
            System.setProperty("com.apple.mrj.application.apple.menu.about.name", information.name)
            System.setProperty("com.apple.mac.useScreenMenuBar", "true")
            System.setProperty("apple.laf.useScreenMenuBar", "true")
            val app = Application.getApplication()
            app.setAboutHandler {
                val aboutDialog = PussycatAboutDialog(information, this)
                aboutDialog.open()
            }
            app.setDefaultMenuBar(mainMenuBar)
        } else {
            headerBar.preferredSize = Dimension(headerBar.width, barHeight * 2)
            val margin = EmptyBorder(10, 0, 0, 0)
            headerBar.border = CompoundBorder(headerBar.border, margin)
            headerBar.add(mainMenuBar, BorderLayout.NORTH)
        }
        val content = PussycatContent()
        val footerBar = PussycatBar(screenSize.width, barHeight)
        add(headerBar, BorderLayout.PAGE_START)
        add(content, BorderLayout.CENTER)
        add(footerBar, BorderLayout.PAGE_END)
    }

    abstract fun getMainMenuItems(): List<PussycatMenu>

}