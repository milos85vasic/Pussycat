package net.milosvasic.pussycat.gui


import com.apple.eawt.Application
import net.milosvasic.pussycat.application.ApplicationInformation
import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.os.OS
import java.awt.*
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder

abstract class PussycatMainWindow(val information: ApplicationInformation, theme: Theme) : PussycatWindow(theme) {

    val menuFactory = PussycatMenuFactory(theme)

    init {
        title = "${information.name} V${information.version} by ${information.author}"
    }

    override fun initialize() {
        super.initialize()
        val screenSize = Toolkit.getDefaultToolkit().screenSize
        val barHeight = (screenSize.height / 100) * 3
        val headerBar = PussycatBar(theme, screenSize.width, barHeight)
        val mainMenuBar = PussycatMenuBar(theme, headerBar.width, barHeight)
        val mainMenuItems = menuFactory.MAIN.create(getMainMenuItems())
        for (item in mainMenuItems) {
            mainMenuBar.add(item)
        }
        if (OS.isMacOS()) {
            System.setProperty("com.apple.mrj.application.apple.menu.about.name", information.name)
            System.setProperty("com.apple.mac.useScreenMenuBar", "true")
            System.setProperty("apple.laf.useScreenMenuBar", "true")
            val app = Application.getApplication()
            app.setAboutHandler {
                val aboutDialog = PussycatAboutDialog(information, theme, this)
                aboutDialog.open()
            }
            app.setDefaultMenuBar(mainMenuBar)
        } else {
            headerBar.preferredSize = Dimension(headerBar.width, barHeight * 2)
            val margin = EmptyBorder(10, 0, 0, 0)
            headerBar.border = CompoundBorder(headerBar.border, margin)
            headerBar.add(mainMenuBar, BorderLayout.NORTH)
        }
        val content = PussycatContent(theme)
        val footerBar = PussycatBar(theme, screenSize.width, barHeight)
        add(headerBar, BorderLayout.PAGE_START)
        add(content, BorderLayout.CENTER)
        add(footerBar, BorderLayout.PAGE_END)
    }

    abstract fun getMainMenuItems(): List<PussycatMenu>

}