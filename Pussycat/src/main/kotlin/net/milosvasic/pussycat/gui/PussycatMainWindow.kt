package net.milosvasic.pussycat.gui


import com.apple.eawt.Application
import net.milosvasic.pussycat.application.ApplicationInformation
import net.milosvasic.pussycat.gui.content.Labels
import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.os.OS
import java.awt.*
import javax.swing.JMenuBar
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder

abstract class PussycatMainWindow(val information: ApplicationInformation, theme: Theme) : PussycatWindow(theme) {

    init {
        title = "${information.name} V${information.version} by ${information.author}"
    }

    override fun initialize() {
        super.initialize()
        val screenSize = Toolkit.getDefaultToolkit().screenSize
        val barHeight = (screenSize.height / 100) * 3
        val headerBar = PussycatBar(theme, screenSize.width, barHeight)
        val mainMenu = createMainMenu()
        if (OS.isMacOS()) {
            System.setProperty("com.apple.mrj.application.apple.menu.about.name", information.name)
            System.setProperty("com.apple.mac.useScreenMenuBar", "true")
            System.setProperty("apple.laf.useScreenMenuBar", "true")
            val app = Application.getApplication()
            app.setAboutHandler {
                val aboutDialog = PussycatAboutDialog(information, theme, this)
                aboutDialog.open()
            }
            val menu = JMenuBar()
            for (item in mainMenu) {
                menu.add(item)
            }
            app.setDefaultMenuBar(menu)
        } else {
            headerBar.preferredSize = Dimension(headerBar.width, barHeight * 2)
            val margin = EmptyBorder(10, 0, 0, 0)
            headerBar.border = CompoundBorder(headerBar.border, margin)
            for (item in mainMenu) {
                headerBar.add(item, BorderLayout.WEST)
            }
            enableOSXFullscreen()
        }
        val content = PussycatContent(theme)
        val footerBar = PussycatBar(theme, screenSize.width, barHeight)
        add(headerBar, BorderLayout.PAGE_START)
        add(content, BorderLayout.CENTER)
        add(footerBar, BorderLayout.PAGE_END)
    }

    abstract fun getMainMenuItems(): List<PussycatMenu>

    private fun createMainMenu(): List<PussycatMenu> {
        val items = mutableListOf<PussycatMenu>()
        if (!OS.isMacOS()) {
            val file = PussycatMenu(theme, Labels.FILE)
            val quit = PussycatMenuItem(theme, Labels.QUIT)
            quit.addActionListener { System.exit(0) }
            file.addMenuItem(quit)
            items.add(file)
        }
        for (item in getMainMenuItems()) {
            items.add(item)
        }
        if (!OS.isMacOS()) {
            val pussycat = PussycatMenu(theme, Labels.PUSSYCAT)
            val about = PussycatMenuItem(theme, Labels.ABOUT)
            about.addActionListener {
                val aboutDialog = PussycatAboutDialog(information, theme, this)
                aboutDialog.open()
            }
            pussycat.addMenuItem(about)
            items.add(pussycat)
        }
        return items
    }

}