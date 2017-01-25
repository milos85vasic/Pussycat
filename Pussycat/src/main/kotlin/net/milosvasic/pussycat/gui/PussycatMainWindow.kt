package net.milosvasic.pussycat.gui


import com.apple.eawt.Application
import net.milosvasic.pussycat.application.ApplicationInformation
import net.milosvasic.pussycat.gui.content.Labels
import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.listeners.Listeners
import net.milosvasic.pussycat.os.OS
import java.awt.*
import java.util.concurrent.atomic.AtomicBoolean
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder

abstract class PussycatMainWindow(val information: ApplicationInformation, theme: Theme) : PussycatWindow(theme) {

    val SUBSCRIPTIONS = Subscriptions()

    class Subscriptions {
        val STATUS: Listeners<Boolean> = Listeners.obtain()
    }

    private val ready = AtomicBoolean()
    private val busy = AtomicBoolean()
    private val list = PussycatList(theme)

    init {
        title = "${information.name} V${information.version} by ${information.author}"
    }

    override fun initialize() {
        super.initialize()
        val screenSize = Toolkit.getDefaultToolkit().screenSize
        val barHeight = (screenSize.height / 100) * 3
        val headerBar = PussycatBar(theme, screenSize.width, barHeight * 2)
        val margin = EmptyBorder(10, 0, 0, 0)
        headerBar.border = CompoundBorder(headerBar.border, margin)
        val mainMenu = createMainMenu()
        for (item in mainMenu) {
            headerBar.add(item, BorderLayout.WEST)
        }
        if (OS.isMacOS()) {
            System.setProperty("com.apple.mac.useScreenMenuBar", "true")
            System.setProperty("apple.laf.useScreenMenuBar", "true")
            val app = Application.getApplication()
            app.setAboutHandler {
                val aboutDialog = PussycatAboutDialog(information, theme, this)
                aboutDialog.open()
            }
            enableOSXFullscreen()
            requestOSXFullscreen(this)
        }
        val footerBar = PussycatBar(theme, screenSize.width, barHeight)
        add(headerBar, BorderLayout.PAGE_START)
        val content = PussycatContent(theme)
        val scrollPane = PussycatScrollPane(theme)
        scrollPane.setViewportView(list)
        content.add(scrollPane, BorderLayout.CENTER)
        add(content, BorderLayout.CENTER)
        add(footerBar, BorderLayout.PAGE_END)
    }

    override fun open() {
        super.open()
        updateStatus(true)
    }

    override fun close() {
        updateStatus(false)
        super.close()
    }

    fun isReady(): Boolean {
        return ready.get()
    }

    fun isBusy(): Boolean {
        return busy.get()
    }

    fun setBusy(bussy: Boolean) {
        this.busy.set(bussy)
    }

    fun addContentItem(item: PussycatListItem) {
        list.add(item)
    }

    abstract fun getMainMenuItems(): List<PussycatMenu>

    private fun updateStatus(status: Boolean) {
        ready.set(status)
        SUBSCRIPTIONS.STATUS.notify(status)
    }

    private fun createMainMenu(): List<PussycatMenu> {
        val items = mutableListOf<PussycatMenu>()
        val file = PussycatMenu(theme, Labels.FILE)
        val quit = PussycatMenuItem(theme, Labels.QUIT)
        quit.addActionListener { System.exit(0) }
        file.addMenuItem(quit)
        items.add(file)
        for (item in getMainMenuItems()) {
            items.add(item)
        }
        val pussycat = PussycatMenu(theme, Labels.PUSSYCAT)
        val about = PussycatMenuItem(theme, Labels.ABOUT)
        about.addActionListener {
            val aboutDialog = PussycatAboutDialog(information, theme, this)
            aboutDialog.open()
        }
        pussycat.addMenuItem(about)
        items.add(pussycat)
        return items
    }

}
