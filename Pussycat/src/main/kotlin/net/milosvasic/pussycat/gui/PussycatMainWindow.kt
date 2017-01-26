package net.milosvasic.pussycat.gui


import com.apple.eawt.Application
import net.milosvasic.pussycat.application.ApplicationInformation
import net.milosvasic.pussycat.gui.configuration.PussycatMainWindowConfiguration
import net.milosvasic.pussycat.gui.content.Labels
import net.milosvasic.pussycat.gui.events.SCROLLING_EVENT
import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.listeners.Listener
import net.milosvasic.pussycat.listeners.Listeners
import net.milosvasic.pussycat.os.OS
import java.awt.*
import java.awt.event.MouseEvent
import java.awt.event.MouseListener
import java.util.concurrent.atomic.AtomicBoolean
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder


abstract class PussycatMainWindow(val information: ApplicationInformation, theme: Theme) : PussycatWindow(theme) {

    val subscriptions = Subscriptions()
    val configuration = PussycatMainWindowConfiguration()

    private val ready = AtomicBoolean()
    private val busy = AtomicBoolean()
    private val list = PussycatList(theme)
    private val scrollPane = PussycatScrollPane(theme)

    class Subscriptions {
        val STATUS: Listeners<Boolean> = Listeners.obtain()
    }

    val scrollbarEventsListener = object : Listener<SCROLLING_EVENT> {
        override fun onEvent(value: SCROLLING_EVENT?) {
            when (value) {
                SCROLLING_EVENT.BOTTOM_REACHED -> {
                    if (!configuration.isScrollbarAnchored()) {
                        configuration.setScrollbarAnchored(true)
                    }
                }
                SCROLLING_EVENT.TOP_REACHED -> {
                    // reached the top
                }
            }
        }
    }

    val scrollbarAnchoringListener = object : Listener<Boolean> {
        override fun onEvent(value: Boolean?) {
            if (value != null) {
                println("Anchoring: [ $value ]")
            }
        }
    }

    val listClickListener = object : MouseListener {
        override fun mouseClicked(e: MouseEvent?) {
            configuration.setScrollbarAnchored(false)
        }

        override fun mouseEntered(e: MouseEvent?) {
        }

        override fun mouseReleased(e: MouseEvent?) {
        }

        override fun mouseExited(e: MouseEvent?) {
        }

        override fun mousePressed(e: MouseEvent?) {
        }
    }

    init {
        title = "${information.name} V${information.version} by ${information.author}"
        list.addMouseListener(listClickListener)
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
        scrollPane.setViewportView(list)
        scrollPane.scrollingEvents.subscribe(scrollbarEventsListener)
        content.add(scrollPane, BorderLayout.CENTER)
        add(content, BorderLayout.CENTER)
        add(footerBar, BorderLayout.PAGE_END)
        configuration.scrollbarAnchoring.subscribe(scrollbarAnchoringListener)
    }

    override fun open() {
        super.open()
        updateStatus(true)
    }

    override fun close() {
        configuration.scrollbarAnchoring.unsubscribe(scrollbarAnchoringListener)
        scrollPane.scrollingEvents.unsubscribe(scrollbarEventsListener)
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
        contentPane.validate()
        if (configuration.isScrollbarAnchored()) {
            val vertical = scrollPane.verticalScrollBar
            vertical.value = vertical.maximum
        }
    }

    abstract fun getMainMenuItems(): List<PussycatMenu>

    private fun updateStatus(status: Boolean) {
        ready.set(status)
        subscriptions.STATUS.notify(status)
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
