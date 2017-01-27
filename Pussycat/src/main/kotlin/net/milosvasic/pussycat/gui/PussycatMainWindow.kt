package net.milosvasic.pussycat.gui


import com.apple.eawt.Application
import net.milosvasic.pussycat.application.ApplicationInformation
import net.milosvasic.pussycat.content.Messages
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
import java.util.*
import java.util.concurrent.atomic.AtomicBoolean
import javax.imageio.ImageIO
import javax.swing.BoxLayout


abstract class PussycatMainWindow(val information: ApplicationInformation, theme: Theme) : PussycatWindow(theme) {

    val subscriptions = Subscriptions()
    val configuration = PussycatMainWindowConfiguration()

    private val ready = AtomicBoolean()
    private val busy = AtomicBoolean()
    private val list = PussycatList(theme)
    private var anchor: PussycatIconButton? = null
    private val scrollPane = PussycatScrollPane(theme)

    class Subscriptions {
        val STATUS: Listeners<Boolean> = Listeners.obtain()
    }

    val scrollbarEventsListener = object : Listener<SCROLLING_EVENT> {
        override fun onEvent(value: SCROLLING_EVENT?) {
            when (value) {
                SCROLLING_EVENT.TOP_REACHED -> {
                    // top reached - latest data already there
                }
                SCROLLING_EVENT.BOTTOM_REACHED -> {
                    // bottom reached - load more from the past // TODO: Load more --- deep into past
                }
            }
        }
    }

    val scrollbarAnchoringListener = object : Listener<Boolean> {
        override fun onEvent(value: Boolean?) {
            if (value != null) {
                if (value) {
                    anchor?.setState(PussycatIconButton.STATE.ACTIVE.value)
                } else {
                    anchor?.setState(PussycatIconButton.STATE.DEFAULT.value)
                }
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
        headerBar.layout = BoxLayout(headerBar, BoxLayout.PAGE_AXIS)
        val mainMenu = createMainMenu()
        val menuBar = PussycatBar(theme, screenSize.width, barHeight)
        for (item in mainMenu) {
            menuBar.add(item, BorderLayout.WEST)
        }
        headerBar.add(menuBar)
        Thread(Runnable {
            Thread.currentThread().name = Messages.INITIALIZING_TOOLBAR
            val toolBar = PussycatBar(theme, screenSize.width, barHeight)
            for (icon in createToolbar((barHeight * 0.7).toInt())) {
                toolBar.add(icon)
            }
            headerBar.add(toolBar)
        }).start()
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
        list.add(item, 0)
        contentPane.validate()
        if (configuration.isScrollbarAnchored()) {
            val vertical = scrollPane.verticalScrollBar
            vertical.value = vertical.minimum
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

    fun createToolbar(size: Int): List<PussycatIconButton?> {
        val items = mutableListOf<PussycatIconButton?>()
        anchor = getAnchorButton(size)
        items.add(anchor)
        return items
    }

    private fun getAnchorButton(size: Int): PussycatIconButton? {
        val anchorIcons = HashMap<Int, Image>()
        val iconDefault = ImageIO.read(javaClass.classLoader.getResourceAsStream("icons/anchor.png"))
        val iconActive = ImageIO.read(javaClass.classLoader.getResourceAsStream("icons/anchor_active.png"))
        val iconDefaultResized = iconDefault.getScaledInstance(size, size, Image.SCALE_SMOOTH)
        val iconActiveResized = iconActive.getScaledInstance(size, size, Image.SCALE_SMOOTH)
        anchorIcons.put(PussycatIconButton.STATE.DEFAULT.value, iconDefaultResized)
        anchorIcons.put(PussycatIconButton.STATE.ACTIVE.value, iconActiveResized)
        val anchor = PussycatIconButton(size, theme, anchorIcons)
        if (configuration.isScrollbarAnchored()) {
            anchor.setState(PussycatIconButton.STATE.ACTIVE.value)
        } else {
            anchor.setState(PussycatIconButton.STATE.DEFAULT.value)
        }
        anchor.toolTipText = Labels.ANCHOR_BTN_TOOLTIP
        anchor.addActionListener {
            configuration.setScrollbarAnchored(!configuration.isScrollbarAnchored())
        }
        return anchor
    }

}
