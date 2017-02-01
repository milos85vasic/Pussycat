package net.milosvasic.pussycat.gui


import com.apple.eawt.Application
import net.milosvasic.pussycat.application.ApplicationInformation
import net.milosvasic.pussycat.content.Messages
import net.milosvasic.pussycat.gui.content.Labels
import net.milosvasic.pussycat.gui.events.RequestBarrierReachedCallback
import net.milosvasic.pussycat.gui.events.SCROLLING_EVENT
import net.milosvasic.pussycat.gui.factory.DIRECTION
import net.milosvasic.pussycat.gui.factory.PussycatListItemsFactory
import net.milosvasic.pussycat.gui.factory.PussycatListItemsRequestCallback
import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.listeners.Listener
import net.milosvasic.pussycat.listeners.Listeners
import net.milosvasic.pussycat.os.OS
import java.awt.*
import java.awt.event.ActionListener
import java.util.*
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicInteger
import javax.imageio.ImageIO
import javax.swing.BoxLayout

abstract class PussycatMainWindow(val information: ApplicationInformation, theme: Theme) : PussycatWindow(theme), PussycatListItemsRequestCallback {

    val subscriptions = Subscriptions()
    var requestBarrierReachedCallback: RequestBarrierReachedCallback? = null

    private val busy = AtomicBoolean()
    private val ready = AtomicBoolean()
    private val list = PussycatList(theme)
    private val lastItemIndex = AtomicInteger(0)
    private val firstItemIndex = AtomicInteger(0)
    private val scrollPane = PussycatScrollPane(theme)

    class Subscriptions {
        val STATUS: Listeners<Boolean> = Listeners.obtain()
    }

    val scrollbarEventsListener = object : Listener<SCROLLING_EVENT> {
        override fun onEvent(value: SCROLLING_EVENT?) {
            when (value) {
                SCROLLING_EVENT.TOP_REACHED -> {
                    // top reached
                }
                SCROLLING_EVENT.BOTTOM_REACHED -> {
                    // bottom reached
                }
                SCROLLING_EVENT.TOP_DELTA_REACHED -> {
                    if (firstItemIndex.get() > 0) {
                        requestBarrierReachedCallback?.onBarrierReached(firstItemIndex.get(), DIRECTION.UP)
                    }
                }
                SCROLLING_EVENT.BOTTOM_DELTA_REACHED -> {
                    requestBarrierReachedCallback?.onBarrierReached(lastItemIndex.get())
                    checkListCapacity(DIRECTION.UP)
                }
            }
        }
    }

    init {
        title = "${information.name} V${information.version} by ${information.author}"
    }

    override fun initialize() {
        super.initialize()
        val screenSize = Toolkit.getDefaultToolkit().screenSize
        val barHeight = (screenSize.height / 100) * 3
        val headerBar = PussycatBar(theme, screenSize.width, (barHeight * 2.7).toInt())
        headerBar.layout = BoxLayout(headerBar, BoxLayout.PAGE_AXIS)
        val mainMenu = createMainMenu()
        val menuBar = PussycatBar(theme, screenSize.width, barHeight)
        for (item in mainMenu) {
            menuBar.add(item, BorderLayout.WEST)
        }
        headerBar.add(menuBar)
        Thread(Runnable {
            Thread.currentThread().name = Messages.INITIALIZING_TOOLBAR
            val toolBar = PussycatToolbar(theme, screenSize.width, barHeight)
            for (icon in createToolbar((barHeight * 0.8).toInt())) {
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
    }

    override fun open() {
        super.open()
        updateStatus(true)
    }

    override fun close() {
        scrollPane.scrollingEvents.unsubscribe(scrollbarEventsListener)
        updateStatus(false)
        super.close()
    }

    override fun onData(items: List<PussycatListItem>, direction: DIRECTION) {
        if (direction == DIRECTION.DOWN) {
            for (item in items) {
                appendPussycatListItem(item)
                lastItemIndex.incrementAndGet()
            }
        } else {
            for (item in items) {
                prependPussycatListItem(item)
                firstItemIndex.decrementAndGet()
                checkListCapacity(DIRECTION.DOWN)
            }
        }
    }

    fun isReady(): Boolean {
        return ready.get()
    }

    fun isBusy(): Boolean {
        return busy.get()
    }

    fun appendPussycatListItem(item: PussycatListItem) {
        list.add(item)
        contentPane.validate()
    }

    fun prependPussycatListItem(item: PussycatListItem) {
        list.add(item, 0)
        contentPane.validate()
        val vertical = scrollPane.verticalScrollBar
        vertical.value += item.height
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

    private fun createToolbar(size: Int): List<PussycatIconButton?> {
        val items = mutableListOf<PussycatIconButton?>()
        items.add(getGoTopButton(size))
        items.add(getGoBottomButton(size))
        items.add(getPageTopButton(size))
        items.add(getPageBottomButton(size))
        return items
    }

    private fun getPageTopButton(size: Int): PussycatIconButton? {
        val action = ActionListener {
            val vertical = scrollPane.verticalScrollBar
            vertical.value = vertical.minimum
        }
        val definition = PussycatIconButtonDefinition(
                size,
                "page_top",
                Labels.PAGE_TOP_BTN_TOOLTIP,
                action,
                "page_top",
                "page_top_disabled"
        )
        return PussycatIconButton.create(theme, definition)
    }

    private fun getPageBottomButton(size: Int): PussycatIconButton? {
        val action = ActionListener {
            val vertical = scrollPane.verticalScrollBar
            vertical.value = vertical.maximum
        }
        val definition = PussycatIconButtonDefinition(
                size,
                "page_bottom",
                Labels.PAGE_BOTTOM_BTN_TOOLTIP,
                action,
                "page_bottom",
                "page_bottom_disabled"
        )
        return PussycatIconButton.create(theme, definition)
    }

    private fun getGoTopButton(size: Int): PussycatIconButton? {
        val action = ActionListener {
            //            val vertical = scrollPane.verticalScrollBar
//            vertical.value = vertical.minimum
        }
        val definition = PussycatIconButtonDefinition(
                size,
                "go_top",
                Labels.GO_TOP_BTN_TOOLTIP,
                action,
                "go_top",
                "go_top_disabled"
        )
        return PussycatIconButton.create(theme, definition)
    }

    private fun getGoBottomButton(size: Int): PussycatIconButton? {
        val action = ActionListener {
            //            val vertical = scrollPane.verticalScrollBar
//            vertical.value = vertical.maximum
        }
        val definition = PussycatIconButtonDefinition(
                size,
                "go_bottom",
                Labels.GO_BOTTOM_BTN_TOOLTIP,
                action,
                "go_bottom",
                "go_bottom_disabled"
        )
        return PussycatIconButton.create(theme, definition)
    }

    private fun checkListCapacity(direction: DIRECTION) {
        if (list.componentCount >= PussycatListItemsFactory.REQUEST_DELTA * 2) {
            for (x in 0..PussycatListItemsFactory.REQUEST_DELTA) {
                if (direction == DIRECTION.UP) {
                    list.remove(0)
                    list.validate()
                    firstItemIndex.incrementAndGet()
                } else {
                    list.remove(list.componentCount - 1)
                    list.validate()
                    lastItemIndex.decrementAndGet()
                }
            }
        }
    }

}
