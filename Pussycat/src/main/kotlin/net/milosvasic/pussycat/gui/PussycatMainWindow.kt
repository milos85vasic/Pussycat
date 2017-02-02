package net.milosvasic.pussycat.gui


import com.apple.eawt.Application
import net.milosvasic.pussycat.application.ApplicationInformation
import net.milosvasic.pussycat.gui.content.Labels
import net.milosvasic.pussycat.gui.data.DataRequestCallback
import net.milosvasic.pussycat.gui.data.DataSizeObtain
import net.milosvasic.pussycat.gui.data.DataRequestStrategy
import net.milosvasic.pussycat.gui.events.SCROLLING_EVENT
import net.milosvasic.pussycat.gui.data.DIRECTION
import net.milosvasic.pussycat.gui.factory.PussycatListItemsFactory
import net.milosvasic.pussycat.gui.factory.PussycatListItemsRequest
import net.milosvasic.pussycat.gui.data.DataCallback
import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.listeners.Listener
import net.milosvasic.pussycat.listeners.Listeners
import net.milosvasic.pussycat.os.OS
import java.awt.*
import java.awt.event.ActionListener
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicInteger
import javax.swing.BoxLayout

abstract class PussycatMainWindow(val information: ApplicationInformation, theme: Theme) : PussycatWindow(theme), DataCallback {

    val subscriptions = Subscriptions()
    var dataSizeObtain: DataSizeObtain? = null
    var dataRequestStrategy: DataRequestStrategy? = null

    private val busy = AtomicBoolean()
    private val ready = AtomicBoolean()
    private val list = PussycatList(theme)
    private val lastItemIndex = AtomicInteger(0)
    private val firstItemIndex = AtomicInteger(0)
    private val scrollPane = PussycatScrollPane(theme)
    private val screenSize: Dimension = Toolkit.getDefaultToolkit().screenSize

    private var btnPageUp: PussycatIconButton? = null
    private var btnPageDown: PussycatIconButton? = null
    private var btnGoTop: PussycatIconButton? = null
    private var btnGoBottom: PussycatIconButton? = null

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
                    if (firstItemIndex.get() > 0 && !busy.get()) {
                        busy.set(true)
                        dataRequestStrategy?.barrierReached(firstItemIndex.get(), DIRECTION.UP)
                    }
                }
                SCROLLING_EVENT.BOTTOM_DELTA_REACHED -> {
                    if (!busy.get()) {
                        busy.set(true)
                        dataRequestStrategy?.barrierReached(lastItemIndex.get(), DIRECTION.DOWN)
                        checkListCapacity(DIRECTION.UP)
                    }
                }
            }
        }
    }

    init {
        title = "${information.name} V${information.version} by ${information.author}"
    }

    override fun initialize() {
        super.initialize()
        val barHeight = (screenSize.height / 100) * 3
        val headerBar = PussycatBar(theme, screenSize.width, (barHeight * 2.7).toInt())
        headerBar.layout = BoxLayout(headerBar, BoxLayout.PAGE_AXIS)
        val toolbar = createToolbar(barHeight)
        val menuBar = createMenuBar(barHeight)
        headerBar.add(menuBar)
        headerBar.add(toolbar)
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

    override fun onData(request: PussycatListItemsRequest, items: List<PussycatListItem>, direction: DIRECTION) {
        println("on data ${items.size} $direction") // TODO: Remove this.
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
        updateNavigationButtons()
        request.dataRequestCallback?.finished()
        busy.set(false)
    }

    override fun onDataRequestRejected(request: PussycatListItemsRequest) {
        busy.set(false)
        println("Request rejected.") // TODO: Remove this.
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
            if (firstItemIndex.get() > 0) {
                btnGoTop?.setState(PussycatIconButton.STATE.DISABLED)
                busy.set(true)
                list.removeAll()
                validate()
                val vertical = scrollPane.verticalScrollBar
                vertical.value = vertical.minimum
                lastItemIndex.set(0)
                firstItemIndex.set(0)
                dataRequestStrategy?.refresh()
            } else {
                val vertical = scrollPane.verticalScrollBar
                vertical.value = vertical.minimum
            }
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
            if (dataSizeObtain != null) {
                val sizeObtain = dataSizeObtain as DataSizeObtain
                btnGoBottom?.setState(PussycatIconButton.STATE.DISABLED)
                busy.set(true)
                list.removeAll()
                validate()
                lastItemIndex.set(sizeObtain.getDataSize() - 1)
                firstItemIndex.set(lastItemIndex.get() + 1)
                val callback = object : DataRequestCallback {
                    override fun finished() {
                        val vertical = scrollPane.verticalScrollBar
                        vertical.value = vertical.maximum
                    }
                }
                dataRequestStrategy?.requestData(
                        lastItemIndex.get(), PussycatListItemsFactory.REQUEST_DELTA, DIRECTION.UP, callback
                )
            }
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

    private fun createMenuBar(barHeight: Int): PussycatBar {
        val mainMenu = createMainMenu()
        val menuBar = PussycatBar(theme, screenSize.width, barHeight)
        for (item in mainMenu) {
            menuBar.add(item, BorderLayout.WEST)
        }
        return menuBar
    }

    private fun createToolbar(barHeight: Int): PussycatToolbar {
        val size = (barHeight * 0.8).toInt()
        val toolBar = PussycatToolbar(theme, screenSize.width, barHeight)
        btnGoTop = getGoTopButton(size)
        btnGoBottom = getGoBottomButton(size)
        btnPageUp = getPageTopButton(size)
        btnPageDown = getPageBottomButton(size)
        toolBar.add(btnGoTop)
        toolBar.add(btnGoBottom)
        toolBar.add(btnPageUp)
        toolBar.add(btnPageDown)
        btnGoTop?.setState(PussycatIconButton.STATE.DISABLED)
        btnGoBottom?.setState(PussycatIconButton.STATE.DISABLED)
        btnPageUp?.setState(PussycatIconButton.STATE.DISABLED)
        btnPageDown?.setState(PussycatIconButton.STATE.DISABLED)
        return toolBar
    }

    private fun updateNavigationButtons() {
        val sizeObtain = dataSizeObtain as DataSizeObtain
        if (sizeObtain.getDataSize() >= PussycatListItemsFactory.REQUEST_DELTA) {
            if (dataSizeObtain != null) {
                if (btnGoTop != null) {
                    val btn = btnGoTop as PussycatIconButton
                    if (!btn.isEnabled) {
                        btn.setState(PussycatIconButton.STATE.DEFAULT)
                    }
                }
                if (btnGoBottom != null) {
                    val btn = btnGoBottom as PussycatIconButton
                    if (!btn.isEnabled) {
                        btn.setState(PussycatIconButton.STATE.DEFAULT)
                    }
                }
                if (btnPageDown != null) {
                    val btn = btnPageDown as PussycatIconButton
                    if (!btn.isEnabled) {
                        btn.setState(PussycatIconButton.STATE.DEFAULT)
                    }
                }
                if (btnPageUp != null) {
                    val btn = btnPageUp as PussycatIconButton
                    if (!btn.isEnabled) {
                        btn.setState(PussycatIconButton.STATE.DEFAULT)
                    }
                }
            }
        }
    }

}
