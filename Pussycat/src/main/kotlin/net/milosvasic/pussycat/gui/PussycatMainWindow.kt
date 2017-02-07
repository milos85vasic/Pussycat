package net.milosvasic.pussycat.gui


import com.apple.eawt.Application
import net.milosvasic.pussycat.application.ApplicationInformation
import net.milosvasic.pussycat.core.COMMAND
import net.milosvasic.pussycat.gui.commands.CommandCallback
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
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import net.milosvasic.pussycat.listeners.Listener
import net.milosvasic.pussycat.listeners.Listeners
import net.milosvasic.pussycat.os.OS
import java.awt.*
import java.awt.event.ActionListener
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicInteger
import javax.swing.BoxLayout
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder

abstract class PussycatMainWindow(val information: ApplicationInformation, theme: Theme) : PussycatWindow(theme), DataCallback {

    val subscriptions = Subscriptions()
    var dataSizeObtain: DataSizeObtain? = null
    var dataRequestStrategy: DataRequestStrategy? = null

    var commandCallback: CommandCallback? = null
        get() = field
        set(value) {
            field = value
            updateNavigationButtons()
        }

    private val busy = AtomicBoolean()
    private val ready = AtomicBoolean()
    private val list = PussycatList(theme)
    private val lastItemIndex = AtomicInteger(0)
    private val firstItemIndex = AtomicInteger(0)
    private val footerLeft = PussycatLabel(theme)
    private val scrollPane = PussycatScrollPane(theme)
    private val screenSize: Dimension = Toolkit.getDefaultToolkit().screenSize

    private var btnPageUp: PussycatIconButton? = null
    private var btnPageDown: PussycatIconButton? = null
    private var btnGoTop: PussycatIconButton? = null
    private var btnGoBottom: PussycatIconButton? = null
    private var btnClear: PussycatIconButton? = null
    private var btnRefresh: PussycatIconButton? = null

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
                        dataRequestStrategy?.requestData(
                                firstItemIndex.get(), PussycatListItemsFactory.REQUEST_DELTA / 2, DIRECTION.UP
                        )
                    }
                }
                SCROLLING_EVENT.BOTTOM_DELTA_REACHED -> {
                    if (!busy.get()) {
                        busy.set(true)
                        dataRequestStrategy?.requestData(
                                lastItemIndex.get(), PussycatListItemsFactory.REQUEST_DELTA / 2, DIRECTION.DOWN
                        )
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
            app.removeAboutMenuItem()
            enableOSXFullscreen()
            requestOSXFullscreen(this)
        }
        val footerBar = PussycatBar(theme, screenSize.width, barHeight)
        footerLeft.border = CompoundBorder(footerLeft.border, EmptyBorder(2, 10, 0, 0))
        footerBar.add(footerLeft)
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
    }

    fun isReady(): Boolean {
        return ready.get()
    }

    fun isBusy(): Boolean {
        return busy.get()
    }

    fun clear() {
        busy.set(true)
        list.removeAll()
        validate()
        updateNavigationButtons()
        lastItemIndex.set(0)
        firstItemIndex.set(0)
        busy.set(false)
    }

    fun refresh() {
        busy.set(true)
        list.removeAll()
        validate()
        updateNavigationButtons()
        val vertical = scrollPane.verticalScrollBar
        vertical.value = vertical.minimum
        lastItemIndex.set(0)
        firstItemIndex.set(0)
        dataRequestStrategy?.requestData(0, PussycatListItemsFactory.REQUEST_DELTA, DIRECTION.DOWN)
    }

    fun updateDataCount(count: Int){
        footerLeft.text = "${Labels.DATA_LOADED}: $count"
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
        btnRefresh = getRefreshButton(size)
        btnClear = getClearButton(size)
        btnGoTop = getGoTopButton(size)
        btnGoBottom = getGoBottomButton(size)
        btnPageUp = getPageTopButton(size)
        btnPageDown = getPageBottomButton(size)
        toolBar.add(btnRefresh)
        toolBar.add(btnClear)
        toolBar.add(btnGoTop)
        toolBar.add(btnGoBottom)
        toolBar.add(btnPageUp)
        toolBar.add(btnPageDown)
        updateNavigationButtons()
        return toolBar
    }

    private fun getPageTopButton(size: Int): PussycatIconButton? {
        val action = ActionListener {
            if (list.componentCount == 0) {
                return@ActionListener
            }
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
            if (list.componentCount == 0) {
                return@ActionListener
            }
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
            if (list.componentCount == 0) {
                return@ActionListener
            }
            if (firstItemIndex.get() > 0) {
                refresh()
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
            if (list.componentCount == 0) {
                return@ActionListener
            }
            if (dataSizeObtain != null) {
                val sizeObtain = dataSizeObtain as DataSizeObtain
                busy.set(true)
                list.removeAll()
                validate()
                updateNavigationButtons()
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

    private fun getRefreshButton(size: Int): PussycatIconButton? {
        val action = ActionListener {
            commandCallback?.execute(COMMAND.RESET)
        }
        val definition = PussycatIconButtonDefinition(
                size,
                "refresh",
                Labels.REFRESH_BTN_TOOLTIP,
                action,
                "refresh",
                "refresh_disabled"
        )
        return PussycatIconButton.create(theme, definition)
    }

    private fun getClearButton(size: Int): PussycatIconButton? {
        val action = ActionListener {
            if (commandCallback != null) {
                commandCallback?.execute(COMMAND.CLEAR)
            }
        }
        val definition = PussycatIconButtonDefinition(
                size,
                "clean",
                Labels.CLEAR_BTN_TOOLTIP,
                action,
                "clean",
                "clean_disabled"
        )
        return PussycatIconButton.create(theme, definition)
    }

    private fun updateNavigationButtons() {
        if (commandCallback != null) {
            enableBtn(btnClear)
            enableBtn(btnRefresh)
        } else {
            disableBtn(btnClear)
            disableBtn(btnRefresh)
        }
        if (list.componentCount == 0) {
            disableBtn(btnGoTop)
            disableBtn(btnGoBottom)
            disableBtn(btnPageDown)
            disableBtn(btnPageUp)
            return
        }
        val sizeObtain = dataSizeObtain as DataSizeObtain
        if (sizeObtain.getDataSize() >= PussycatListItemsFactory.REQUEST_DELTA) {
            if (dataSizeObtain != null) {
                enableBtn(btnGoTop)
                enableBtn(btnGoBottom)
                enableBtn(btnPageDown)
                enableBtn(btnPageUp)
            }
        }
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

    private fun appendPussycatListItem(item: PussycatListItem) {
        list.add(item)
        contentPane.validate()
    }

    private fun prependPussycatListItem(item: PussycatListItem) {
        list.add(item, 0)
        contentPane.validate()
        val vertical = scrollPane.verticalScrollBar
        vertical.value += item.height
    }

    private fun enableBtn(button: PussycatIconButton?) {
        if (button != null) {
            if (!button.isEnabled) {
                button.setState(PussycatIconButton.STATE.DEFAULT)
            }
        }
    }

    private fun disableBtn(button: PussycatIconButton?) {
        button?.setState(PussycatIconButton.STATE.DISABLED)
    }

}
