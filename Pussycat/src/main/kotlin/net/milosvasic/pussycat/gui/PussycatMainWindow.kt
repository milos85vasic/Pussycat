package net.milosvasic.pussycat.gui


import com.apple.eawt.Application
import net.milosvasic.pussycat.application.ApplicationInformation
import net.milosvasic.pussycat.core.data.Data
import net.milosvasic.pussycat.events.EVENT
import net.milosvasic.pussycat.gui.content.Labels
import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.listeners.Listeners
import net.milosvasic.pussycat.os.OS
import java.awt.*
import java.util.concurrent.LinkedBlockingDeque
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.AtomicBoolean
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder
import javax.swing.event.ListDataEvent
import javax.swing.event.ListDataListener


abstract class PussycatMainWindow<T>(val information: ApplicationInformation, theme: Theme) : PussycatWindow(theme) {

    val SUBSCRIPTIONS = Subscriptions()

    class Subscriptions {
        val STATUS: Listeners<Boolean> = Listeners.obtain()
    }

    private val ready = AtomicBoolean()
    private var currentDataItem: T? = null
    private val dataQueue = LinkedBlockingQueue<T>()
    private val scrollPane = PussycatScrollPane(theme)

    init {
        title = "${information.name} V${information.version} by ${information.author}"
    }

    val listDataListener = object : ListDataListener {
        override fun contentsChanged(e: ListDataEvent?) {
        }

        override fun intervalRemoved(e: ListDataEvent?) {
        }

        override fun intervalAdded(e: ListDataEvent?) {
            addNextDataItem()
        }
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
        val content = PussycatContent(theme)
        val list = getList()
        list.listModel.addListDataListener(listDataListener)
        scrollPane.setViewportView(list)
        content.add(scrollPane, BorderLayout.CENTER)
        val footerBar = PussycatBar(theme, screenSize.width, barHeight)
        add(headerBar, BorderLayout.PAGE_START)
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

    fun addData(item: T) {
        dataQueue.add(item)
        if (currentDataItem == null) {
            addNextDataItem()
        }
    }

    fun clearData() {
        getList().listModel.clear()
    }

    abstract fun getMainMenuItems(): List<PussycatMenu>

    abstract fun getList(): PussycatList<T>

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

    private fun addNextDataItem() {
        if (!dataQueue.isEmpty()) {
            currentDataItem = dataQueue.poll()
            if (currentDataItem != null) {
                getList().listModel.addElement(currentDataItem)
            }
        } else {
            currentDataItem = null
        }
    }

}
