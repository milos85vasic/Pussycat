package net.milosvasic.pussycat.android

import com.android.ddmlib.Log
import com.apple.eawt.Application
import net.milosvasic.pussycat.android.command.ANDROID_COMMAND
import net.milosvasic.pussycat.android.data.AndroidData
import net.milosvasic.pussycat.android.data.AndroidLogCatMessage
import net.milosvasic.pussycat.android.gui.GuiPussycatListItemFactory
import net.milosvasic.pussycat.android.resources.WebGuiPussycatServerResourceProvider
import net.milosvasic.pussycat.application.ApplicationInformation
import net.milosvasic.pussycat.content.Messages
import net.milosvasic.pussycat.core.COMMAND
import net.milosvasic.pussycat.events.EVENT
import net.milosvasic.pussycat.gui.OnSplashComplete
import net.milosvasic.pussycat.gui.PussycatMenuItemDefinition
import net.milosvasic.pussycat.gui.PussycatSplashScreen
import net.milosvasic.pussycat.gui.content.Labels
import net.milosvasic.pussycat.gui.factory.PussycatListItemsFactory
import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.listeners.Listener
import net.milosvasic.pussycat.os.OS
import net.milosvasic.pussycat.web.PussycatServer
import java.awt.Desktop
import java.awt.MenuItem
import java.awt.PopupMenu
import java.awt.event.ActionListener
import java.awt.image.BufferedImage
import java.net.URI
import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.atomic.AtomicBoolean
import javax.imageio.ImageIO
import javax.swing.JFrame

class WebGuiPussycat(host: String, port: Int, information: ApplicationInformation, theme: Theme) : AndroidPussycat() {

    private val resourceProvider = WebGuiPussycatServerResourceProvider()
    private val server = PussycatServer(port, resourceProvider)
    private var favicon: BufferedImage? = null
    private val location = URI("http://$host:$port")
    private val filterApplying = AtomicBoolean(false)
    private val pussycatListItemFactory = GuiPussycatListItemFactory(theme)
    private var pussycatListItemsFactory: PussycatListItemsFactory<AndroidLogCatMessage>? = null

    val splashScreenCallback: OnSplashComplete = object : OnSplashComplete {
        override fun onComplete(success: Boolean) {
            try {
                server.start()
            } catch (e: Exception) {
                logger.e(TAG, "${Labels.ERROR}: $e")
            }
            Desktop.getDesktop().browse(location)
        }
    }

    // TODO: Switch to API calls
//    val dataStrategy = object : DataStrategy {
//        override fun getFirstIndex(): Int {
//            var index = 0
//            if (pussycatListItemsFactory != null) {
//                val factory = pussycatListItemsFactory as PussycatListItemsFactory<AndroidLogCatMessage>
//                index = factory.getFirstIndex()
//            }
//            return index
//        }
//
//        override fun getLastIndex(): Int {
//            var index = data.get().size - 1
//            if (pussycatListItemsFactory != null) {
//                val factory = pussycatListItemsFactory as PussycatListItemsFactory<AndroidLogCatMessage>
//                index = factory.getLastIndex()
//            }
//            return index
//        }
//
//        override fun limitToIndexes(indexes: List<Int>) {
//            pussycatListItemsFactory?.applyIndexLimits(indexes)
//        }
//
//        override fun requestData(from: Int, amount: Int, direction: DIRECTION, callback: DataRequestCallback?) {
//            val request = PussycatListItemsRequest(from, amount, direction, mainWindow, callback)
//            pussycatListItemsFactory?.requestData(request)
//        }
//
//        override fun releaseData(index: Int) {
//            pussycatListItemsFactory?.releaseData(index)
//        }
//    }

//    val sizeObtain = object : DataSizeObtain {
//        override fun getDataSize(): Int {
//            return data.get().size
//        }
//    }

//    val commandCallback = object : CommandCallback {
//        override fun execute(command: COMMAND) {
//            this@WebGuiPussycat.execute(command)
//        }
//    }

//    val filterCallback = object : FilteringStrategy {
//        override fun filter(value: String) {
//            data.apply(value)
//        }
//    }

//    val filterObtain = object : FilterObtain {
//        override fun getFilterValue(): String {
//            val filter = data.getFilterPattern()
//            return if (Text.isEmpty(filter)) {
//                Labels.TEXT_FIELD_FILTER_HINT_EMPTY
//            } else {
//                "${Labels.TEXT_FIELD_FILTER_HINT} $filter"
//            }
//        }
//    }

    val splashScreen = PussycatSplashScreen(information, theme, splashScreenCallback)

    init {
        splashScreen.defaultCloseOperation = JFrame.EXIT_ON_CLOSE
        favicon = ImageIO.read(javaClass.classLoader.getResourceAsStream("icons/Favicon.png"))
        pussycatListItemsFactory = PussycatListItemsFactory(data, pussycatListItemFactory)
    }

    val eventsListener = object : Listener<EVENT> {
        override fun onEvent(value: EVENT?) {
            if (value == EVENT.STOP) {
                SUBSCRIPTIONS.EVENTS.unsubscribe(this)
                SUBSCRIPTIONS.FILESYSTEM_LOADING_PROGRESS.unsubscribe(filesystemProgressListener)
                /*

                String originalHandle = driver.getWindowHandle();

                //Do something to open new tabs

                for(String handle : driver.getWindowHandles()) {
                    if (!handle.equals(originalHandle)) {
                        driver.switchTo().window(handle);
                        driver.close();
                    }
                }

                driver.switchTo().window(originalHandle);

                * */
            }
        }
    }

    val filesystemProgressListener = object : Listener<Double> {
        override fun onEvent(value: Double?) {
            val s = String.format("%.0f", value)
            splashScreen.updateStatus("${Messages.PARSING}: $s%")
        }
    }

    override fun initData() {
        data = AndroidData(this)
    }

    override fun start(args: Array<String>) {
        if (configuration.exitOnStop) {
            // TODO: Handle this.
        }
        splashScreen.start()

        val hook = Thread(Runnable {
            execute(COMMAND.STOP)
        })

        SUBSCRIPTIONS.EVENTS.subscribe(eventsListener)
        SUBSCRIPTIONS.FILESYSTEM_LOADING_PROGRESS.subscribe(filesystemProgressListener)

        Runtime.getRuntime().addShutdownHook(hook)
        initialize(args)
    }

    override fun execute(executable: COMMAND, params: Array<String>) {
        super.execute(executable, params)
        when (executable) {
            COMMAND.RESET -> {
                // TODO: Execute api call
            }
        }
    }

    override fun status() {

    }

    override fun clear() {
        // TODO: Execute api call
    }

    override fun printLine(text: String) {
        // TODO: Change this.
        println(text)
    }

    override fun printLine(text: String, logLevel: Log.LogLevel) {
        // TODO: Change this.
        println(text)
    }

    override fun printLine(line: AndroidLogCatMessage) {
        // pussycatListItemsFactory?.addRawData(line, data.get().indexOf(line)) // TODO: Handle this.
    }

    override fun getPrintableLogLevelValue(): String {
        return ""
    }

    override fun getPrintableFilterValue(): String {
        return ""
    }

    override fun onParsingComplete() {
        //        mainWindow.updateDataCount(data.get().size) // TODO: Execute api call
        data.get().forEachIndexed { i, msg -> pussycatListItemsFactory?.addRawData(msg, i) }
    }

    override fun onFilterPatternSet(data: CopyOnWriteArrayList<AndroidLogCatMessage>) {
        if (!filterApplying.get()) {
            Thread(Runnable {
                Thread.currentThread().name = Labels.DATA_FILTERING_THREAD
                filterApplying.set(true)
                val indexes = mutableListOf<Int>()
                data
                        .filter { this.data.evaluate(it) }
                        .forEach { msg ->
                            if (this.data.getLogLevel() != null) {
                                if (msg.logLevel == this.data.getLogLevel()) {
                                    indexes.add(data.indexOf(msg))
                                }
                            } else {
                                indexes.add(data.indexOf(msg))
                            }
                        }
//                mainWindow.onFilteringResult(indexes) // TODO: Execute api call
                filterApplying.set(false)
            }).start()
        }
    }

    private fun initialize(args: Array<String>) {
        Thread(
                Runnable {
                    initPopupMenu()
                    var adb = true
                    var file: String = ""
                    for (arg in args) {
                        var parsedArg: String
                        try {
                            parsedArg = getArgumentOption(arg.trim())
                        } catch (e: Exception) {
                            printLine("Error parsing arguments: $arg\n" + e.message)
                            adb = false
                            execute(ANDROID_COMMAND.PARENT.STOP)
                            break
                        }
                        when (parsedArg) {
                            "--adb" -> adb = true
                            "--filesystem" -> {
                                adb = false
                                file = getArgumentValue(arg)
                            }
                            else -> {
                            }
                        }
                    }
                    if (adb) {
                        execute(ANDROID_COMMAND.PARENT.LIVE)
                    } else {
                        execute(ANDROID_COMMAND.PARENT.FILESYSTEM, arrayOf(file))
                    }
                    splashScreen.updateStatus("Loading complete")
                    splashScreen.finish()
                }
        ).start()
    }

    override fun executeFilesystemRunnable(runnable: Runnable) {
        runnable.run()
    }

    private fun initPopupMenu() {
        val items = getPopupMenuItems()
//        mainWindow.addPopUpMenuItems(items) // TODO: Execute api call
        if (OS.isMacOS()) {
            val popupMenu = PopupMenu()
            for (item in items) {
                val menuItem = MenuItem(item.title)
                menuItem.addActionListener(item.action)
                popupMenu.add(menuItem)
            }
            val app = Application.getApplication()
            app.dockIconImage = favicon
            app.dockMenu = popupMenu
        }
        splashScreen.updateStatus("Main menu initialized")
    }

    private fun getPopupMenuItems(): List<PussycatMenuItemDefinition> {
        val list = mutableListOf<PussycatMenuItemDefinition>()
        for (command in COMMAND.list) {
            when (command) {
                COMMAND.UNKNOWN -> {
                }
                else -> {
                    val action = ActionListener {
                        when (command) {
                            COMMAND.FILESYSTEM -> {
                                // TODO: Open file ...
                                println("Open file ...")
                            }
                            COMMAND.STOP -> {
                                try {
                                    server.stop()
                                } catch (e: Exception) {
                                    logger.e(TAG, "${Labels.ERROR}: $e")
                                }
                                execute(command)
                                splashScreen.close()
                            }
                            else -> {
                                execute(command)
                            }
                        }
                    }
                    val menuItem = PussycatMenuItemDefinition(command.value, action)
                    list.add(menuItem)
                }
            }
        }
        return list
    }

}