package net.milosvasic.pussycat.android

import com.android.ddmlib.Log
import net.milosvasic.pussycat.android.data.AndroidLogCatMessage
import net.milosvasic.pussycat.application.ApplicationInformation
import com.apple.eawt.Application
import net.milosvasic.pussycat.android.command.ANDROID_COMMAND
import net.milosvasic.pussycat.android.gui.GuiPussycatListItemFactory
import net.milosvasic.pussycat.android.gui.GuiPussycatMainWindow
import net.milosvasic.pussycat.content.Messages
import net.milosvasic.pussycat.core.COMMAND
import net.milosvasic.pussycat.events.EVENT
import net.milosvasic.pussycat.gui.*
import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.listeners.Listener
import net.milosvasic.pussycat.logging.LOG_TYPE
import net.milosvasic.pussycat.os.OS
import java.awt.MenuItem
import java.awt.PopupMenu
import java.awt.event.ActionListener
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import javax.swing.WindowConstants


class GuiPussycat(information: ApplicationInformation, val theme: Theme) : AndroidPussycat() {

    private var favicon: BufferedImage? = null
    val mainWindow = GuiPussycatMainWindow(information, theme)
    val pussycatListItems = mutableListOf<PussycatListItem>()

    val splashScreenCallback: OnSplashComplete = object : OnSplashComplete {
        override fun onComplete(success: Boolean) {
            mainWindow.SUBSCRIPTIONS.STATUS.subscribe(mainWindowStatusListener)
            mainWindow.open()
        }
    }

    val mainWindowStatusListener = object : Listener<Boolean> {
        override fun onEvent(value: Boolean?) {
            if (value != null && value) {
                Thread(Runnable {
                    Thread.currentThread().priority = Thread.MAX_PRIORITY
                    mainWindow.setBusy(true)
                    for (pussycatListItem in pussycatListItems) {
                        mainWindow.addContentItem(pussycatListItem)
                    }
                    mainWindow.setBusy(false)
                }).start()
            }
        }
    }

    val splashScreen = PussycatSplashScreen(information, theme, mainWindow, splashScreenCallback)

    init {
        favicon = ImageIO.read(javaClass.classLoader.getResourceAsStream("icons/Favicon.png"))
    }

    val eventsListener = object : Listener<EVENT> {
        override fun onEvent(value: EVENT?) {
            if (value == EVENT.STOP) {
                SUBSCRIPTIONS.EVENTS.unsubscribe(this)
                SUBSCRIPTIONS.FILESYSTEM_LOADING_PROGRESS.unsubscribe(filesystemProgressListener)
                mainWindow.SUBSCRIPTIONS.STATUS.unsubscribe(mainWindowStatusListener)
            }
        }
    }

    val filesystemProgressListener = object : Listener<Double> {
        override fun onEvent(value: Double?) {
            val s = String.format("%.0f", value)
            splashScreen.updateStatus("${Messages.PARSING}: $s%")
        }
    }

    override fun start(args: Array<String>) {
        if (configuration.exitOnStop) {
            mainWindow.defaultCloseOperation = WindowConstants.EXIT_ON_CLOSE
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

    override fun status() {

    }

    override fun clear() {

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
        if (mainWindow.isReady() && !mainWindow.isBusy()) {
            getPussycatListItems(line)
        }
    }

    override fun getPrintableLogLevelValue(): String {
        return ""
    }

    override fun getPrintableFilterValue(): String {
        return ""
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
                    getPussycatListItems()
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
        mainWindow.addPopUpMenuItems(items)
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
                                execute(command)
                                splashScreen.close()
                                mainWindow.close()
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

    private fun getPussycatListItems() {
        val count = data.get().size
        for (x in 0..count - 1) {
            val item = data.get()[x]
            pussycatListItems.addAll(getPussycatListItems(item))
            val value: Double = (x * 100.0) / count
            val s = String.format("%.0f", value)
            splashScreen.updateStatus("${Messages.GENERATING_UI}: $s%")
        }
    }

    private fun getPussycatListItems(line: AndroidLogCatMessage): List<PussycatListItem> {
        val pussycatListItems = mutableListOf<PussycatListItem>()

        val index = data.get().indexOf(line)

        val color = when (line.logLevel) {
            Log.LogLevel.DEBUG -> {
                theme.getTextColor(LOG_TYPE.DEBUG)
            }
            Log.LogLevel.INFO -> {
                theme.getTextColor(LOG_TYPE.INFORMATION)
            }
            Log.LogLevel.WARN -> {
                theme.getTextColor(LOG_TYPE.WARNING)
            }
            Log.LogLevel.ERROR -> {
                theme.getTextColor(LOG_TYPE.ERROR)
            }
            else -> {
                theme.getTextColor(LOG_TYPE.VERBOSE)
            }
        }

        val itemPairs = mutableListOf<Pair<String, Int>>()
        itemPairs.add(Pair(line.time, AndroidLogCatMessage.LENGTHS.SPACING_DEFAULT))
        itemPairs.add(Pair("${line.pid}", AndroidLogCatMessage.LENGTHS.SPACING_SHORT))
        itemPairs.add(Pair("${line.tid}", AndroidLogCatMessage.LENGTHS.SPACING_SHORT))
        itemPairs.add(Pair(line.appName, AndroidLogCatMessage.LENGTHS.SPACING_LONG))
        itemPairs.add(Pair(line.tag, AndroidLogCatMessage.LENGTHS.SPACING_LONG))
        itemPairs.add(Pair(line.msg, AndroidLogCatMessage.LENGTHS.NO_SPACING_APPLIED))

        val item = GuiPussycatListItemFactory.create(
                theme,
                itemPairs,
                index,
                color
        )

        pussycatListItems.add(item)

        for (stacktraceItem in line.getStacktrace()) {
            val builder = StringBuilder()
            val stacktraceListItemPairs = mutableListOf<Pair<String, Int>>()
            for (itemPair in itemPairs) {
                val spaces = itemPair.second
                for (x in 0..spaces) {
                    builder.append("  ")
                }
            }
            for (x in 0..AndroidLogCatMessage.LENGTHS.SPACING_DEFAULT) {
                builder.append("  ")
            }
            builder.append(stacktraceItem)
            stacktraceListItemPairs.add(Pair(builder.toString(), AndroidLogCatMessage.LENGTHS.NO_SPACING_APPLIED))
            val stacktraceListItem = GuiPussycatListItemFactory.create(
                    theme,
                    stacktraceListItemPairs,
                    index,
                    color
            )
            pussycatListItems.add(stacktraceListItem)
        }

        return pussycatListItems
    }

}
