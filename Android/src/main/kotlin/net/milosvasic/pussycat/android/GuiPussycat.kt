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
    val pussycatListItemFactory = GuiPussycatListItemFactory(theme)
    val pussycatListItemsFactory: PussycatListItemsFactory<AndroidLogCatMessage>

    val splashScreenCallback: OnSplashComplete = object : OnSplashComplete {
        override fun onComplete(success: Boolean) {
            mainWindow.subscriptions.STATUS.subscribe(mainWindowStatusListener)
            mainWindow.open()
        }
    }

    val mainWindowStatusListener = object : Listener<Boolean> {
        override fun onEvent(value: Boolean?) {
            if (value != null && value) {
                Thread(Runnable {
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
        pussycatListItemsFactory = PussycatListItemsFactory(pussycatListItemFactory)
    }

    val eventsListener = object : Listener<EVENT> {
        override fun onEvent(value: EVENT?) {
            if (value == EVENT.STOP) {
                data.events.subscribe(pussycatListItemsFactory)
                SUBSCRIPTIONS.EVENTS.unsubscribe(this)
                SUBSCRIPTIONS.FILESYSTEM_LOADING_PROGRESS.unsubscribe(filesystemProgressListener)
                mainWindow.subscriptions.STATUS.unsubscribe(mainWindowStatusListener)
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
        data.events.subscribe(pussycatListItemsFactory)

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
            // TODO: Change this.
//            val item = getPussycatListItem(line)
//            pussycatListItems.add(item)
//            mainWindow.addContentItem(item)
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

}
