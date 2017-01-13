package net.milosvasic.pussycat.android

import com.android.ddmlib.Log
import net.milosvasic.pussycat.android.data.AndroidLogCatMessage
import net.milosvasic.pussycat.gui.OnSplashComplete
import net.milosvasic.pussycat.gui.PussycatSplashScreen
import net.milosvasic.pussycat.application.ApplicationInformation
import com.apple.eawt.Application
import net.milosvasic.pussycat.Messages
import net.milosvasic.pussycat.core.COMMAND
import net.milosvasic.pussycat.events.EVENT
import net.milosvasic.pussycat.events.Events
import net.milosvasic.pussycat.os.OS
import net.milosvasic.pussycat.utils.Gui
import java.awt.MenuItem
import java.awt.PopupMenu
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import javax.swing.JFrame
import javax.swing.WindowConstants


class GuiPussycat(information: ApplicationInformation) : AndroidPussycat() {

    val mainFrame = JFrame()
    private var favicon: BufferedImage? = null

    val splashScreenCallback: OnSplashComplete = object : OnSplashComplete {
        override fun onComplete(success: Boolean) {
            Gui.close(splashScreen)
            Gui.show(mainFrame)
        }
    }

    val splashScreen = PussycatSplashScreen(information, mainFrame, splashScreenCallback)

    init {
        favicon = ImageIO.read(javaClass.classLoader.getResourceAsStream("icons/Favicon.png"))
        mainFrame.extendedState = JFrame.MAXIMIZED_BOTH
        mainFrame.title = "${information.name} V${information.version} by ${information.author}"
    }

    override fun start(args: Array<String>) {
        if (configuration.exitOnStop) {
            mainFrame.defaultCloseOperation = WindowConstants.EXIT_ON_CLOSE
        }
        splashScreen.start()

        val hook = Thread(Runnable {
            execute(COMMAND.STOP)
        })

        Runtime.getRuntime().addShutdownHook(hook)

        // TODO: Take into account params --- live vs fs etc.
        initialize()
    }

    override fun status() {

    }

    override fun clear() {

    }

    override fun printLine(text: String) {

    }

    override fun printLine(text: String, logLevel: Log.LogLevel) {

    }

    override fun printLine(line: AndroidLogCatMessage) {

    }

    override fun getPrintableLogLevelValue(): String {
        return ""
    }

    override fun getPrintableFilterValue(): String {
        return ""
    }

    private fun initialize() {
        Thread(
                Runnable {
                    val osString = OS.getOS()
                    initPopupMenu(osString)
                    initMainIcon()
                    splashScreen.updateStatus("Loading complete")
                    splashScreen.finish()
                }
        ).start()
    }

    private fun initMainIcon() {
        splashScreen.setIconImage(favicon)
        mainFrame.iconImage = favicon
        splashScreen.updateStatus("Main icon set")
    }

    private fun initPopupMenu(osString: String) {
        val popupMenu = generateApplicationPopupMenu()
        if (osString.contains(OS.MACOS)) {
            val app = Application.getApplication()
            app.dockIconImage = favicon
            app.dockMenu = popupMenu
        } else if (osString.contains(OS.LINUX)) {

        } else if (osString.contains(OS.WINDOWS)) {

        }
        splashScreen.updateStatus("Main menu initialized")
    }

    private fun generateApplicationPopupMenu(): PopupMenu {
        // TODO: Refine this a bit and complete the implementation.
        val menu = PopupMenu()
        for (command in COMMAND.list) {
            when (command) {
                COMMAND.UNKNOWN -> {
                }
                else -> {
                    val menuItem = MenuItem()
                    menuItem.label = command.value
                    menuItem.addActionListener {
                        when (command) {
                            COMMAND.FILESYSTEM -> {
                                // TODO: Open file ...
                                println("Open file ...")
                            }
                            COMMAND.STOP -> {
                                execute(command)
                                Gui.close(splashScreen)
                                Gui.close(mainFrame)
                            }
                            else -> {
                                execute(command)
                            }
                        }
                    }
                    menu.add(menuItem)
                }
            }
        }
        return menu
    }

}
