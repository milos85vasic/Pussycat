package net.milosvasic.pussycat.android

import com.android.ddmlib.Log
import net.milosvasic.pussycat.android.data.AndroidLogCatMessage
import net.milosvasic.pussycat.gui.OnSplashComplete
import net.milosvasic.pussycat.gui.PussycatSplashScreen
import net.milosvasic.pussycat.application.ApplicationInformation
import com.apple.eawt.Application
import net.milosvasic.pussycat.core.COMMAND
import net.milosvasic.pussycat.os.OS
import net.milosvasic.pussycat.utils.Gui
import java.awt.MenuItem
import java.awt.PopupMenu
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import javax.swing.JFrame


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
        mainFrame.isUndecorated = true
    }

    override fun start(args: Array<String>) {
        splashScreen.start()
        initGui()
    }

    override fun stop() {
        super.stop()
        Gui.close(splashScreen)
        Gui.close(mainFrame)
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

    private fun initGui() {
        Thread(
                Runnable {
                    val osString = OS.getOS()
                    val popupMenu = generateApplicationPopupMenu()
                    splashScreen.setIconImage(favicon)
                    mainFrame.iconImage = favicon

                    if (osString.contains(OS.MACOS)) {
                        val app = Application.getApplication()
                        app.dockIconImage = favicon
                        app.dockMenu = popupMenu
                    } else if (osString.contains(OS.LINUX)) {

                    } else if (osString.contains(OS.WINDOWS)) {

                    }

                    Thread.sleep(15 * 1000) // TODO: Dynamic part to be implemented.
                    splashScreen.finish()
                }
        ).start()
    }

    private fun generateApplicationPopupMenu(): PopupMenu {
        val menu = PopupMenu()
        for (command in COMMAND.list) {
            if (command in listOf(COMMAND.UNKNOWN)) {
                continue
            }
            val menuItem = MenuItem()
            menuItem.label = command.value
            menuItem.addActionListener {
                execute(command)
            }
            menu.add(menuItem)
        }
        return menu
    }

}
