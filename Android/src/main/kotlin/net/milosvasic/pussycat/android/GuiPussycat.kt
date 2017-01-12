package net.milosvasic.pussycat.android

import com.android.ddmlib.Log
import net.milosvasic.pussycat.android.data.AndroidLogCatMessage
import net.milosvasic.pussycat.android.gui.OnSplashComplete
import net.milosvasic.pussycat.android.gui.PussycatSplashScreen
import net.milosvasic.pussycat.application.ApplicationInformation
import com.apple.eawt.Application;
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import javax.swing.JFrame


class GuiPussycat(information: ApplicationInformation) : AndroidPussycat() {

    val mainFrame = JFrame()
    private var favicon: BufferedImage? = null

    val splashScreenCallback: OnSplashComplete = object : OnSplashComplete {
        override fun onComplete(success: Boolean) {
            splashScreen.isVisible = false
            mainFrame.isVisible = true
        }
    }

    val splashScreen = PussycatSplashScreen(information, mainFrame, splashScreenCallback)

    init {
        favicon = ImageIO.read(javaClass.classLoader.getResourceAsStream("icons/Favicon.png"))
        mainFrame.extendedState = JFrame.MAXIMIZED_BOTH
        mainFrame.isUndecorated = true
        val osString = System.getProperty("os.name").toLowerCase()
        if (osString.contains("mac")) {
            val app = Application.getApplication()
            app.dockIconImage = favicon
        } else if (osString.contains("linux")) {

        } else if (osString.contains("windows")) {

        }
    }

    override fun start(args: Array<String>) {
        splashScreen.start()
        initGui()
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
                    Thread.sleep(15 * 1000) // TODO: Dynamic part to be implemented.
                    splashScreen.finish()
                }
        ).start()
    }

}