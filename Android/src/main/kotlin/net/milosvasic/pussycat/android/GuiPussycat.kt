package net.milosvasic.pussycat.android

import com.android.ddmlib.Log
import net.milosvasic.pussycat.android.data.AndroidLogCatMessage
import net.milosvasic.pussycat.android.gui.OnSplashComplete
import net.milosvasic.pussycat.android.gui.PussycatSplashScreen
import net.milosvasic.pussycat.application.ApplicationInformation
import javax.swing.JFrame


class GuiPussycat(information: ApplicationInformation) : AndroidPussycat() {

    val mainFrame = JFrame()

    val splashScreenCallback: OnSplashComplete = object : OnSplashComplete {
        override fun onComplete(success: Boolean) {
            splashScreen.isVisible = false
            mainFrame.isVisible = true
        }
    }

    val splashScreen = PussycatSplashScreen(information, mainFrame, splashScreenCallback)

    init {
        mainFrame.extendedState = JFrame.MAXIMIZED_BOTH
        mainFrame.isUndecorated = true
    }

    override fun start(args: Array<String>) {
        splashScreen.isVisible = true
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

}