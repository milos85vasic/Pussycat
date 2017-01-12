package net.milosvasic.pussycat.android.gui

import net.milosvasic.pussycat.application.ApplicationInformation
import javax.swing.JWindow
import java.awt.*


class PussycatSplashScreen(information: ApplicationInformation, owner: Frame?, val callback: OnSplashComplete) : JWindow(owner) {

    val splashWidth = 640
    val splashHeight = 339

    init {
        val screenSize = Toolkit.getDefaultToolkit().screenSize

        setLocation(
                (screenSize.width / 2) - (splashWidth / 2),
                (screenSize.height / 2) - (splashHeight / 2)
        )

        setSize(splashWidth, splashHeight)
        val splashPanel = PussycatSplashPanel(splashWidth, splashHeight)
        add(splashPanel)
        splashPanel.isVisible = true
    }

    override fun setVisible(b: Boolean) {
        super.setVisible(b)
        Thread(
                Runnable {
                    Thread.sleep(3000)
                    callback.onComplete(true)
                }
        ).start()
    }
}