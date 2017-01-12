package net.milosvasic.pussycat.android.gui

import net.milosvasic.pussycat.application.ApplicationInformation
import javax.swing.JWindow
import java.awt.*
import javax.swing.JLabel
import javax.swing.SwingConstants
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder




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
        val splashLabel = JLabel()
        splashLabel.isOpaque = true
        splashLabel.background = Color.BLACK
        val dimension = Dimension(splashWidth / 4, splashHeight / 4)
        splashLabel.minimumSize = dimension
        splashLabel.maximumSize = dimension
        splashLabel.preferredSize = dimension
        splashLabel.text = "<html><font color='white'>${information.name}, Version: ${information.version}<br />Author: ${information.author}<br />Website: ${information.website}</font></html>"
        splashLabel.horizontalAlignment = SwingConstants.RIGHT
        splashLabel.border = CompoundBorder(splashLabel.border, EmptyBorder(10, 10, 10, 10))
        splashLabel.isVisible = true
        add(splashPanel)
        add(splashLabel, BorderLayout.NORTH)
        splashPanel.isVisible = true
    }

    override fun setVisible(b: Boolean) {
        super.setVisible(b)
        Thread(
                Runnable {
                    Thread.sleep(30000)
                    callback.onComplete(true)
                }
        ).start()
    }
}