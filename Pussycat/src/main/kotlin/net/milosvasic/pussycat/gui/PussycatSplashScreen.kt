package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.application.ApplicationInformation
import javax.swing.JWindow
import java.awt.*
import java.util.concurrent.atomic.AtomicBoolean
import javax.swing.JLabel
import javax.swing.SwingConstants
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder


class PussycatSplashScreen(information: ApplicationInformation, owner: Frame?, val callback: OnSplashComplete) : JWindow(owner) {

    val footer: JLabel
    val splashWidth = 640
    val splashHeight = 389
    private val finished = AtomicBoolean()

    init {
        val screenSize = Toolkit.getDefaultToolkit().screenSize

        setLocation(
                (screenSize.width / 2) - (splashWidth / 2),
                (screenSize.height / 2) - (splashHeight / 2)
        )

        setSize(splashWidth, splashHeight)
        val body = PussycatSplashPanel(splashWidth, splashHeight)
        val header = generateHeader(information)
        footer = generateFooter()
        add(body)
        add(header, BorderLayout.NORTH)
        add(footer, BorderLayout.SOUTH)
        body.isVisible = true
    }

    fun start() {
        isVisible = true
        finished.set(false)
        Thread(
                Runnable {
                    var points = ""
                    while (!finished.get()) {
                        points += " ."
                        footer.text = "<html><font color='white'>Loading$points</font></html>"
                        Thread.sleep(500)
                    }
                    callback.onComplete(true)
                }
        ).start()
    }

    fun finish() {
        finished.set(true)
    }

    private fun generateHeader(information: ApplicationInformation): JLabel {
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
        return splashLabel
    }

    private fun generateFooter(): JLabel {
        val splashLabel = JLabel()
        splashLabel.isOpaque = true
        splashLabel.background = Color.BLACK
        splashLabel.text = "<html><font color='white'>Loading</font></html>"
        splashLabel.horizontalAlignment = SwingConstants.LEFT
        splashLabel.border = CompoundBorder(splashLabel.border, EmptyBorder(10, 10, 10, 10))
        splashLabel.isVisible = true
        return splashLabel
    }

}