package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.application.ApplicationInformation
import javax.swing.JWindow
import java.awt.*
import java.awt.event.WindowEvent
import java.awt.image.BufferedImage
import java.util.concurrent.atomic.AtomicBoolean
import javax.imageio.ImageIO
import javax.swing.JLabel
import javax.swing.SwingConstants
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder


class PussycatSplashScreen(information: ApplicationInformation, owner: Frame?, val callback: OnSplashComplete) : PussycatAboutDialogAbstract(information, owner) {

    var progress = ""
    private var status = "Loading"
    private val finished = AtomicBoolean()

    fun start() {
        isVisible = true
        finished.set(false)
        Thread(
                Runnable {
                    while (!finished.get()) {
                        progress += " ."
                        updateFooterText()
                        Thread.sleep(500)
                    }
                    callback.onComplete(true)
                }
        ).start()
    }

    fun finish() {
        finished.set(true)
        close()
    }

    fun updateStatus(newStatus: String) {
        status = newStatus
        updateFooterText()
    }

    private fun updateFooterText() {
        if (footer is JLabel) {
            footer.text = "<html><font color='white'>$status$progress</font></html>"
        }
    }

    override fun generateFooter(): JLabel {
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