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
    var footer: JLabel? = null
    private var status = "Loading"
    private val finished = AtomicBoolean()

    fun start() {
        open()
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
        footer?.text = "<html><font color='white'>$status$progress</font></html>"
    }

    override fun generateFooter(): JLabel? {
        footer = JLabel()
        footer?.isOpaque = true
        footer?.background = Color.BLACK
        footer?.text = "<html><font color='white'>Loading</font></html>"
        footer?.horizontalAlignment = SwingConstants.LEFT
        footer?.border = CompoundBorder(footer?.border, EmptyBorder(10, 10, 10, 10))
        footer?.isVisible = true
        return footer
    }

}