package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.application.ApplicationInformation
import java.awt.Color
import javax.swing.JFrame
import javax.swing.JPanel


class PussycatMainWindow(information: ApplicationInformation) : JFrame() {

    init {
        extendedState = JFrame.MAXIMIZED_BOTH
        title = "${information.name} V${information.version} by ${information.author}"
    }

    fun initialize() {
        val panel = JPanel()
        panel.isOpaque = true
        panel.background = Color(60, 63, 65)
        add(panel)
    }

}