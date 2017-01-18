package net.milosvasic.pussycat.gui

import java.awt.BorderLayout
import java.awt.Dimension
import javax.swing.JPanel


open class PussycatBar(width: Int, height: Int) : JPanel() {

    init {
        preferredSize = Dimension(width, height)
        layout = BorderLayout()
    }

}