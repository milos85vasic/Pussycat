package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import java.awt.BorderLayout
import java.awt.Dimension
import javax.swing.JPanel


class PussycatBar(val theme: Theme, width: Int, height: Int) : JPanel() {

    init {
        preferredSize = Dimension(width, height)
        layout = BorderLayout()
        isOpaque = true
        background = theme.getColor(TYPE.BASE, INTENSITY.MEDIUM)
        border = theme.getBorder(this)
    }

}