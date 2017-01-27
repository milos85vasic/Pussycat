package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import java.awt.Dimension
import java.awt.FlowLayout
import java.awt.Graphics
import javax.swing.JPanel


open class PussycatBar(val theme: Theme, width: Int, height: Int) : JPanel() {

    init {
        preferredSize = Dimension(width, height)
        layout = FlowLayout(FlowLayout.LEFT)
        isOpaque = true
        background = theme.getColor(TYPE.BASE, INTENSITY.MEDIUM)
    }

    override fun paintComponent(g: Graphics?) {
        border = theme.getBorder(this)
        super.paintComponent(g)
    }

}