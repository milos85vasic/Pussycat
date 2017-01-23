package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import java.awt.BorderLayout
import javax.swing.JPanel

class PussycatContent(val theme: Theme) : JPanel() {

    init {
        layout = BorderLayout()
        setSize(width, 1200)
        isOpaque = true
        background = theme.getColor(TYPE.BASE, INTENSITY.DARK)
        border = theme.getBorder(this)
    }

}