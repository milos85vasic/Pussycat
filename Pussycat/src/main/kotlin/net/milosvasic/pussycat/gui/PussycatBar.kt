package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.themes.Themable
import net.milosvasic.pussycat.gui.themes.Theme
import net.milosvasic.pussycat.gui.themes.color.INTENSITY
import net.milosvasic.pussycat.gui.themes.color.TYPE
import java.awt.BorderLayout
import java.awt.Dimension
import javax.swing.JPanel
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder


open class PussycatBar(width: Int, height: Int) : JPanel(), Themable {

    init {
        preferredSize = Dimension(width, height)
        layout = BorderLayout()
    }

    override fun apply(theme: Theme?) {
        isOpaque = true
        background = theme?.getColor(TYPE.BASE, INTENSITY.MEDIUM)
        border = CompoundBorder(border, EmptyBorder(0, 0, 0, 0))
    }

}