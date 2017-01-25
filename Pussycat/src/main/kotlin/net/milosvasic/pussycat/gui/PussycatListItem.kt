package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import java.awt.Color
import java.awt.Graphics
import java.awt.GridLayout
import javax.swing.JPanel


abstract class PussycatListItem<T>(val theme: Theme, val value: T, val index: Int) : JPanel() {

    init {
        isOpaque = true
        if (index % 2 == 0) {
            background = theme.getColor(TYPE.BASE, INTENSITY.DARK)
        } else {
            background = theme.getColor(TYPE.BASE, INTENSITY.MEDIUM)
        }
    }

    override fun paintComponent(g: Graphics?) {
        layout = GridLayout(getColumns(), getRows())
        border = theme.getBorder(this)
        foreground = getTextColor(value)
        super.paintComponent(g)
    }

    abstract fun getColumns(): Int

    abstract fun getRows(): Int

    abstract fun getTextColor(value: T): Color

}