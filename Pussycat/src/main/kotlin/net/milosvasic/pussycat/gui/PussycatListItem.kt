package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import java.awt.*
import javax.swing.JLabel
import javax.swing.JPanel


class PussycatListItem(val theme: Theme, val index: Int) : JPanel() {

    init {
        layout = FlowLayout(FlowLayout.LEFT)
        isOpaque = true
        val intensity: INTENSITY
        if (index % 2 == 0) {
            intensity = INTENSITY.DARK
        } else {
            intensity = INTENSITY.MEDIUM
        }
        background = theme.getColor(TYPE.BASE, intensity)
        foreground = theme.getTextColor(TYPE.BASE, intensity)
        border = theme.getBorder(this)
        preferredSize = Dimension(width, 10)
    }

    fun append(text: String): PussycatListItem {
        return append(text, foreground)
    }

    fun append(text: String, color: Color): PussycatListItem {
        val item = JLabel(text)
        item.isOpaque = true
        item.background = background
        item.foreground = color
        item.border = theme.getBorder(item)
        item.preferredSize = Dimension(100, 10)
        add(item)
        return this
    }

}