package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import java.awt.Color
import java.awt.Graphics
import java.awt.GridLayout
import javax.swing.JLabel
import javax.swing.JPanel


class PussycatListItem(val title: String, val theme: Theme, val index: Int) : JLabel(title) {

    init {
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
    }

}