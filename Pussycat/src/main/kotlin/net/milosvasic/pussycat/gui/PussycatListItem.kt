package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import java.awt.Color
import javax.swing.JLabel

class PussycatListItem(val theme: Theme, val index: Int, val color: Color? = null) : JLabel() {

    init {
        isOpaque = true
        val intensity: INTENSITY
        if (index % 2 == 0) {
            intensity = INTENSITY.DARK
        } else {
            intensity = INTENSITY.MEDIUM
        }
        background = theme.getColor(TYPE.BASE, intensity)
        if (color != null) {
            foreground = color
        } else {
            foreground = theme.getTextColor(TYPE.BASE, intensity)
        }
        border = theme.getBorder(this)
    }

    fun append(text: String, minEms: Int): PussycatListItem {
        this.text += text
        val spaces = minEms - text.length
        if (spaces > 0) {
            for (x in 0..spaces) {
                this.text += "_"
            }
        }
        return this
    }

}