package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import java.awt.Color
import javax.swing.JTextPane

class PussycatListItem(val theme: Theme, val index: Int, val color: Color? = null) : JTextPane() {

    companion object {
        val SPACING = "  "
    }

    init {
        isEditable = false
        isOpaque = true
        val intensity: INTENSITY
        if (index % 2 == 0) {
            intensity = INTENSITY.MEDIUM
        } else {
            intensity = INTENSITY.DARK
        }
        background = theme.getColor(TYPE.BASE, intensity)
        if (color != null) {
            foreground = color
        } else {
            foreground = theme.getTextColor(TYPE.BASE, intensity)
        }
        border = theme.getBorder(this)
    }

    fun append(text: String, ems: Int): PussycatListItem {
        var rawText = text
        if (text.length > ems && ems > 0) {
            val suffix = "... "
            rawText = text.substring(0, ems - suffix.length) + suffix
        }
        this.text += rawText
        if(ems > 0) {
            val spaces = (ems + 3) - rawText.length
            if (spaces > 0) {
                for (x in 0..spaces) {
                    this.text += SPACING
                }
            }
        }
        return this
    }

}