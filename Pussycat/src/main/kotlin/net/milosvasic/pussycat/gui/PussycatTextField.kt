package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import net.milosvasic.pussycat.gui.theme.font.FONT_WEIGHT
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.RenderingHints
import javax.swing.JTextField


class PussycatTextField(val theme: Theme) : JTextField() {

    var hint: PussycatTextFieldHint? = null
        get() = field
        set(value) {
            field = value
            toolTipText = value?.getHint()
        }

    init {
        isOpaque = true
        toolTipText = hint?.getHint()
        border = theme.getBorder(this)
        background = theme.getColor(TYPE.BASE, INTENSITY.LIGHT)
        foreground = theme.getTextColor(TYPE.BASE, INTENSITY.MEDIUM)
        caretColor = theme.getTextColor(TYPE.BASE, INTENSITY.MEDIUM)
        font = theme.getFont(FONT_WEIGHT.REGULAR, theme.getFontSize())
    }

    override fun paintComponent(g: Graphics?) {
        val g2 = g as Graphics2D
        g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_GASP)
        super.paintComponent(g2)
    }

    override fun paintBorder(g: Graphics?) {
        val g2 = g as Graphics2D
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        super.paintBorder(g2)
    }

}