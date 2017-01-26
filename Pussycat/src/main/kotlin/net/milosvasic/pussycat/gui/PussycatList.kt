package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import net.milosvasic.pussycat.gui.theme.font.FONT_WEIGHT
import java.awt.*
import javax.swing.*


class PussycatList(val theme: Theme) : JPanel() {

    init {
        isOpaque = true
        layout = BoxLayout(this, BoxLayout.PAGE_AXIS)
        background = theme.getColor(TYPE.BASE, INTENSITY.DARK)
        foreground = theme.getTextColor(TYPE.BASE, INTENSITY.MEDIUM)
        font = theme.getFont(FONT_WEIGHT.REGULAR).deriveFont(theme.getFontSize())
    }

    override fun paintComponent(g: Graphics?) {
        if (border != theme.getBorder(this)) {
            border = theme.getBorder(this)
        }
        val g2 = g as Graphics2D
        g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_GASP)
        super.paintComponent(g)
    }


}