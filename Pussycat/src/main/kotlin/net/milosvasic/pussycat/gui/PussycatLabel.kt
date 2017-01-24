package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.gui.theme.font.FONT_WEIGHT
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.RenderingHints
import javax.swing.JLabel


class PussycatLabel(val title: String, val theme: Theme) : JLabel(title) {

    init {
        isOpaque = true
//        font = theme.getFont(FONT_WEIGHT.REGULAR).deriveFont(theme.getFontSize())
    }

    override fun paintComponent(g: Graphics?) {
//        val g2 = g as Graphics2D
//        g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_GASP)
        super.paintComponent(g)
    }

}