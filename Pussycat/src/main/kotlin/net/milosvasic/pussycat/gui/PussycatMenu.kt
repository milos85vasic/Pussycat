package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.themes.Themable
import net.milosvasic.pussycat.gui.themes.Theme
import net.milosvasic.pussycat.gui.themes.color.INTENSITY
import net.milosvasic.pussycat.gui.themes.color.TYPE
import net.milosvasic.pussycat.gui.themes.font.FONT_WEIGHT
import java.awt.Graphics
import javax.swing.JMenu
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder


open class PussycatMenu(val title: String) : JMenu(title), Themable {

    override fun apply(theme: Theme) {
        isOpaque = true
        background = theme.getColor(TYPE.BASE, INTENSITY.MEDIUM)
        foreground = theme.getTextColor(TYPE.BASE, INTENSITY.MEDIUM)
        border = CompoundBorder(border, EmptyBorder(0, 0, 0, 0))
        font = theme.getFont(FONT_WEIGHT.THIN).deriveFont(12f)
    }

    override fun paintComponent(g: Graphics?) {
        super.paintComponent(g)
        if (isOpaque && g != null) {
            g.color = background
            g.fillRect(0, 0, width, height)
            g.color = foreground
            g.font = font
            g.drawString(title, 10, 10)
        }
    }

}