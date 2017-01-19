package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.themes.Themable
import net.milosvasic.pussycat.gui.themes.Theme
import net.milosvasic.pussycat.gui.themes.UI_INTERACTION_TYPE
import net.milosvasic.pussycat.gui.themes.color.INTENSITY
import net.milosvasic.pussycat.gui.themes.color.TYPE
import net.milosvasic.pussycat.gui.themes.font.FONT_WEIGHT
import java.awt.Graphics
import java.awt.event.MouseEvent
import java.awt.event.MouseListener
import javax.swing.JMenu


class PussycatMenu(val title: String) : JMenu(title), Themable, MouseListener {

    private var theme: Theme? = null

    init {
        addMouseListener(this)
    }

    override fun apply(theme: Theme) {
        this.theme = theme
        isOpaque = true
        background = theme.getColor(TYPE.BASE, INTENSITY.MEDIUM)
        foreground = theme.getTextColor(TYPE.BASE, INTENSITY.MEDIUM)
        font = theme.getFont(FONT_WEIGHT.THIN).deriveFont(theme.getFontSize())
    }

    override fun paintComponent(g: Graphics?) {
        super.paintComponent(g)
        if (isOpaque && g != null) {
            g.color = background
            g.fillRect(0, 0, width, height)
            g.color = foreground
            g.font = font
            g.drawString(title, font.size, font.size)
        }
    }

    override fun mouseEntered(e: MouseEvent?) {
        foreground = theme?.getTextColor(TYPE.BASE, INTENSITY.MEDIUM, UI_INTERACTION_TYPE.HOVER)
        repaint()
    }

    override fun mouseExited(e: MouseEvent?) {
        foreground = theme?.getTextColor(TYPE.BASE, INTENSITY.MEDIUM)
        repaint()
    }

    override fun mouseClicked(e: MouseEvent?) {
        return
    }

    override fun mouseReleased(e: MouseEvent?) {
        return
    }

    override fun mousePressed(e: MouseEvent?) {
        return
    }

}