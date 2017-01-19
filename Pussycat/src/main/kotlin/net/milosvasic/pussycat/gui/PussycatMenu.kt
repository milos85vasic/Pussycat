package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.theme.ThemeManager
import net.milosvasic.pussycat.gui.theme.UI_INTERACTION_TYPE
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import net.milosvasic.pussycat.gui.theme.font.FONT_WEIGHT
import java.awt.Graphics
import java.awt.event.MouseEvent
import java.awt.event.MouseListener
import javax.swing.JMenu


class PussycatMenu(val title: String) : JMenu(title), MouseListener {

    init {
        addMouseListener(this)
        isOpaque = true
        val theme = ThemeManager.currentTheme
        background = theme.getColor(TYPE.BASE, INTENSITY.MEDIUM)
        foreground = theme.getTextColor(TYPE.BASE, INTENSITY.MEDIUM)
        font = theme.getFont(FONT_WEIGHT.THIN).deriveFont(theme.getFontSize())
        popupMenu.isOpaque = true
        popupMenu.background = theme.getColor(TYPE.BASE, INTENSITY.MEDIUM)
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
        val theme = ThemeManager.currentTheme
        foreground = theme.getTextColor(TYPE.BASE, INTENSITY.MEDIUM, UI_INTERACTION_TYPE.HOVER)
        repaint()

    }

    override fun mouseExited(e: MouseEvent?) {
        val theme = ThemeManager.currentTheme
        foreground = theme.getTextColor(TYPE.BASE, INTENSITY.MEDIUM)
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