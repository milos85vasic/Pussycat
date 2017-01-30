package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.gui.theme.UI_INTERACTION_TYPE
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import net.milosvasic.pussycat.gui.theme.font.FONT_WEIGHT
import java.awt.Dimension
import java.awt.Graphics
import java.awt.event.MouseEvent
import java.awt.event.MouseListener
import javax.swing.JLabel
import java.awt.Graphics2D
import java.awt.RenderingHints


class PussycatMenu(val theme: Theme, val title: String) : JLabel(title), MouseListener {

    private val popupMenu = PussycatPopupMenu(theme)

    init {
        val preferredWidth = Math.round(title.length * theme.getFontSize())
        preferredSize = Dimension(preferredWidth, preferredSize.height)
        isOpaque = true
        background = theme.getColor(TYPE.BASE, INTENSITY.MEDIUM)
        foreground = theme.getTextColor(TYPE.BASE, INTENSITY.MEDIUM)
        font = theme.getFont(FONT_WEIGHT.REGULAR, theme.getFontSize())
        border = theme.getBorder(this)
        addMouseListener(this)
    }

    override fun paintComponent(g: Graphics?) {
        val g2 = g as Graphics2D
        g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_GASP)
        super.paintComponent(g2)
        g2.color = background
        g2.fillRect(0, 0, width, height)
        g2.color = foreground
        g2.font = font
        g2.drawString(title, font.size, font.size)
    }

    override fun mouseEntered(e: MouseEvent?) {
        foreground = theme.getTextColor(TYPE.BASE, INTENSITY.MEDIUM, UI_INTERACTION_TYPE.HOVER)
        repaint()
        showPopupMenu()
    }

    override fun mouseExited(e: MouseEvent?) {
        foreground = theme.getTextColor(TYPE.BASE, INTENSITY.MEDIUM)
        repaint()
    }

    fun addMenuItem(item: PussycatMenuItem) {
        popupMenu.add(item)
    }

    override fun mouseClicked(e: MouseEvent?) {
        showPopupMenu()
        return
    }

    override fun mouseReleased(e: MouseEvent?) {
        return
    }

    override fun mousePressed(e: MouseEvent?) {
        return
    }

    private fun showPopupMenu() {
        popupMenu.show(this, 0, this.height + (this.height / 2))
    }

}
