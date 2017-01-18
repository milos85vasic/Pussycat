package net.milosvasic.pussycat.gui.menu

import net.milosvasic.pussycat.gui.themes.Theme
import net.milosvasic.pussycat.gui.themes.color.INTENSITY
import net.milosvasic.pussycat.gui.themes.color.TYPE
import java.awt.Font
import java.awt.Graphics
import javax.swing.JMenu
import java.awt.Font.BOLD
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder


class PussycatMenu(val theme: Theme, title: String) : JMenu(title) {

    init {
        isOpaque = true
        background = theme.getColor(TYPE.BASE, INTENSITY.MEDIUM)
        foreground = theme.getTextColor(TYPE.BASE, INTENSITY.MEDIUM)
        border = CompoundBorder(border, EmptyBorder(5, 5, 5, 5))
        isVisible = true
        update(graphics)
    }

//    override fun paintComponent(g: Graphics) {
//        super.paintComponent(g)
//        g.color = theme.getColor(TYPE.BASE, INTENSITY.MEDIUM)
//        g.fillRect(0, 0, width, height)
//        g.font = Font.get
//        g.color = theme.getTextColor(TYPE.BASE, INTENSITY.MEDIUM)
//        g.drawString(text, 10, 10)
//    }
//
//    override fun paint(g: Graphics?) {
//        super.paint(g)
//
//    }

}