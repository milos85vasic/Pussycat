package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.themes.Themable
import net.milosvasic.pussycat.gui.themes.Theme
import net.milosvasic.pussycat.gui.themes.color.INTENSITY
import net.milosvasic.pussycat.gui.themes.color.TYPE
import java.awt.*
import javax.swing.JMenu
import javax.swing.JMenuBar
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder


abstract class PussycatMenuBar(val theme: Theme, width: Int, height: Int) : JMenuBar(), Themable {

    init {
        preferredSize = Dimension(width, height)
    }

    override fun apply(theme: Theme?) {
        isOpaque = true
        background = theme?.getColor(TYPE.BASE, INTENSITY.MEDIUM)
        border = CompoundBorder(border, EmptyBorder(0, 0, 0, 0))
    }

    override fun paintComponent(g: Graphics?) {
        super.paintComponent(g)
        if (isOpaque && g != null) {
            g.color = background
            g.fillRect(0, 0, width, height)
        }
    }

    override fun paintBorder(g: Graphics?) {
        super.paintBorder(g)
        if (isOpaque && g != null) {
            g.color = background
            g.fillRect(0, height - 1, width, 1)
        }
    }

    override fun getMinimumSize(): Dimension {
        return preferredSize
    }

    override fun add(comp: Component?): Component {
        if (comp is Themable) {
            comp.apply(theme)
        }
        return super.add(comp)
    }

    override fun add(c: JMenu?): JMenu {
        if (c is Themable) {
            c.apply(theme)
        }
        return super.add(c)
    }

    override fun add(popup: PopupMenu?) {
        if (popup is Themable) {
            popup.apply(theme)
        }
        super.add(popup)
    }

    override fun add(comp: Component, constraints: Any?) {
        if (comp is Themable) {
            comp.apply(theme)
        }
        super.add(comp, constraints)
    }

    override fun add(comp: Component?, index: Int): Component {
        if (comp is Themable) {
            comp.apply(theme)
        }
        return super.add(comp, index)
    }

    override fun add(name: String?, comp: Component?): Component {
        if (comp is Themable) {
            comp.apply(theme)
        }
        return super.add(name, comp)
    }

    override fun add(comp: Component?, constraints: Any?, index: Int) {
        if (comp is Themable) {
            comp.apply(theme)
        }
        super.add(comp, constraints, index)
    }

}