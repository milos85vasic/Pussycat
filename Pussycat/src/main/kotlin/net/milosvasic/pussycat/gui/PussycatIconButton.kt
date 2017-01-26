package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import java.awt.Image
import java.util.*
import javax.swing.ImageIcon
import javax.swing.JButton


class PussycatIconButton(val size: Int, val theme: Theme, val icons: HashMap<Int, Image>) : JButton() {

    init {
        setSize(size, size)
        isOpaque = true
        background = theme.getColor(TYPE.BASE, INTENSITY.MEDIUM)
        border = theme.getBorder(this)
        icon = ImageIcon(icons[0])
    }

    fun setState(state: Int) {
        icon = ImageIcon(icons[state])
    }

    enum class STATE(val value: Int) {
        DEFAULT(0),
        ACTIVE(1),
        INACTIVE(2),
        ENABLED(3),
        DISABLED(4)
    }

}