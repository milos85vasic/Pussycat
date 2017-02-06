package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import java.awt.Dimension
import java.awt.Image
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.util.*
import javax.imageio.ImageIO
import javax.swing.ImageIcon
import javax.swing.JButton


class PussycatIconButton private constructor(val size: Int, val theme: Theme, val icons: HashMap<Int, Image>) : JButton() {

    companion object {
        fun create(theme: Theme, definition: PussycatIconButtonDefinition): PussycatIconButton {
            val size = definition.size
            val icons = HashMap<Int, Image>()
            val clazz = PussycatIconButtonDefinition::class.java
            val iconDefault = ImageIO.read(clazz.classLoader.getResourceAsStream("${theme.getName()}/icons/${definition.defaultIcon}.png"))
            val iconActive = ImageIO.read(clazz.classLoader.getResourceAsStream("${theme.getName()}/icons/${definition.activeIcon}.png"))
            val disabledActive = ImageIO.read(clazz.classLoader.getResourceAsStream("${theme.getName()}/icons/${definition.disabledIcon}.png"))
            val iconDefaultResized = iconDefault.getScaledInstance(size, size, Image.SCALE_SMOOTH)
            val iconActiveResized = iconActive.getScaledInstance(size, size, Image.SCALE_SMOOTH)
            val iconDisabledResized = disabledActive.getScaledInstance(size, size, Image.SCALE_SMOOTH)
            icons.put(STATE.DEFAULT.value, iconDefaultResized)
            icons.put(STATE.ACTIVE.value, iconActiveResized)
            icons.put(STATE.DISABLED.value, iconDisabledResized)
            val button = PussycatIconButton(size, theme, icons)
            button.addActionListener(definition.action)
            button.toolTipText = definition.toolTip
            return button
        }
    }

    init {
        preferredSize = Dimension(size, size)
        isOpaque = true
        background = theme.getColor(TYPE.BASE, INTENSITY.MEDIUM)
        border = theme.getBorder(this)
        icon = ImageIcon(icons[0])
    }

    fun setState(state: STATE) {
        val position = state.value
        icon = ImageIcon(icons[position])
        isEnabled = state != STATE.DISABLED
    }

    enum class STATE(val value: Int) {
        DEFAULT(0),
        ACTIVE(1),
        INACTIVE(2),
        ENABLED(3),
        DISABLED(4)
    }

}