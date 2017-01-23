package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import net.milosvasic.pussycat.gui.theme.font.FONT_WEIGHT
import java.awt.*
import javax.swing.*


class PussycatList<T>(val theme: Theme, val listModel: DefaultListModel<T> = DefaultListModel<T>()) : JList<T>(listModel) {

//    private val listRenderer = ListRenderer<T>(theme)

    init {
        isOpaque = true
        background = theme.getColor(TYPE.BASE, INTENSITY.DARK)
        foreground = theme.getTextColor(TYPE.BASE, INTENSITY.MEDIUM)
        font = theme.getFont(FONT_WEIGHT.REGULAR).deriveFont(theme.getFontSize())
        border = theme.getBorder(this)
        selectionMode = ListSelectionModel.MULTIPLE_INTERVAL_SELECTION
        layoutOrientation = JList.VERTICAL
        visibleRowCount = -1
//        cellRenderer = listRenderer
    }

    override fun paintComponent(g: Graphics?) {
        val g2 = g as Graphics2D
        g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_GASP)
        super.paintComponent(g)
    }

    private class ListRenderer<T>(theme: Theme) : JLabel(), ListCellRenderer<T> {

        init {
            isOpaque = true
            background = theme.getColor(TYPE.BASE, INTENSITY.DARK)
            foreground = theme.getTextColor(TYPE.BASE, INTENSITY.MEDIUM)
            font = theme.getFont(FONT_WEIGHT.REGULAR).deriveFont(theme.getFontSize())
            border = theme.getBorder(this)
        }

        override fun getListCellRendererComponent(list: JList<out T>?, value: T, index: Int, isSelected: Boolean, cellHasFocus: Boolean): Component {
            if (isSelected) {
                background = Color.RED
            } else {
                background = Color.GREEN
            }
            return this
        }
    }

}