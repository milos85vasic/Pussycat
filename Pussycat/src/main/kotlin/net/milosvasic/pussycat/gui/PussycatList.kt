package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import net.milosvasic.pussycat.gui.theme.font.FONT_WEIGHT
import java.awt.*
import java.util.concurrent.CopyOnWriteArrayList
import javax.swing.*
import javax.swing.table.AbstractTableModel


abstract class PussycatList<T>(val items: CopyOnWriteArrayList<T>, val theme: Theme, listModel: PussycatListModel<T> = PussycatListModel(items)) : JTable(listModel) {

    init {
        isOpaque = true
        background = theme.getColor(TYPE.BASE, INTENSITY.DARK)
        foreground = theme.getTextColor(TYPE.BASE, INTENSITY.MEDIUM)
        font = theme.getFont(FONT_WEIGHT.REGULAR).deriveFont(theme.getFontSize())
        fillsViewportHeight = true
    }

//    override fun paintComponent(g: Graphics?) {
//        border = theme.getBorder(this)
//        val g2 = g as Graphics2D
//        g2.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_GASP)
//        super.paintComponent(g)
//    }

    class PussycatListModel<T>(val items: CopyOnWriteArrayList<T>) : AbstractTableModel() {

        override fun getRowCount(): Int {
            return items.count()
        }

        override fun getColumnCount(): Int {
            return 1
        }

        override fun getValueAt(rowIndex: Int, columnIndex: Int): String {
//            return items[rowIndex]
            return "ZZzzzzz"
        }

    }

}