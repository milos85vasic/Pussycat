package net.milosvasic.pussycat.android.gui

import net.milosvasic.pussycat.gui.PussycatListItem
import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import java.awt.Color


class GuiPussycatListItemFactory {

    companion object {
        fun create(theme: Theme, items: List<Pair<String, Int>>, index: Int, textColor: Color = theme.getTextColor(TYPE.BASE, INTENSITY.DARK)): PussycatListItem {
            val item = PussycatListItem(theme, index, textColor)
            for (line in items) {
                item.append(line.first, line.second)
            }
            return item
        }
    }

}