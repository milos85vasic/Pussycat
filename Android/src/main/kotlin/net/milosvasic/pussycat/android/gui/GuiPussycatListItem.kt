package net.milosvasic.pussycat.android.gui

import net.milosvasic.pussycat.android.data.AndroidLogCatMessage
import net.milosvasic.pussycat.gui.PussycatListItem
import net.milosvasic.pussycat.gui.theme.Theme


class GuiPussycatListItem(theme: Theme, value: AndroidLogCatMessage, index: Int) : PussycatListItem<AndroidLogCatMessage>(theme, value, index) {

    override fun getColumns(): Int {
        return 7
    }

    override fun getRows(): Int {
        return 1
    }

}