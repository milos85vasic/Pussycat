package net.milosvasic.pussycat.android.gui

import net.milosvasic.pussycat.android.data.AndroidLogCatMessage
import net.milosvasic.pussycat.gui.PussycatList
import net.milosvasic.pussycat.gui.PussycatListItem
import net.milosvasic.pussycat.gui.theme.Theme


class GuiPussycatList(theme: Theme): PussycatList<AndroidLogCatMessage>(theme) {

    override fun getListItem(index: Int, value: AndroidLogCatMessage): PussycatListItem<AndroidLogCatMessage> {
        return GuiPussycatListItem(theme, value, index)
    }

}