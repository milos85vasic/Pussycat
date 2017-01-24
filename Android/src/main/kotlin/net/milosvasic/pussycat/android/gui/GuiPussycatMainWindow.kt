package net.milosvasic.pussycat.android.gui


import net.milosvasic.pussycat.android.data.AndroidData
import net.milosvasic.pussycat.android.data.AndroidLogCatMessage
import net.milosvasic.pussycat.application.ApplicationInformation
import net.milosvasic.pussycat.gui.*
import net.milosvasic.pussycat.gui.content.Labels
import net.milosvasic.pussycat.gui.theme.Theme

class GuiPussycatMainWindow(information: ApplicationInformation, theme: Theme) : PussycatMainWindow<AndroidLogCatMessage, AndroidData>(information, theme) {

    private val list = GuiPussycatList(theme)
    private val popupMenuItems = mutableListOf<PussycatMenuItemDefinition>()

    override fun getMainMenuItems(): List<PussycatMenu> {
        val items = mutableListOf<PussycatMenu>()
        if (!popupMenuItems.isEmpty()) {
            val commands = PussycatMenu(theme, Labels.COMMANDS)
            for (item in popupMenuItems) {
                val menuItem = PussycatMenuItem(theme, item.title)
                menuItem.addActionListener(item.action)
                commands.addMenuItem(menuItem)
            }
            items.add(commands)
        }
        return items
    }

    fun addPopUpMenuItems(items: List<PussycatMenuItemDefinition>) {
        popupMenuItems.addAll(items)
    }

    override fun getList(): GuiPussycatList {
        return list
    }

}
