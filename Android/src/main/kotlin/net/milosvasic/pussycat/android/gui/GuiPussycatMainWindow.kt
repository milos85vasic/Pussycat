package net.milosvasic.pussycat.android.gui

import net.milosvasic.pussycat.application.ApplicationInformation
import net.milosvasic.pussycat.gui.PussycatMainWindow
import net.milosvasic.pussycat.gui.PussycatMenu
import net.milosvasic.pussycat.gui.content.Labels
import net.milosvasic.pussycat.gui.theme.Theme

class GuiPussycatMainWindow(information: ApplicationInformation, theme: Theme) : PussycatMainWindow(information, theme) {

    override fun getMainMenuItems(): List<PussycatMenu> {
        val items = mutableListOf<PussycatMenu>()
        val children = GuiPussycatMenuFactory.CONTEXT.create()
        val commands = menuFactory.GENERAL.create(Pair(Labels.COMMANDS, children))
        items.add(commands)
        return items
    }

}