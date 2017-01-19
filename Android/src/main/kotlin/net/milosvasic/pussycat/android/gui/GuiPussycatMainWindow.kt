package net.milosvasic.pussycat.android.gui

import net.milosvasic.pussycat.application.ApplicationInformation
import net.milosvasic.pussycat.gui.PussycatMainWindow
import net.milosvasic.pussycat.gui.PussycatMenu
import net.milosvasic.pussycat.gui.PussycatMenuFactory
import net.milosvasic.pussycat.gui.content.Labels
import net.milosvasic.pussycat.gui.themes.Theme

class GuiPussycatMainWindow(theme: Theme, information: ApplicationInformation) : PussycatMainWindow(theme, information) {

    override fun getMainMenuItems(): List<PussycatMenu> {
        val items = mutableListOf<PussycatMenu>()
        val children = GuiPussycatMenuFactory.CONTEXT.create()
        val commands = PussycatMenuFactory.GENERAL.create(Pair(Labels.COMMANDS, children))
        items.add(commands)
        return items
    }

}