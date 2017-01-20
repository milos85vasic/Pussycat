package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.actions.Creator
import net.milosvasic.pussycat.gui.content.Labels
import net.milosvasic.pussycat.os.OS
import java.awt.MenuItem
import java.awt.PopupMenu


object PussycatMenuFactory {

    object MAIN : Creator.Parametric<List<PussycatMenu>, Collection<PussycatMenu>> {
        override fun create(params: Collection<PussycatMenu>): List<PussycatMenu> {
            val items = mutableListOf<PussycatMenu>()
            val file = PussycatMenu(Labels.FILE)
            if (!OS.isMacOS()) {
                val quit = PussycatMenuItem(Labels.QUIT)
                quit.addActionListener {  }
                file.add(quit)
            }
            items.add(file)
            for (item in params) {
                items.add(item)
            }
            return items
        }
    }

    object GENERAL : Creator.Parametric<PussycatMenu, Pair<String, Collection<PussycatMenuItemDefinition>>> {
        override fun create(params: Pair<String, Collection<PussycatMenuItemDefinition>>): PussycatMenu {
            val menu = PussycatMenu(params.first)
            for (item in params.second) {
                val menuItem = PussycatMenuItem(item.title)
                menuItem.addActionListener(item.action)
                menu.add(menuItem)
            }
            return menu
        }
    }

    object CONTEXT : Creator.Parametric<PopupMenu, Collection<PussycatMenuItemDefinition>> {
        override fun create(params: Collection<PussycatMenuItemDefinition>): PopupMenu {
            val popUpMenu = PopupMenu()
            for (item in params) {
                val menuItem = MenuItem(item.title)
                menuItem.addActionListener(item.action)
                popUpMenu.add(menuItem)
            }
            return popUpMenu
        }
    }

}