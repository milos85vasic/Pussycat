package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.actions.Creator
import java.awt.MenuItem
import java.awt.PopupMenu


object PussycatMenuFactory {

    object MAIN : Creator.Parametric<List<PussycatMenu>, Collection<PussycatMenuItem>> {
        override fun create(params: Collection<PussycatMenuItem>): List<PussycatMenu> {
            // TODO: Finish this,
            val items = mutableListOf<PussycatMenu>()
            for (item in params) {

            }
            return items

        }
    }

    object GENERAL : Creator.Parametric<PussycatMenu, Pair<String, Collection<PussycatMenuItem>>> {
        override fun create(params: Pair<String, Collection<PussycatMenuItem>>): PussycatMenu {
            val menu = PussycatMenu(params.first)
            for (item in params.second) {
                menu.add(item)
            }
            return menu
        }
    }

    object CONTEXT : Creator.Parametric<PopupMenu, Collection<MenuItem>> {
        override fun create(params: Collection<MenuItem>): PopupMenu {
            val popUpMenu = PopupMenu()
            for (item in params) {
                popUpMenu.add(item)
            }
            return popUpMenu
        }
    }

}