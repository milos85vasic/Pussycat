package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.actions.Creator
import net.milosvasic.pussycat.gui.content.Labels
import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.os.OS
import java.awt.MenuItem
import java.awt.PopupMenu


class PussycatMenuFactory(val theme: Theme) {

    val MAIN: Creator.Parametric<List<PussycatMenu>, Collection<PussycatMenu>> = Main(theme)
    val GENERAL: Creator.Parametric<PussycatMenu, Pair<String, Collection<PussycatMenuItemDefinition>>> = General(theme)
    val CONTEXT: Creator.Parametric<PopupMenu, Collection<PussycatMenuItemDefinition>> = Context()

    private class Main(val theme: Theme) : Creator.Parametric<List<PussycatMenu>, Collection<PussycatMenu>> {
        override fun create(params: Collection<PussycatMenu>): List<PussycatMenu> {
            val items = mutableListOf<PussycatMenu>()
            if (!OS.isMacOS()) {
                val file = PussycatMenu(theme, Labels.FILE)
                val quit = PussycatMenuItem(theme, Labels.QUIT)
                quit.addActionListener { System.exit(0) }
                file.add(quit)
                items.add(file)
            }
            for (item in params) {
                items.add(item)
            }
            if (!OS.isMacOS()) {
                val pussycat = PussycatMenu(theme, Labels.PUSSYCAT)
                val about = PussycatMenuItem(theme, Labels.ABOUT)
                about.addActionListener {
                    //                    val aboutDialog = PussycatAboutDialog()
//                    aboutDialog.open()
                }
                pussycat.add(about)
                items.add(pussycat)
            }
            return items
        }
    }

    private class General(val theme: Theme) : Creator.Parametric<PussycatMenu, Pair<String, Collection<PussycatMenuItemDefinition>>> {
        override fun create(params: Pair<String, Collection<PussycatMenuItemDefinition>>): PussycatMenu {
            val menu = PussycatMenu(theme, params.first)
            for (item in params.second) {
                val menuItem = PussycatMenuItem(theme, item.title)
                menuItem.addActionListener(item.action)
                menu.add(menuItem)
            }
            return menu
        }
    }

    private class Context : Creator.Parametric<PopupMenu, Collection<PussycatMenuItemDefinition>> {
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