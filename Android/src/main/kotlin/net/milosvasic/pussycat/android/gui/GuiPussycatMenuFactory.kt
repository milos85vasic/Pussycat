package net.milosvasic.pussycat.android.gui

import net.milosvasic.pussycat.actions.Creator
import java.awt.MenuItem
import java.util.*

object GuiPussycatMenuFactory {

    private val contextMenuItems = LinkedHashMap<String, MenuItem>()

    fun add(key: String, item: MenuItem) {
        contextMenuItems.put(key, item)
    }

    object CONTEXT : Creator.Nonparametric<Collection<MenuItem>> {
        override fun create(): Collection<MenuItem> {
            return contextMenuItems.values
        }
    }

}