package net.milosvasic.pussycat.android.gui

import net.milosvasic.pussycat.actions.Creator
import net.milosvasic.pussycat.gui.PussycatMenuItemDefinition
import java.util.*

object GuiPussycatMenuFactory {

    private val contextMenuItems = LinkedHashMap<String, PussycatMenuItemDefinition>()

    fun add(key: String, item: PussycatMenuItemDefinition) {
        contextMenuItems.put(key, item)
    }

    object CONTEXT : Creator.Nonparametric<Collection<PussycatMenuItemDefinition>> {
        override fun create(): Collection<PussycatMenuItemDefinition> {
            return contextMenuItems.values
        }
    }

}