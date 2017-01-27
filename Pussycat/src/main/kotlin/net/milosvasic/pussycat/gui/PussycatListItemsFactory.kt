package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.listeners.Listener


class PussycatListItemsFactory<T>(val factory: PussycatListItemFactory<T>) : Listener<T> {

    override fun onEvent(value: T?) {
        println("New data")
    }

    val pussycatListItems = mutableListOf<PussycatListItem>()

}