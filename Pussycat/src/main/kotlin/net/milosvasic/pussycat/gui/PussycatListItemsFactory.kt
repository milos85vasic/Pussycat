package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.listeners.Listener


class PussycatListItemsFactory<T>(val factory: PussycatListItemFactory<T>) : Listener<Pair<T, Int>> {

    override fun onEvent(value: Pair<T, Int>?) {
        val item = value?.first
        val index = value?.second
        println(">> $index")
    }

    val pussycatListItems = mutableListOf<PussycatListItem>()

}