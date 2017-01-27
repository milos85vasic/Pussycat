package net.milosvasic.pussycat.gui

interface PussycatListItemFactory<T> {

    fun obtain(item: T, index: Int): PussycatListItem

}