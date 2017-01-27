package net.milosvasic.pussycat.gui.factory

import net.milosvasic.pussycat.gui.PussycatListItem

interface PussycatListItemFactory<T> {

    fun obtain(item: T, index: Int): PussycatListItem

}