package net.milosvasic.pussycat.gui.factory

import net.milosvasic.pussycat.gui.PussycatListItem

interface PussycatListItemsRequestCallback {

    fun onData(items: List<PussycatListItem>, direction: DIRECTION = DIRECTION.DOWN)

}