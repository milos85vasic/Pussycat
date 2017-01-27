package net.milosvasic.pussycat.gui.factory

import net.milosvasic.pussycat.gui.PussycatListItem

interface PussycatListItemsRequestCallback {

    fun onData(items: Collection<PussycatListItem>)

}