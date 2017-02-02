package net.milosvasic.pussycat.gui.data

import net.milosvasic.pussycat.gui.PussycatListItem
import net.milosvasic.pussycat.gui.factory.DIRECTION
import net.milosvasic.pussycat.gui.factory.PussycatListItemsRequest

interface DataCallback {

    fun onData(request: PussycatListItemsRequest, items: List<PussycatListItem>, direction: DIRECTION = DIRECTION.DOWN)

    fun onDataRequestRejected(request: PussycatListItemsRequest)

}