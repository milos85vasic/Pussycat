package net.milosvasic.pussycat.gui.factory

data class PussycatListItemsRequest(
        val from: Int,
        val amount: Int,
        val direction: DIRECTION,
        val callback: PussycatListItemsRequestCallback
)
