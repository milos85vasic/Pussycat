package net.milosvasic.pussycat.gui.factory

import net.milosvasic.pussycat.gui.data.DIRECTION
import net.milosvasic.pussycat.gui.data.DataCallback
import net.milosvasic.pussycat.gui.data.DataRequestCallback

data class PussycatListItemsRequest(
        val from: Int,
        val amount: Int,
        val direction: DIRECTION,
        val callback: DataCallback,
        val dataRequestCallback: DataRequestCallback? = null
)
