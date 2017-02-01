package net.milosvasic.pussycat.gui.events

import net.milosvasic.pussycat.gui.factory.DIRECTION

interface DataRequestCallback {

    fun onRefresh()

    fun onBarrierReached(from: Int, direction: DIRECTION = DIRECTION.DOWN)

    fun requestData(from: Int, amount: Int, direction: DIRECTION = DIRECTION.DOWN)

}