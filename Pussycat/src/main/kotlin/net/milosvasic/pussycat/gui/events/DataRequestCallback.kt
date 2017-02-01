package net.milosvasic.pussycat.gui.events

import net.milosvasic.pussycat.gui.factory.DIRECTION

interface DataRequestCallback {

    fun onBarrierReached(from: Int, direction: DIRECTION = DIRECTION.DOWN)

    fun onRefresh()

}