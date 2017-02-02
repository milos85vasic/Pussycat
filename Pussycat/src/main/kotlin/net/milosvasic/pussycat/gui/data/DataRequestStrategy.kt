package net.milosvasic.pussycat.gui.data

import net.milosvasic.pussycat.gui.data.DIRECTION

interface DataRequestStrategy {

    fun refresh(callback : DataRequestCallback? = null)

    fun barrierReached(from: Int, direction: DIRECTION, callback : DataRequestCallback? = null)

    fun requestData(from: Int, amount: Int, direction: DIRECTION, callback : DataRequestCallback? = null)

}