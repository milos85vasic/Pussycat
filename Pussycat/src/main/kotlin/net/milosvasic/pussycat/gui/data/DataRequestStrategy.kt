package net.milosvasic.pussycat.gui.data


interface DataRequestStrategy {

    fun requestData(from: Int, amount: Int, direction: DIRECTION, callback: DataRequestCallback? = null)

}