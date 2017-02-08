package net.milosvasic.pussycat.gui.data


interface DataRequestStrategy {

    fun getFirstIndex(): Int

    fun limitToIndexes(indexes: List<Int>)

    fun requestData(from: Int, amount: Int, direction: DIRECTION, callback: DataRequestCallback? = null)

}