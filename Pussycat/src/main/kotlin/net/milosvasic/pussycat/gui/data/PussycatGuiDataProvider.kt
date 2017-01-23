package net.milosvasic.pussycat.gui.data


interface PussycatGuiDataProvider<T> {

    fun getData(): List<T>


}