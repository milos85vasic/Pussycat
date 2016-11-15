package net.milosvasic.pussycat.core.common


interface Filter<T> {

    fun filter(pattern: T? = null)

}