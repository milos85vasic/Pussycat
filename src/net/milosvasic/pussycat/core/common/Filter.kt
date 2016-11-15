package net.milosvasic.pussycat.core.common


interface Filter<T> {

    fun apply(pattern: T? = null)

}