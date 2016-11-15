package net.milosvasic.pussycat.core.common


interface Filter<in P> {

    fun apply(pattern: P? = null)

}