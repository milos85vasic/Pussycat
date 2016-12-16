package net.milosvasic.pussycat.core.common


interface DataFilter<in D, in P> {

    fun apply(data: D, pattern: P? = null)

}