package net.milosvasic.pussycat.core.common


interface Filter<in P, in M> {

    fun apply(pattern: P? = null)

    fun evaluate(message: M): Boolean

}