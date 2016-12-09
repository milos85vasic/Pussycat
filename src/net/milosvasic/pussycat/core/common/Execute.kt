package net.milosvasic.pussycat.core.common

interface Execute<T, P> {

    fun execute(executable: T)

    fun execute(executable: T, params: Array<P>)

}