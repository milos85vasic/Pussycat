package net.milosvasic.pussycat.core.common

interface Execute<T, P> {

    fun execute(executable: T, vararg params: P?)

}