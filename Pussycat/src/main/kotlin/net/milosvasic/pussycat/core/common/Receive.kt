package net.milosvasic.pussycat.core.common

interface Receive<T> {

    fun onReceive(data: T)

}