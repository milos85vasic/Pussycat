package net.milosvasic.pussycat.listeners


interface Listener<in T> {

    fun onEvent(value: T?)

}