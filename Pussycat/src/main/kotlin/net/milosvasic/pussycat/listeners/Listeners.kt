package net.milosvasic.pussycat.listeners

import java.util.*


class Listeners<LISTENER_RESULT_TYPE> private constructor() {

    companion object {
        fun <LISTENER_RESULT_TYPE> obtain(): Listeners<LISTENER_RESULT_TYPE> {
            return Listeners()
        }
    }

    private val listeners: MutableSet<Listener<LISTENER_RESULT_TYPE>> = Collections.synchronizedSet(HashSet<Listener<LISTENER_RESULT_TYPE>>())

    fun subscribe(listener: Listener<LISTENER_RESULT_TYPE>) {
        listeners.add(listener)
    }

    fun unsubscribe(listener: Listener<LISTENER_RESULT_TYPE>) {
        listeners.remove(listener)
    }

    fun notify(value: LISTENER_RESULT_TYPE?) {
        for (listener in listeners) {
            listener.onEvent(value)
        }
    }

}