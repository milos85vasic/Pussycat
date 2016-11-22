package net.milosvasic.pussycat

import net.milosvasic.pussycat.color.Color
import net.milosvasic.pussycat.core.COMMAND
import net.milosvasic.pussycat.core.common.DataFilter
import net.milosvasic.pussycat.core.data.DataAbstract
import net.milosvasic.pussycat.utils.Text
import java.util.concurrent.CopyOnWriteArrayList
import net.milosvasic.pussycat.core.common.Execute
import net.milosvasic.pussycat.events.EVENT
import net.milosvasic.pussycat.events.Events
import net.milosvasic.pussycat.logging.Logger
import kotlin.reflect.KClass
import java.lang.ref.WeakReference


abstract class PussycatAbstract : Execute<COMMAND, String>, DataFilter<CopyOnWriteArrayList<String>, String>, Events {

    protected lateinit var TAG: KClass<*>
    protected lateinit var logger: Logger
    protected var color: String = Color.BLACK
    protected lateinit var data: DataAbstract
    protected val eventListeners: CopyOnWriteArrayList<WeakReference<Events>> = CopyOnWriteArrayList()

    override fun execute(executable: COMMAND, vararg params: String?) {
        when (executable) {
            COMMAND.LIVE -> live()
            COMMAND.FILESYSTEM -> filesystem(params)
            COMMAND.STOP -> stop()
            COMMAND.CLEAR -> clear()
            COMMAND.RESET -> filter()
            COMMAND.PAUSE -> pause()
            COMMAND.RESUME -> resume()
            COMMAND.STATUS -> status()
            else -> logger.w(TAG, "Unknown command: " + executable)
        }
    }

    fun filter() {
        data.apply()
    }

    fun filter(filter: String) {
        data.apply(filter)
    }

    fun getFilter(): String {
        if (Text.isEmpty(data.getFilterPattern())) {
            return "No apply applied"
        } else {
            return data.getFilterPattern()
        }
    }

    open protected fun stop() {
        onEvent(EVENT.STOP)
    }

    open protected fun pause() {
        onEvent(EVENT.PAUSE)
    }

    open protected fun resume() {
        onEvent(EVENT.RESUME)
    }

    override fun onEvent(event: EVENT) {
        for (reference in eventListeners) {
            reference.get()?.onEvent(event)
        }
    }

    fun subscribe(listener: Events) {
        val iterator = eventListeners.iterator()
        while (iterator.hasNext()) {
            val reference = iterator.next()
            val refListener = reference.get()
            if (refListener != null && refListener === listener) {
                return
            }
        }
        eventListeners.add(WeakReference<Events>(listener))
    }

    fun unsubscribe(listener: Events) {
        val iterator = eventListeners.iterator()
        while (iterator.hasNext()) {
            val reference = iterator.next()
            val refListener = reference.get()
            if (refListener != null && refListener === listener) {
                eventListeners.remove(reference)
            }
        }
    }

    abstract protected fun live()

    abstract protected fun filesystem(params: Array<out String?>)

    abstract protected fun status()

    abstract protected fun clear()

}