package net.milosvasic.pussycat


import net.milosvasic.pussycat.color.Color
import net.milosvasic.pussycat.core.COMMAND
import net.milosvasic.pussycat.core.common.DataFilter
import net.milosvasic.pussycat.core.data.Data
import net.milosvasic.pussycat.core.common.Execute
import net.milosvasic.pussycat.events.EVENT
import net.milosvasic.pussycat.events.Events
import net.milosvasic.pussycat.logging.Logger
import kotlin.reflect.KClass
import java.util.*
import java.util.concurrent.CopyOnWriteArrayList


abstract class PussycatAbstract<T, D : Data<T>> : Execute<COMMAND, String>, DataFilter<CopyOnWriteArrayList<T>, String>, Events {

    protected lateinit var TAG: KClass<*>
    protected lateinit var logger: Logger
    protected var color: String = Color.BLACK
    protected lateinit var data: D
    val configuration = PussycatConfiguration()
    protected val listeners: MutableSet<Events> = Collections.synchronizedSet(HashSet<Events>())

    override fun execute(executable: COMMAND, params: Array<String>) {
        when (executable) {
            COMMAND.LIVE -> live()
            COMMAND.FILESYSTEM -> filesystem(params)
            COMMAND.STOP -> stop()
            COMMAND.CLEAR -> clear()
            COMMAND.RESET -> filter()
            COMMAND.PAUSE -> pause()
            COMMAND.RESUME -> resume()
            COMMAND.STATUS -> status()
            COMMAND.EXPORT -> export(params)
            else -> executeOther(executable, params)
        }
    }

    override fun execute(executable: COMMAND) {
        execute(executable, emptyArray())
    }

    fun filter() {
        data.apply()
    }

    fun filter(filter: String) {
        data.apply(filter)
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

    open protected fun executeOther(executable: COMMAND, params: Array<out String?>) {
        logger.w(TAG, "Unknown command: " + executable)
    }

    override fun onEvent(event: EVENT) {
        for (listener in listeners) {
            listener.onEvent(event)
        }
    }

    fun subscribe(listener: Events) {
        listeners.add(listener)
    }

    fun unsubscribe(listener: Events) {
        listeners.remove(listener)
    }

    abstract fun start(args: Array<String>)

    abstract protected fun live()

    abstract protected fun filesystem(params: Array<out String?>)

    abstract protected fun status()

    abstract protected fun clear()

    abstract protected fun export(params: Array<out String?>)

}