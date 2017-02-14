package net.milosvasic.pussycat

import net.milosvasic.pussycat.color.Color
import net.milosvasic.pussycat.core.COMMAND
import net.milosvasic.pussycat.core.common.DataFilter
import net.milosvasic.pussycat.core.data.Data
import net.milosvasic.pussycat.core.common.Execute
import net.milosvasic.pussycat.events.EVENT
import net.milosvasic.pussycat.listeners.Listeners
import net.milosvasic.pussycat.logging.Logger
import java.io.File
import kotlin.reflect.KClass
import java.util.concurrent.CopyOnWriteArrayList


abstract class PussycatAbstract<T, D : Data<T>> : Execute<COMMAND, String>, DataFilter<CopyOnWriteArrayList<T>, String> {

    protected lateinit var data: D
    protected val logger = Logger()
    protected lateinit var TAG: KClass<*>
    protected var color: String = Color.BLACK
    protected var mode: PUSSYCAT_MODE? = null

    val SUBSCRIPTIONS = Subscriptions()
    val configuration = PussycatConfiguration()

    companion object {
        fun getPussycatHome(): File {
            val home = System.getProperty("user.home")
            val root = File("$home${File.separator}Pussycat")
            if (!root.exists()) {
                root.mkdirs()
            }
            return root
        }
    }

    class Subscriptions {
        val EVENTS: Listeners<EVENT> = Listeners.obtain()
        val FILESYSTEM_LOADING_PROGRESS: Listeners<Double> = Listeners.obtain()
    }

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

    open fun onEvent(event: EVENT) {
        SUBSCRIPTIONS.EVENTS.notify(event)
    }

    fun getPussycatMode(): PUSSYCAT_MODE? {
        return mode
    }

    fun getArgumentOption(arg: String): String {
        var occurrences = 0
        for (char in arg) {
            if (char == '=') {
                occurrences++
            }
        }
        if (occurrences > 1) {
            throw IllegalArgumentException("'=' is contained on more than 1 places.")
        }
        if (occurrences == 1) {
            return arg.substring(0, arg.indexOf('='))
        }
        return arg
    }

    fun getArgumentValue(arg: String): String {
        if (arg.contains('=')) {
            return arg.substring(arg.indexOf('=') + 1)
        }
        return arg
    }

    abstract fun start(args: Array<String>)

    abstract protected fun live()

    abstract protected fun filesystem(params: Array<out String?>)

    abstract protected fun status()

    abstract protected fun clear()

    abstract protected fun export(params: Array<out String?>)

    abstract protected fun printLine(text: String)

}