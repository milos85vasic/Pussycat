package net.milosvasic.pussycat

import net.milosvasic.pussycat.color.Color
import net.milosvasic.pussycat.core.commands.COMMAND
import java.util.concurrent.atomic.AtomicBoolean
import net.milosvasic.pussycat.core.common.DataFilter
import net.milosvasic.pussycat.core.data.DataAbstract
import net.milosvasic.pussycat.logging.ConsoleLogger
import net.milosvasic.pussycat.utils.Text
import java.util.concurrent.CopyOnWriteArrayList
import net.milosvasic.pussycat.core.common.Execute
import kotlin.reflect.KClass


abstract class PussycatAbstract : Execute<COMMAND, String>, DataFilter<CopyOnWriteArrayList<String>, String> {

    protected lateinit var TAG : KClass<*>
    protected var logger = ConsoleLogger()
    protected var color: String = Color.BLACK
    protected lateinit var data : DataAbstract
    protected var refreshing = AtomicBoolean(false)

    override fun execute(executable: COMMAND, vararg params: String?) {
        when (executable) {
            COMMAND.LIVE -> live()
            COMMAND.FILESYSTEM -> filesystem(params)
            COMMAND.STOP -> stop()
            COMMAND.CLEAR -> clear()
            COMMAND.RESET -> filter()
            COMMAND.PAUSE -> pause()
            COMMAND.RESUME -> resume()
            COMMAND.STATUS -> logger.v(TAG, "Filter applied [ ${getFilter()} ]\n\n")
            else -> logger.w(TAG, "Unknown command: " + executable)
        }
    }

    abstract protected fun live()

    abstract protected fun filesystem(params: Array<out String?>)

    abstract protected fun stop()

    abstract protected fun pause()

    abstract protected fun resume()

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

    fun clear() {
        execute(COMMAND.RESET)
    }

}