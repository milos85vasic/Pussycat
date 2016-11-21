package net.milosvasic.pussycat

import net.milosvasic.pussycat.color.Color
import net.milosvasic.pussycat.core.commands.COMMAND
import java.util.concurrent.atomic.AtomicBoolean
import net.milosvasic.pussycat.core.common.DataFilter
import net.milosvasic.pussycat.core.data.Data
import net.milosvasic.pussycat.logging.ConsoleLogger
import net.milosvasic.pussycat.utils.Text
import java.io.File
import java.util.concurrent.CopyOnWriteArrayList
import net.milosvasic.pussycat.core.common.Execute


public class Pussycat() : Execute<COMMAND>, DataFilter<CopyOnWriteArrayList<String>, String> {

    private val data = Data(this)
    private val TAG = Pussycat::class
    private var logger = ConsoleLogger()

    private var run = AtomicBoolean(true)
    private var color: String = Color.BLACK
    private val paused = AtomicBoolean(false)
    private var refreshing = AtomicBoolean(false)

    override fun execute(executable: COMMAND) {
        when (executable) {
            COMMAND.STOP -> stop()
            COMMAND.CLEAR -> clear()
            COMMAND.RESET -> filter()
            COMMAND.PAUSE -> pause()
            COMMAND.RESUME -> resume()
            COMMAND.STATUS -> getFilter()
            else -> logger.w(TAG, "Unknown command: " + executable)
        }
    }

    fun stop() {

    }

    fun pause() {

    }

    fun resume() {

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

    fun clear() {
        execute(COMMAND.RESET)
    }


    override fun apply(data: CopyOnWriteArrayList<String>, pattern: String?) {
        // TODO: Implement this.
    }

}