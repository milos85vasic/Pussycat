package net.milosvasic.pussycat.core

import net.milosvasic.pussycat.color.Color
import net.milosvasic.pussycat.core.commands.CommandsExecutor
import java.util.concurrent.atomic.AtomicBoolean
import net.milosvasic.pussycat.core.common.DataFilter
import net.milosvasic.pussycat.core.data.Data
import net.milosvasic.pussycat.logger
import net.milosvasic.pussycat.logging.ConsoleLogger
import net.milosvasic.pussycat.utils.Text
import java.io.File
import java.util.concurrent.CopyOnWriteArrayList


class Pussycat(executor: CommandsExecutor, filter: DataFilter<CopyOnWriteArrayList<String>, String>) : PussycatActions {

    private val TAG = Pussycat::class
    private val data = Data(filter)
    private var logger = ConsoleLogger()

    init {
        executor.init(this)
    }


    private var run = AtomicBoolean(true)
    private var color: String = Color.BLACK
    private val paused = AtomicBoolean(false)
    private var refreshing = AtomicBoolean(false)

    override fun live() {

    }

    override fun filesystem(file: File) {

    }

    override fun stop() {

    }

    override fun pause() {

    }

    override fun resume() {

    }

    override fun filter() {

    }

    override fun filter(filter: String) {

    }

    override fun getFilter(): String {
        if (Text.isEmpty(data.getFilterPattern())) {
            return "No apply applied"
        } else {
            return data.getFilterPattern()
        }
    }

    override fun clear() {

    }

}