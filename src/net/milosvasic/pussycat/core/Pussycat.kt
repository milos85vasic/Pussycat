package net.milosvasic.pussycat.core

import net.milosvasic.pussycat.color.Color
import net.milosvasic.pussycat.core.commands.CommandsExecutor
import java.util.concurrent.atomic.AtomicBoolean
import net.milosvasic.pussycat.core.common.Filter
import net.milosvasic.pussycat.core.data.Data
import java.io.File


class Pussycat(
        executor: CommandsExecutor,
        filter: Filter<String>
) : PussycatActions {

    private val data = Data(filter)

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

    override fun printFilter(): String {
        return ""
    }

    override fun clear() {

    }

}