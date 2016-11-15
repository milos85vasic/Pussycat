package net.milosvasic.pussycat.core

import net.milosvasic.pussycat.color.Color
import net.milosvasic.pussycat.core.commands.CommandsExecutor
import java.util.concurrent.atomic.AtomicBoolean
import net.milosvasic.pussycat.core.common.Filter
import net.milosvasic.pussycat.core.data.Data


class PussycatNew(
        executor: CommandsExecutor,
        filter: Filter<String>
) {

    private val data = Data(filter)


    private var run = AtomicBoolean(true)
    private var color: String = Color.BLACK
    private val paused = AtomicBoolean(false)
    private var refreshing = AtomicBoolean(false)

}