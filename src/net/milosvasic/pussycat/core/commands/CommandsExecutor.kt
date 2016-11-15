package net.milosvasic.pussycat.core.commands

import net.milosvasic.pussycat.core.PussycatActions
import net.milosvasic.pussycat.core.PussycatLegacy
import net.milosvasic.pussycat.core.common.Execute
import net.milosvasic.pussycat.logging.ConsoleLogger
import net.milosvasic.pussycat.logging.Logger


class CommandsExecutor : Execute<COMMAND> {

    private val TAG = PussycatLegacy::class
    private var logger: Logger = ConsoleLogger()
    private lateinit var actions: PussycatActions

    fun init(actions: PussycatActions) {
        this.actions = actions
    }

    override fun execute(executable: COMMAND) {
        when (executable) {
            COMMAND.EXIT -> actions.stop()
            COMMAND.CLEAR -> actions.clear()
            COMMAND.RESET -> actions.filter()
            COMMAND.PAUSE -> actions.pause()
            COMMAND.RESUME -> actions.resume()
            COMMAND.STATUS -> actions.getFilter()
            else -> logger.w(TAG, "Unknown command: " + executable)
        }
    }

    fun setLogger(logger: Logger) {
        this.logger = logger
    }

}