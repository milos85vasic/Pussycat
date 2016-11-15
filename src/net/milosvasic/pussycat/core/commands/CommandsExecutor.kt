package net.milosvasic.pussycat.core.commands

import net.milosvasic.pussycat.core.PussycatActions
import net.milosvasic.pussycat.core.PussycatLegacy
import net.milosvasic.pussycat.core.common.Execute
import net.milosvasic.pussycat.finish
import net.milosvasic.pussycat.logging.ConsoleLogger
import net.milosvasic.pussycat.logging.Logger
import java.io.File


class CommandsExecutor : Execute<COMMAND>, PussycatActions {

    private val TAG = PussycatLegacy::class
    private var logger: Logger = ConsoleLogger()

    override fun execute(executable: COMMAND) {
        when (executable) {
            COMMAND.EXIT -> finish()
            COMMAND.CLEAR -> clear()
            COMMAND.RESET -> filter()
            COMMAND.PAUSE -> pause()
            COMMAND.RESUME -> resume()
            COMMAND.STATUS -> printFilter()
            else -> logger.w(TAG, "Unknown command: " + executable)
        }
    }

    fun setLogger(logger: Logger) {
        this.logger = logger
    }

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