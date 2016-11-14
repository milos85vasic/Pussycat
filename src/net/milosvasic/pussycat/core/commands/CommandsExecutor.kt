package net.milosvasic.pussycat.core.commands

import net.milosvasic.pussycat.core.common.Execute
import net.milosvasic.pussycat.finish
import net.milosvasic.pussycat.pussy


class CommandsExecutor : Execute<COMMAND> {

    override fun execute(executable: COMMAND) {
        when (executable) {
            COMMAND.EXIT -> finish()
            COMMAND.CLEAR -> pussy.clear()
            COMMAND.RESET -> pussy.filter()
            COMMAND.PAUSE -> pussy.pause()
            COMMAND.RESUME -> pussy.resume()
            COMMAND.STATUS -> pussy.printFilter()
            else -> pussy.filter(line)
        }
    }

}