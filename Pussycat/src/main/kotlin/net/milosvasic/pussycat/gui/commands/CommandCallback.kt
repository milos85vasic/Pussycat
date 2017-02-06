package net.milosvasic.pussycat.gui.commands

import net.milosvasic.pussycat.core.COMMAND


interface CommandCallback {

    fun execute(command: COMMAND)

}