package net.milosvasic.pussycat.core.commands

import net.milosvasic.pussycat.core.common.Receive

abstract class CommandsReceiver(executor: CommandsExecutor) : Receive<COMMAND>