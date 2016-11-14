package net.milosvasic.pussycat.core.commands

import net.milosvasic.pussycat.finish
import net.milosvasic.pussycat.pussy
import java.util.concurrent.atomic.AtomicBoolean

class ConsoleCommandsReceiver(val executor: CommandsExecutor) : CommandsReceiver(executor) {

    private var run = AtomicBoolean(false)

    fun start() {
        run.set(true)
        Thread(Runnable {
            Thread.currentThread().name = "Commands thread"
            while (run.get()) {
                val line = readLine()
                if (line != null && !line.isEmpty()) {
                    onReceive(COMMAND.valueOf(line))
                } else {
                    onReceive(COMMAND.STATUS)
                }
            }
        }).start()
    }

    fun stop() {
        run.set(false)
    }

    override fun onReceive(data: COMMAND) {
        executor.execute(data)
    }

}