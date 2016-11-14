package net.milosvasic.pussycat.core.commands

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
                    try {
                        onReceive(COMMAND.valueOf(line))
                    } catch (e: IllegalArgumentException) {
                        
                    }
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