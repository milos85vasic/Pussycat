package net.milosvasic.pussycat

import net.milosvasic.pussycat.terminal.Printer


class PussycatConfiguration {

    var exitOnStop = true
    var terminalPriner: Printer? = null
    private var exitRoutine: Runnable? = null

    fun getExitRoutine() = Runnable {
        exitRoutine?.run()
        if (exitOnStop) {
            System.exit(0)
        }
    }

    fun setExitRoutine(routine: Runnable) {
        exitRoutine = routine
    }

}