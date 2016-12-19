package net.milosvasic.pussycat


class PussycatConfiguration {

    var exitOnStop = true
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