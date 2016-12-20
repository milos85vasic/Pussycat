package net.milosvasic.pussycat.android.application

import net.milosvasic.pussycat.PussycatAbstract
import net.milosvasic.pussycat.android.terminal.BlackHoleTerminalPrinter
import net.milosvasic.pussycat.application.APPLICATION_TYPE
import net.milosvasic.pussycat.application.PUSSYCAT_MODE
import net.milosvasic.pussycat.core.COMMAND
import org.junit.Assert
import org.junit.Test


class ApplicationTestModesToggling : ApplicationTestAbstract() {

    init {
        params = arrayOf("--terminal")
        expectedMode = PUSSYCAT_MODE.LIVE
        expectedType = APPLICATION_TYPE.CLI
    }

    @Test
    override fun testApplication() {
        val app = Application(params)
        app.pussy?.configuration?.exitOnStop = false
        app.pussy?.configuration?.terminalPriner = BlackHoleTerminalPrinter()
        Thread(Runnable {
            app.start()
            Assert.assertEquals(app.type, expectedType)
        }).start()
        Thread.sleep(3000)
        Assert.assertEquals(app.pussy?.getPussycatMode(), expectedMode)
        for (x in 0..10) {
            if (x % 2 == 0) {
                if (x >= 5) {
                    app.pussy?.execute(COMMAND.FILESYSTEM, arrayOf("./$x.txt"))
                } else {
                    expectedMode = PUSSYCAT_MODE.FILESYSTEM
                    app.pussy?.execute(COMMAND.FILESYSTEM, arrayOf(localSample.absolutePath))
                }
            } else {
                app.pussy?.execute(COMMAND.LIVE)
                expectedMode = PUSSYCAT_MODE.LIVE
            }
            Thread.sleep(3000)
            Assert.assertEquals(app.pussy?.getPussycatMode(), expectedMode)
        }
        app.stop()
    }

}