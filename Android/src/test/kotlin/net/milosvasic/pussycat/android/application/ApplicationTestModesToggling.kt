package net.milosvasic.pussycat.android.application


import net.milosvasic.pussycat.android.terminal.BlackHoleTerminalPrinter
import net.milosvasic.pussycat.application.APPLICATION_TYPE
import net.milosvasic.pussycat.application.PUSSYCAT_MODE
import net.milosvasic.pussycat.core.COMMAND
import org.junit.Assert
import org.junit.Test


class ApplicationTestModesToggling : ApplicationTestAbstract() {

    init {
        params = arrayOf("--terminal")
        expectedType = APPLICATION_TYPE.CLI
    }

    @Test
    override fun testApplication() {
        val app = Application(params)
        var pussycatModeToExpect = PUSSYCAT_MODE.LIVE

        app.pussy?.configuration?.exitOnStop = false
        app.pussy?.configuration?.waitingForDevicesTimeoutInSeconds = 2
        app.pussy?.configuration?.terminalPriner = BlackHoleTerminalPrinter()

        app.start()
        Thread.sleep(waitingTime)
        Assert.assertEquals(app.type, expectedType)
        Assert.assertEquals(app.pussy?.getPussycatMode(), pussycatModeToExpect)

        for (x in 0..3) {
            app.pussy?.execute(COMMAND.FILESYSTEM, arrayOf("./not_existing.txt"))
            Thread.sleep(waitingTime)
            Assert.assertEquals(app.pussy?.getPussycatMode(), pussycatModeToExpect)

            Assert.assertTrue(localSample.exists())
            app.pussy?.execute(COMMAND.FILESYSTEM, arrayOf(localSample.absolutePath))
            pussycatModeToExpect = PUSSYCAT_MODE.FILESYSTEM
            Thread.sleep(waitingTime)
            Assert.assertEquals(app.pussy?.getPussycatMode(), pussycatModeToExpect)

            app.pussy?.execute(COMMAND.LIVE)
            pussycatModeToExpect = PUSSYCAT_MODE.LIVE
            Thread.sleep(waitingTime)
            Assert.assertEquals(app.pussy?.getPussycatMode(), pussycatModeToExpect)
        }

        for (x in 0..3) {
            app.pussy?.execute(COMMAND.LIVE)
            pussycatModeToExpect = PUSSYCAT_MODE.LIVE
            Thread.sleep(waitingTime)
            Assert.assertEquals(app.pussy?.getPussycatMode(), pussycatModeToExpect)

            Assert.assertTrue(localSample.exists())
            app.pussy?.execute(COMMAND.FILESYSTEM, arrayOf(localSample.absolutePath))
            pussycatModeToExpect = PUSSYCAT_MODE.FILESYSTEM
            Thread.sleep(waitingTime)
            Assert.assertEquals(app.pussy?.getPussycatMode(), pussycatModeToExpect)

            app.pussy?.execute(COMMAND.FILESYSTEM, arrayOf("./not_existing.txt"))
            Thread.sleep(waitingTime)
            Assert.assertEquals(app.pussy?.getPussycatMode(), pussycatModeToExpect)
        }

        for (x in 0..4) {
            app.pussy?.execute(COMMAND.LIVE)
            pussycatModeToExpect = PUSSYCAT_MODE.LIVE
            Thread.sleep(waitingTime)
            Assert.assertEquals(app.pussy?.getPussycatMode(), pussycatModeToExpect)

            Assert.assertTrue(localSample.exists())
            app.pussy?.execute(COMMAND.FILESYSTEM, arrayOf(localSample.absolutePath))
            pussycatModeToExpect = PUSSYCAT_MODE.FILESYSTEM
            Thread.sleep(waitingTime)
            Assert.assertEquals(app.pussy?.getPussycatMode(), pussycatModeToExpect)
        }

        app.stop()
    }

}