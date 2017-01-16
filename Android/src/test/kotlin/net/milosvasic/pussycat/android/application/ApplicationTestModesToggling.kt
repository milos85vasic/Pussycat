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
    }

    override fun getExpectedType(): APPLICATION_TYPE {
        return APPLICATION_TYPE.CLI
    }

    override fun getExpectedMode(): PUSSYCAT_MODE? {
        return null
    }


    @Test
    override fun testApplication() {
        val app = Application(params)
        var pussycatModeToExpect = PUSSYCAT_MODE.LIVE

        app.pussy?.configuration?.exitOnStop = false
        app.pussy?.configuration?.waitingForDevicesTimeoutInSeconds = 1
        app.pussy?.configuration?.terminalPriner = BlackHoleTerminalPrinter()

        app.start()
        Thread.sleep(waitingTime)
        Assert.assertEquals(getExpectedType(), app.type)
        Assert.assertEquals(pussycatModeToExpect, app.pussy?.getPussycatMode())

        for (x in 0..3) {
            app.pussy?.execute(COMMAND.FILESYSTEM, arrayOf("./not_existing.txt"))
            Thread.sleep(waitingTime)
            Assert.assertEquals(pussycatModeToExpect, app.pussy?.getPussycatMode())

            Assert.assertTrue(localSample.exists())
            app.pussy?.execute(COMMAND.FILESYSTEM, arrayOf(localSample.absolutePath))
            pussycatModeToExpect = PUSSYCAT_MODE.FILESYSTEM
            Thread.sleep(waitingTime)
            Assert.assertEquals(pussycatModeToExpect, app.pussy?.getPussycatMode())

            app.pussy?.execute(COMMAND.LIVE)
            pussycatModeToExpect = PUSSYCAT_MODE.LIVE
            Thread.sleep(waitingTime)
            Assert.assertEquals(pussycatModeToExpect, app.pussy?.getPussycatMode())
        }

        for (x in 0..3) {
            app.pussy?.execute(COMMAND.LIVE)
            pussycatModeToExpect = PUSSYCAT_MODE.LIVE
            Thread.sleep(waitingTime)
            Assert.assertEquals(pussycatModeToExpect, app.pussy?.getPussycatMode())

            Assert.assertTrue(localSample.exists())
            app.pussy?.execute(COMMAND.FILESYSTEM, arrayOf(localSample.absolutePath))
            pussycatModeToExpect = PUSSYCAT_MODE.FILESYSTEM
            Thread.sleep(waitingTime)
            Assert.assertEquals(pussycatModeToExpect, app.pussy?.getPussycatMode())

            app.pussy?.execute(COMMAND.FILESYSTEM, arrayOf("./not_existing.txt"))
            Thread.sleep(waitingTime)
            Assert.assertEquals(pussycatModeToExpect, app.pussy?.getPussycatMode())
        }

        for (x in 0..4) {
            app.pussy?.execute(COMMAND.LIVE)
            pussycatModeToExpect = PUSSYCAT_MODE.LIVE
            Thread.sleep(waitingTime)
            Assert.assertEquals(pussycatModeToExpect, app.pussy?.getPussycatMode())

            Assert.assertTrue(localSample.exists())
            app.pussy?.execute(COMMAND.FILESYSTEM, arrayOf(localSample.absolutePath))
            pussycatModeToExpect = PUSSYCAT_MODE.FILESYSTEM
            Thread.sleep(waitingTime)
            Assert.assertEquals(pussycatModeToExpect, app.pussy?.getPussycatMode())
        }

        app.stop()
        Thread.sleep(waitingTime)
    }

}