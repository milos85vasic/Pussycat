package net.milosvasic.pussycat.android.application

import net.milosvasic.pussycat.application.APPLICATION_TYPE
import net.milosvasic.pussycat.application.PUSSYCAT_MODE
import net.milosvasic.pussycat.terminal.Printer
import org.junit.Assert
import org.junit.Test

abstract class ApplicationTestAbstract {

    lateinit var params: Array<String>
    var expectedMode: PUSSYCAT_MODE? = null
    lateinit var expectedType: APPLICATION_TYPE

    @Test
    open fun testApplication() {
        val app = Application(params)
        app.pussy?.configuration?.exitOnStop = false
        app.pussy?.configuration?.terminalPriner = object : Printer {
            override fun printLine(text: String?) {
                // Black hole output ;)
            }
        }
        Thread(Runnable {
            app.start()
            Assert.assertEquals(app.type, expectedType)
        }).start()
        Thread.sleep(3000)
        Assert.assertEquals(app.pussy?.getPussycatMode(), expectedMode)
        app.stop()
    }

}