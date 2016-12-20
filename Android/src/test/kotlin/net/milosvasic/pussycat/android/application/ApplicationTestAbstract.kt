package net.milosvasic.pussycat.android.application

import net.milosvasic.pussycat.PussycatAbstract
import net.milosvasic.pussycat.android.terminal.BlackHoleTerminalPrinter
import net.milosvasic.pussycat.application.APPLICATION_TYPE
import net.milosvasic.pussycat.application.PUSSYCAT_MODE
import org.junit.Assert
import org.junit.Before
import org.junit.Test
import java.io.File

abstract class ApplicationTestAbstract {

    var waitingTime : Long = 3000
    lateinit var localSample : File
    lateinit var params: Array<String>
    var expectedMode: PUSSYCAT_MODE? = null
    lateinit var expectedType: APPLICATION_TYPE

    @Before
    fun beforeTestApplication() {
        val localFileName = "testing_parser.txt"
        val root = PussycatAbstract.getPussycatHome()
        localSample = File(root.absolutePath, localFileName)
        if (!localSample.exists()) {
            val input = javaClass.classLoader.getResourceAsStream("samples/android/$localFileName")
            localSample.writeBytes(input.readBytes())
            input.close()
        } else {
            println("All test resources already prepared.")
        }
        if (!localSample.exists()) {
            println("Couldn't prepare test resource ${localSample.absolutePath}")
            Assert.fail()
        }
    }

    @Test
    open fun testApplication() {
        val app = Application(params)
        app.pussy?.configuration?.exitOnStop = false
        app.pussy?.configuration?.terminalPriner = BlackHoleTerminalPrinter()
        app.pussy?.configuration?.waitingForDevicesTimeoutInSeconds = 2
        Thread(Runnable {
            app.start()
            Assert.assertEquals(app.type, expectedType)
        }).start()
        Thread.sleep(waitingTime)
        Assert.assertEquals(app.pussy?.getPussycatMode(), expectedMode)
        app.stop()
    }

}