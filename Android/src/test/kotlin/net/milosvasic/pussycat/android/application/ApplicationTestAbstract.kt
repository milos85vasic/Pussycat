package net.milosvasic.pussycat.android.application

import net.milosvasic.pussycat.PussycatAbstract
import net.milosvasic.pussycat.android.terminal.BlackHoleTerminalPrinter
import net.milosvasic.pussycat.application.APPLICATION_TYPE
import net.milosvasic.pussycat.PUSSYCAT_MODE
import org.junit.After
import org.junit.Assert
import org.junit.Before
import org.junit.Test
import java.io.File

abstract class ApplicationTestAbstract {

    var waitingTime : Long = 3000
    lateinit var localSample : File
    lateinit var params: Array<String>
    val localFileName = "testing_parser.txt"

    @Before
    fun beforeTestApplication() {
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
        app.pussy?.configuration?.terminalPrinter = BlackHoleTerminalPrinter()
        app.pussy?.configuration?.waitingForDevicesTimeoutInSeconds = 1
        Thread(Runnable {
            app.start()
            Assert.assertEquals(getExpectedType(), app.type)
        }).start()
        Thread.sleep(waitingTime)
        Assert.assertEquals(getExpectedMode(), app.pussy?.getPussycatMode())
        app.stop()
        Thread.sleep(waitingTime)
    }

    @After
    fun afterTestParser() {
        val root = PussycatAbstract.getPussycatHome()
        val localSample = File(root.absolutePath, localFileName)
        Assert.assertTrue(localSample.exists())
        localSample.delete()
        Assert.assertFalse(localSample.exists())
    }

    abstract fun getExpectedType(): APPLICATION_TYPE

    abstract fun getExpectedMode(): PUSSYCAT_MODE?

}