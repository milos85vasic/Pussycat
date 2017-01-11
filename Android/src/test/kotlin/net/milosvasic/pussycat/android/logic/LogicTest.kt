package net.milosvasic.pussycat.android.logic

import net.milosvasic.pussycat.PussycatAbstract
import net.milosvasic.pussycat.android.application.Application
import net.milosvasic.pussycat.android.terminal.BlackHoleTerminalPrinter
import net.milosvasic.pussycat.application.APPLICATION_TYPE
import net.milosvasic.pussycat.application.PUSSYCAT_MODE
import net.milosvasic.pussycat.utils.Files
import org.junit.After
import org.junit.Assert
import org.junit.Before
import org.junit.Test
import java.io.File


class LogicTest {

    var resources = mutableListOf<String>()

    @Before
    fun beforeTestLogic() {
        resources.addAll(Files.getResourceFiles("samples/android/logic"))
        Assert.assertTrue(resources.size == 1)
        val root = PussycatAbstract.getPussycatHome()
        for (resource in resources) {
            val localSample = File(root.absolutePath, resource)
            if (!localSample.exists()) {
                val input = javaClass.classLoader.getResourceAsStream("samples/android/logic/$resource")
                localSample.writeBytes(input.readBytes())
                input.close()
            }
            if (!localSample.exists()) {
                println("Couldn't prepare test resource ${localSample.absolutePath}")
                Assert.fail()
            }
        }
    }

    @Test
    fun testLogic() {
        for (resource in resources) {
            val resourceFile = File(PussycatAbstract.Companion.getPussycatHome(), resource)
            val params = arrayOf("--terminal", "--filesystem=${resourceFile.absolutePath}")
            val app = Application(params)
            app.pussy?.configuration?.exitOnStop = false
//            app.pussy?.configuration?.terminalPriner = BlackHoleTerminalPrinter()
            app.pussy?.configuration?.waitingForDevicesTimeoutInSeconds = 1
            Thread(Runnable {
                app.start()
                Assert.assertEquals(app.type, APPLICATION_TYPE.CLI)
            }).start()
            Thread.sleep(3000)
            Assert.assertEquals(app.pussy?.getPussycatMode(), PUSSYCAT_MODE.FILESYSTEM)
            app.stop()
        }
    }

    @After
    fun afterTestLogic() {
        for (resource in resources) {
            val root = PussycatAbstract.getPussycatHome()
            val localSample = File(root.absolutePath, resource)
            Assert.assertTrue(localSample.exists())
            localSample.delete()
            Assert.assertFalse(localSample.exists())
        }
    }

}