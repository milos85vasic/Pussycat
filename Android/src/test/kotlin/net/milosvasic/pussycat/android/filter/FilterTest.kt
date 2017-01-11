package net.milosvasic.pussycat.android.filter

import net.milosvasic.pussycat.PussycatAbstract
import net.milosvasic.pussycat.android.application.Application
import net.milosvasic.pussycat.application.APPLICATION_TYPE
import net.milosvasic.pussycat.application.PUSSYCAT_MODE
import net.milosvasic.pussycat.terminal.Printer
import net.milosvasic.pussycat.utils.Files
import org.junit.After
import org.junit.Assert
import org.junit.Before
import org.junit.Test
import java.io.File
import java.util.*
import java.util.concurrent.atomic.AtomicInteger


class FilterTest {

    val linesPrinted = AtomicInteger()
    val testSets = HashMap<String, Int>()
    var resources = mutableListOf<String>()

    val printLineCallback = object : PrintLineCallback {
        override fun printLine(text: String?) {
            val got = linesPrinted.incrementAndGet()
            println("[ $got ] >> $text")
        }
    }

    @Before
    fun beforeTestFilter() {
        testSets.put("Lion", 3)
        testSets.put("Cow", 2)
        testSets.put("Elephant", 2)

        resources.addAll(Files.getResourceFiles("samples/android/filter"))
        Assert.assertTrue(resources.size == 1)
        val root = PussycatAbstract.getPussycatHome()
        for (resource in resources) {
            val localSample = File(root.absolutePath, resource)
            if (!localSample.exists()) {
                val input = javaClass.classLoader.getResourceAsStream("samples/android/filter/$resource")
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
    fun testFilter() {
        for (resource in resources) {
            val resourceFile = File(PussycatAbstract.Companion.getPussycatHome(), resource)
            val params = arrayOf("--terminal", "--filesystem=${resourceFile.absolutePath}")
            val app = Application(params)
            app.pussy?.configuration?.exitOnStop = false
            app.pussy?.configuration?.terminalPriner = FilterTestTerminalPrinter(printLineCallback)
            app.pussy?.configuration?.waitingForDevicesTimeoutInSeconds = 1
            Thread(Runnable {
                app.start()
                Assert.assertEquals(app.type, APPLICATION_TYPE.CLI)
            }).start()
            Thread.sleep(1500)

            for ((key, value) in testSets) {
                linesPrinted.set(0)
                app.pussy?.filter(key)
                Assert.assertEquals(value + 1, linesPrinted.get())
                linesPrinted.set(0)
            }

            Assert.assertEquals(app.pussy?.getPussycatMode(), PUSSYCAT_MODE.FILESYSTEM)
            app.stop()
        }
    }

    @After
    fun afterTestFilter() {
        for (resource in resources) {
            val root = PussycatAbstract.getPussycatHome()
            val localSample = File(root.absolutePath, resource)
            Assert.assertTrue(localSample.exists())
            localSample.delete()
            Assert.assertFalse(localSample.exists())
        }
    }

    class FilterTestTerminalPrinter(val callback: PrintLineCallback) : Printer {
        override fun printLine(text: String?) {
            callback.printLine(text)
        }
    }

    interface PrintLineCallback {
        fun printLine(text: String?)
    }

}