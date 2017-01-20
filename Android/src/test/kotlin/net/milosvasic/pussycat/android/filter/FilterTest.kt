package net.milosvasic.pussycat.android.filter

import net.milosvasic.pussycat.content.Messages
import net.milosvasic.pussycat.PussycatAbstract
import net.milosvasic.pussycat.android.TerminalPussycat
import net.milosvasic.pussycat.android.application.Application
import net.milosvasic.pussycat.application.APPLICATION_TYPE
import net.milosvasic.pussycat.PUSSYCAT_MODE
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
    val FILTER_TEST_PRINT_TAG = "FilterTestPrint"

    val printLineCallback = object : PrintLineCallback {
        override fun printLine(text: String?) {
            if (text != null) {
                if (text.contains(FILTER_TEST_PRINT_TAG)) {
                    linesPrinted.incrementAndGet()
                }
            } else {
                Assert.fail("We received null text!")
            }
        }
    }

    @Before
    fun beforeTestFilter() {
        testSets.put("nsnsjsjsj", 0)
        testSets.put("L ion", 0)
        testSets.put("Li on", 0)
        testSets.put("Lion", 3)
        testSets.put("Cow", 2)
        testSets.put("Elephant", 2)
        testSets.put("Lion || Elephant", 5)
        testSets.put("Elephant || Lion", 5)
        testSets.put("lion || elephant", 5)
        testSets.put("phant || Lion", 5)
        testSets.put("cow || zion", 3)
        testSets.put("cow && zion", 0)
        testSets.put("Lion && zion", 1)
        testSets.put("Zion && Lion", 1)
        testSets.put("Zion && zion", 1)
        testSets.put("vehicle", 2)
        testSets.put("animal", 4)
        testSets.put("animal && !lion", 2)
        testSets.put("!cow && !lion", 7)
        testSets.put("king || animal && !lion", 4)
        testSets.put("cow && animal || vehicle", 3)
        testSets.put("cow || lion || vehicle", 7)
        testSets.put("!lion", 9)
        testSets.put("lion && !king", 2)
        testSets.put("Cow gives us milk", 1)
        testSets.put("Cow gives us milk || Lion is the animal", 2)

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
            Thread.sleep(500)

            for ((key, value) in testSets) {
                linesPrinted.set(0)
                app.pussy?.filter(key)
                Assert.assertEquals(value, linesPrinted.get())
                linesPrinted.set(0)
            }

            Assert.assertEquals(app.pussy?.getPussycatMode(), PUSSYCAT_MODE.FILESYSTEM)
            app.stop()
            Thread.sleep(3000)
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