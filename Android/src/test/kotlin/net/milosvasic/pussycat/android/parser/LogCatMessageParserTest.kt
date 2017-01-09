package net.milosvasic.pussycat.android.parser

import net.milosvasic.pussycat.PussycatAbstract
import net.milosvasic.pussycat.android.data.parser.LogCatMessageParser
import org.junit.Assert
import org.junit.Before
import org.junit.Test
import java.util.ArrayList
import com.sun.org.apache.xalan.internal.utils.SecuritySupport.getContextClassLoader
import net.milosvasic.pussycat.android.data.AndroidLogCatMessage
import net.milosvasic.pussycat.android.data.parser.LogCatMessageParserMatcherListener
import net.milosvasic.pussycat.utils.Text
import org.junit.After
import java.io.*


class LogCatMessageParserTest {

    var currentResource = ""
    val parser = LogCatMessageParser()
    var resources = mutableListOf<String>()

    val listener = object : LogCatMessageParserMatcherListener {
        override fun onMatch(success: Boolean, message: AndroidLogCatMessage?) {
            val msg = message?.msg as String
            for (item in ParsingFailureIgnoreList.list) {
                if (msg.contains(item)) {
                    return
                }
            }
            if (!success) Assert.fail(message?.msg)
        }
    }

    @Before
    fun beforeTestParser() {
        parser.subscribe(listener)
        resources.addAll(getResourceFiles("samples/android/parser"))
        Assert.assertTrue(resources.size == 9)
        for (resource in resources) {
            val root = PussycatAbstract.getPussycatHome()
            val localSample = File(root.absolutePath, resource)
            if (!localSample.exists()) {
                val input = javaClass.classLoader.getResourceAsStream("samples/android/parser/$resource")
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
    fun testParser() {
        for (resource in resources) {
            currentResource = resource
            val resourceFile = File(PussycatAbstract.Companion.getPussycatHome(), resource)
            val lines = resourceFile.readLines()
            val messages = parser.processLogLines(Array(lines.size, { i -> lines[i] }))
            Assert.assertFalse(messages.isEmpty())
        }
    }

    @After
    fun afterTestParser() {
        parser.unsubscribe(listener)
    }

    private fun getResourceFiles(path: String): List<String> {
        val filenames = ArrayList<String>()
        getResourceAsStream(path).use({ input ->
            BufferedReader(InputStreamReader(input)).use { br ->
                var resource: String
                resource = br.readLine()
                while (!Text.isEmpty(resource)) {
                    filenames.add(resource)
                    try {
                        resource = br.readLine()
                    } catch (e: Exception) {
                        resource = ""
                    }
                }
            }
        })
        return filenames
    }

    private fun getResourceAsStream(resource: String): InputStream {
        val input = getContextClassLoader().getResourceAsStream(resource)
        return input ?: javaClass.getResourceAsStream(resource)
    }

}