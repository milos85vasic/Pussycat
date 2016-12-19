package net.milosvasic.pussycat.android

import net.milosvasic.pussycat.android.application.Application
import net.milosvasic.pussycat.application.APPLICATION_TYPE
import org.junit.Assert
import org.junit.Test
import java.util.*

class ApplicationStartingTest {

    class TestParameter(val params: Array<String>, val expectedType: APPLICATION_TYPE)

    val params = LinkedList<TestParameter>()

    init {
        val param1 = TestParameter(arrayOf<String>(), APPLICATION_TYPE.GUI)
        val param2 = TestParameter(arrayOf("--gui"), APPLICATION_TYPE.GUI)
        params.add(param1)
        params.add(param2)
    }

    @Test
    fun testApplicationStarting() {
        for (args in params) {
            val app = Application()
            val type = app.start(args.params)
            Assert.assertEquals(type, args.expectedType)
            app.stop()
        }
    }

}