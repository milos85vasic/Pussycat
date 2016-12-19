package net.milosvasic.pussycat.android

import net.milosvasic.pussycat.android.application.Application
import net.milosvasic.pussycat.application.APPLICATION_TYPE
import org.junit.Assert
import org.junit.Test


class ApplicationTestTerminalParam : ApplicationTestAbstract() {

    /*

     init {
        val param4 = TestParameter(arrayOf("--terminal --filesystem"), APPLICATION_TYPE.CLI)
//        val param5 = TestParameter(arrayOf("--terminal --filesystem="), APPLICATION_TYPE.CLI)
//        val param6 = TestParameter(arrayOf("--terminal --filesystem==="), APPLICATION_TYPE.CLI)
        val param7 = TestParameter(arrayOf("--terminal --filesystem=samples/android/testing_parser.txt"), APPLICATION_TYPE.CLI)
        params.add(param1)
        params.add(param2)
        params.add(param3)
        params.add(param4)
//        params.add(param5)
//        params.add(param6)
        params.add(param7)
    }

    */

    init {
        params = arrayOf("--terminal")
        expectedType = APPLICATION_TYPE.CLI
    }

    @Test
    override fun testApplication() {
        val app = Application()
        val type = app.start(params)
        Assert.assertEquals(type, expectedType)
        Thread.sleep(3000)
        app.stop()
    }

}