package net.milosvasic.pussycat.android

import net.milosvasic.pussycat.android.application.Application
import net.milosvasic.pussycat.application.APPLICATION_TYPE
import org.junit.Assert
import org.junit.Test


class ApplicationTestTerminalFilesystemParams4  : ApplicationTestAbstract() {

    init {
        params = arrayOf("--terminal", "--filesystem=../samples/android/testing_parser.txt")
        expectedType = APPLICATION_TYPE.CLI
    }

    @Test
    override fun testApplication() {
        val app = Application()
        Thread(Runnable {
            val type = app.start(params)
            Assert.assertEquals(type, expectedType)
            app.pussy?.configuration?.exitOnStop = false
        }).start()
        Thread.sleep(5000)
        app.stop()
    }

}