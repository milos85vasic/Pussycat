package net.milosvasic.pussycat.android

import net.milosvasic.pussycat.android.application.Application
import net.milosvasic.pussycat.application.APPLICATION_TYPE
import org.junit.Assert
import org.junit.Test

class ApplicationTestTerminalFilesystemParams1 : ApplicationTestAbstract() {

    init {
        params = arrayOf("--terminal", "--filesystem")
        expectedType = APPLICATION_TYPE.CLI
    }

    @Test
    override fun testApplication() {
        val app = Application(params)
        app.pussy?.configuration?.exitOnStop = false
        Thread(Runnable {
            app.start()
            Assert.assertEquals(app.type, expectedType)
        }).start()
        Thread.sleep(5000)
        app.stop()
    }

}