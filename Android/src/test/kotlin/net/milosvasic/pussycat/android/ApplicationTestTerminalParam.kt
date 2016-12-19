package net.milosvasic.pussycat.android

import net.milosvasic.pussycat.android.application.Application
import net.milosvasic.pussycat.application.APPLICATION_TYPE
import org.junit.Assert
import org.junit.Test


class ApplicationTestTerminalParam : ApplicationTestAbstract() {

    init {
        params = arrayOf("--terminal")
        expectedType = APPLICATION_TYPE.CLI
    }

    @Test
    override fun testApplication() {
        val app = Application()
        Thread(Runnable {
            val type = app.start(params)
            app.pussy?.configuration?.exitOnStop = false
            Assert.assertEquals(type, expectedType)
        }).start()
        Thread.sleep(5000)
        app.stop()
    }

}