package net.milosvasic.pussycat.android

import net.milosvasic.pussycat.android.application.Application
import net.milosvasic.pussycat.application.APPLICATION_TYPE
import org.junit.Assert
import org.junit.Test


class ApplicationTestGuiParam : ApplicationTestAbstract() {

    init {
        params = arrayOf("--gui")
        expectedType = APPLICATION_TYPE.GUI
    }

    @Test
    override fun testApplication() {
        val app = Application()
        Thread(Runnable {
            val type = app.start(params)
            Assert.assertEquals(type, expectedType)
        }).start()
        Thread.sleep(5000)
        app.stop()
    }

}