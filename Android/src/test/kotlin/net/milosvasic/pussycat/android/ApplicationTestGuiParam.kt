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
        val type = app.start(params)
        Assert.assertEquals(type, expectedType)
        Thread.sleep(3000)
        app.stop()
    }

}