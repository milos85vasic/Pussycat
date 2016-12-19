package net.milosvasic.pussycat.android

import net.milosvasic.pussycat.android.application.Application
import net.milosvasic.pussycat.application.APPLICATION_TYPE
import org.junit.Assert
import org.junit.Test

class ApplicationTestNoParams : ApplicationTestAbstract() {

    init {
        params = arrayOf<String>()
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