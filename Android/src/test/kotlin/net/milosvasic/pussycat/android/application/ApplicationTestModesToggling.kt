package net.milosvasic.pussycat.android.application

import net.milosvasic.pussycat.application.APPLICATION_TYPE
import net.milosvasic.pussycat.application.PUSSYCAT_MODE
import net.milosvasic.pussycat.core.COMMAND
import org.junit.Assert
import org.junit.Test


class ApplicationTestModesToggling : ApplicationTestAbstract() {

    init {
        params = arrayOf("--terminal")
        expectedMode = PUSSYCAT_MODE.LIVE
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
        Thread.sleep(3000)
        Assert.assertEquals(app.pussy?.getPussycatMode(), expectedMode)
        for (x in 0..10) {
            if (x % 2 == 0) {
                val file: String
                if (x >= 5) {
                    file = "$x.txt"
                } else {
                    file = "testing_parser.txt"
                    expectedMode = PUSSYCAT_MODE.FILESYSTEM
                }
                app.pussy?.execute(COMMAND.FILESYSTEM, arrayOf("../samples/android/$file"))
            } else {
                app.pussy?.execute(COMMAND.LIVE)
                expectedMode = PUSSYCAT_MODE.LIVE
            }
            Thread.sleep(3000)
            Assert.assertEquals(app.pussy?.getPussycatMode(), expectedMode)
        }
        app.stop()
    }

}