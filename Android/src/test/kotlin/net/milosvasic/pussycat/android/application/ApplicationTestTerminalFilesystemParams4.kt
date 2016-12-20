package net.milosvasic.pussycat.android.application

import net.milosvasic.pussycat.application.APPLICATION_TYPE
import net.milosvasic.pussycat.application.PUSSYCAT_MODE


class ApplicationTestTerminalFilesystemParams4 : ApplicationTestAbstract() {

    init {
        expectedType = APPLICATION_TYPE.CLI
        expectedMode = PUSSYCAT_MODE.FILESYSTEM
    }

    override fun testApplication() {
        params = arrayOf("--terminal", "--filesystem=${localSample.absolutePath}")
        super.testApplication()
    }

}