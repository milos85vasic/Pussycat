package net.milosvasic.pussycat.android.application

import net.milosvasic.pussycat.application.APPLICATION_TYPE
import net.milosvasic.pussycat.PUSSYCAT_MODE


class ApplicationTestTerminalFilesystemParams4 : ApplicationTestAbstract() {

    override fun testApplication() {
        params = arrayOf("--terminal", "--filesystem=${localSample.absolutePath}")
        super.testApplication()
    }

    override fun getExpectedType(): APPLICATION_TYPE {
        return APPLICATION_TYPE.CLI
    }

    override fun getExpectedMode(): PUSSYCAT_MODE? {
        return PUSSYCAT_MODE.FILESYSTEM
    }

}