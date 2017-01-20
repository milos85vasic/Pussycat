package net.milosvasic.pussycat.android.application

import net.milosvasic.pussycat.application.APPLICATION_TYPE
import net.milosvasic.pussycat.PUSSYCAT_MODE

class ApplicationTestTerminalFilesystemParams3  : ApplicationTestAbstract() {

    init {
        params = arrayOf("--terminal", "--filesystem===")
    }

    override fun getExpectedType(): APPLICATION_TYPE {
        return APPLICATION_TYPE.CLI
    }

    override fun getExpectedMode(): PUSSYCAT_MODE? {
        return null
    }

}