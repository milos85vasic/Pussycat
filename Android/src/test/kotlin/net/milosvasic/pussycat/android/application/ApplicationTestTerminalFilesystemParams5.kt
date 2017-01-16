package net.milosvasic.pussycat.android.application

import net.milosvasic.pussycat.application.APPLICATION_TYPE
import net.milosvasic.pussycat.application.PUSSYCAT_MODE


class ApplicationTestTerminalFilesystemParams5 : ApplicationTestAbstract() {

    init {
        params = arrayOf("--terminal", "--filesystem=../samples/android/doesnt_exist.txt")
    }

    override fun getExpectedMode(): PUSSYCAT_MODE? {
        return null
    }

    override fun getExpectedType(): APPLICATION_TYPE {
        return APPLICATION_TYPE.CLI
    }

}