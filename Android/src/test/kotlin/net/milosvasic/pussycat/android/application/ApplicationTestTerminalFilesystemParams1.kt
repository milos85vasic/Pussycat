package net.milosvasic.pussycat.android.application

import net.milosvasic.pussycat.application.APPLICATION_TYPE

class ApplicationTestTerminalFilesystemParams1 : ApplicationTestAbstract() {

    init {
        params = arrayOf("--terminal", "--filesystem")
        expectedType = APPLICATION_TYPE.CLI
    }

}