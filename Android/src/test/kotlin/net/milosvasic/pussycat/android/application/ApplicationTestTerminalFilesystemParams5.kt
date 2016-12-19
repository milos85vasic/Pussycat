package net.milosvasic.pussycat.android.application

import net.milosvasic.pussycat.application.APPLICATION_TYPE


class ApplicationTestTerminalFilesystemParams5 : ApplicationTestAbstract() {

    init {
        params = arrayOf("--terminal", "--filesystem=../samples/android/doesnt_exist.txt")
        expectedType = APPLICATION_TYPE.CLI
    }

}