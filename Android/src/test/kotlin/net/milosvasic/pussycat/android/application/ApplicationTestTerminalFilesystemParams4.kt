package net.milosvasic.pussycat.android.application

import net.milosvasic.pussycat.application.APPLICATION_TYPE
import net.milosvasic.pussycat.application.PUSSYCAT_MODE


class ApplicationTestTerminalFilesystemParams4 : ApplicationTestAbstract() {

    init {
        params = arrayOf("--terminal", "--filesystem=../samples/android/testing_parser.txt")
        expectedType = APPLICATION_TYPE.CLI
        expectedMode = PUSSYCAT_MODE.FILESYSTEM
    }

}