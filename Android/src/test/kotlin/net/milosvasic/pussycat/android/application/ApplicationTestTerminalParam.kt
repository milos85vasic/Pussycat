package net.milosvasic.pussycat.android.application

import net.milosvasic.pussycat.application.APPLICATION_TYPE
import net.milosvasic.pussycat.application.PUSSYCAT_MODE

class ApplicationTestTerminalParam : ApplicationTestAbstract() {

    init {
        params = arrayOf("--terminal")
        expectedType = APPLICATION_TYPE.CLI
        expectedMode = PUSSYCAT_MODE.LIVE
    }

}