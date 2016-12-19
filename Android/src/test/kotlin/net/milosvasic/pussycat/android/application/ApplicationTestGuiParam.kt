package net.milosvasic.pussycat.android.application

import net.milosvasic.pussycat.application.APPLICATION_TYPE

class ApplicationTestGuiParam : ApplicationTestAbstract() {

    init {
        params = arrayOf("--gui")
        expectedType = APPLICATION_TYPE.GUI
    }

}