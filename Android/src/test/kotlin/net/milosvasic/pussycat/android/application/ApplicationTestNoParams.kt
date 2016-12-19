package net.milosvasic.pussycat.android.application

import net.milosvasic.pussycat.application.APPLICATION_TYPE

class ApplicationTestNoParams : ApplicationTestAbstract() {

    init {
        params = arrayOf<String>()
        expectedType = APPLICATION_TYPE.GUI
    }

}