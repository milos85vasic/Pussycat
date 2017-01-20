package net.milosvasic.pussycat.android.application

import net.milosvasic.pussycat.application.APPLICATION_TYPE
import net.milosvasic.pussycat.PUSSYCAT_MODE

class ApplicationTestNoParams : ApplicationTestAbstract() {

    init {
        params = arrayOf<String>()
    }

    override fun getExpectedType(): APPLICATION_TYPE {
        return APPLICATION_TYPE.GUI
    }

    override fun getExpectedMode(): PUSSYCAT_MODE? {
        return PUSSYCAT_MODE.LIVE
    }

}