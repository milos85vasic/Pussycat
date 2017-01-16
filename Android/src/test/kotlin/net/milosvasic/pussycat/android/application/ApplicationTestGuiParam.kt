package net.milosvasic.pussycat.android.application

import net.milosvasic.pussycat.application.APPLICATION_TYPE
import net.milosvasic.pussycat.application.PUSSYCAT_MODE

class ApplicationTestGuiParam : ApplicationTestAbstract() {

    init {
        params = arrayOf("--gui")
    }

    override fun getExpectedMode(): PUSSYCAT_MODE? {
        return PUSSYCAT_MODE.LIVE
    }

    override fun getExpectedType(): APPLICATION_TYPE {
        return APPLICATION_TYPE.GUI
    }

}