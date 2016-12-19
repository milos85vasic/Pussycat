package net.milosvasic.pussycat.android

import net.milosvasic.pussycat.application.APPLICATION_TYPE

abstract class ApplicationTestAbstract {

    lateinit var params: Array<String>
    lateinit var expectedType: APPLICATION_TYPE

    abstract fun testApplication()

}