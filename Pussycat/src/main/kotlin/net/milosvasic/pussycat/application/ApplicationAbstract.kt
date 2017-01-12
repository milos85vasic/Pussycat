package net.milosvasic.pussycat.application

abstract class ApplicationAbstract(val args: Array<String>) {

    abstract fun start()

    abstract fun stop()

    abstract fun getApplicationInformation(): ApplicationInformation

}