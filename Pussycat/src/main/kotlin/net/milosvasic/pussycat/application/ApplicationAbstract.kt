package net.milosvasic.pussycat.application

abstract class ApplicationAbstract {

    abstract fun start(args: Array<String>): APPLICATION_TYPE?

    abstract fun stop()

}