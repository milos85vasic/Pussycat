package net.milosvasic.pussycat.application

import net.milosvasic.pussycat.content.Messages


abstract class ApplicationAbstract(val args: Array<String>) {

    companion object {
        private var instance: ApplicationAbstract? = null
    }

    lateinit var information: ApplicationInformation
        protected set

    open fun start() {
        if (instance != null) {
            throw IllegalStateException(Messages.APPLICATION_ALREADY_RUNNING)
        }
        instance = this
    }

    open fun stop() {
        if (instance == null) {
            throw IllegalStateException(Messages.APPLICATION_NOT_RUNNING)
        }
        instance = null
    }

}