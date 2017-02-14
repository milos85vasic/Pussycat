package net.milosvasic.pussycat.web

import net.milosvasic.dispatcher.Dispatcher
import net.milosvasic.pussycat.gui.content.Labels


class PussycatServer(port: Int) {

    private val dispatcher = Dispatcher(Labels.PUSSYCAT, port)

    fun start() {
        dispatcher.start()
    }

    fun stop() {
        dispatcher.stop()
    }

}