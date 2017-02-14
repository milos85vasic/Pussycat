package net.milosvasic.pussycat.web

import net.milosvasic.dispatcher.Dispatcher
import net.milosvasic.dispatcher.response.Response
import net.milosvasic.dispatcher.response.ResponseFactory
import net.milosvasic.dispatcher.route.RootRouteElement
import net.milosvasic.dispatcher.route.Route
import net.milosvasic.dispatcher.route.RouteElement
import net.milosvasic.pussycat.gui.content.Labels
import net.milosvasic.pussycat.logging.Logger
import java.io.BufferedReader
import java.io.InputStreamReader
import java.util.*


class PussycatServer(port: Int) {

    private val logger = Logger()
    private val TAG = PussycatServer::class
    private val dispatcher = Dispatcher(Labels.PUSSYCAT, port)

    init {
        val rootRoute = Route.Builder()
                .addRouteElement(RootRouteElement())
                .build()

        val homepage = object : ResponseFactory {
            override fun getResponse(params: HashMap<RouteElement, String>): Response {
                val input = javaClass.classLoader.getResourceAsStream("Html/index.html")
                val bufferedReader = BufferedReader(InputStreamReader(input, "UTF-8"))
                var line = bufferedReader.readLine()
                val builder = StringBuilder()
                while (line != null) {
                    builder
                            .append(line)
                            .append("\n")
                    line = bufferedReader.readLine()
                }
                val content = builder.toString()
                //                logger.v(TAG, content) // TODO: Remove this.
                return Response(content)
            }
        }

        dispatcher.registerRoute(rootRoute, homepage)
    }

    @Synchronized
    fun start() {
        dispatcher.start()
    }

    @Synchronized
    fun stop() {
        dispatcher.stop()
    }

}