package net.milosvasic.pussycat.web

import net.milosvasic.dispatcher.Dispatcher
import net.milosvasic.dispatcher.response.Asset
import net.milosvasic.dispatcher.response.AssetFactory
import net.milosvasic.dispatcher.response.Response
import net.milosvasic.dispatcher.response.ResponseFactory
import net.milosvasic.dispatcher.route.*
import net.milosvasic.pussycat.gui.content.Labels
import java.io.InputStream
import java.util.*


class PussycatServer(port: Int, resourceProvider: PussycatServerResourceProvider) {

    //    private val logger = Logger()
    //    private val TAG = PussycatServer::class
    private val dispatcher = Dispatcher(Labels.PUSSYCAT, port)

    init {
        val rootRoute = Route.Builder()
                .addRouteElement(RootRouteElement())
                .build()

        val assets = StaticRouteElement(Labels.FOLDER_ASSETS)
        val assetsRoute = AssetsRoute.Builder()
                .addRouteElement(assets)
                .addRouteElement(DynamicRouteElement(Labels.ROUTE_ASSET))
                .build()

        val homepage = object : ResponseFactory {
            override fun getResponse(params: HashMap<RouteElement, String>): Response {
                val input = resourceProvider.getResource("${Labels.FOLDER_WEB_ROOT}/${Labels.PAGE_WEB_ROOT}")
                return Response(String(input.readBytes()))
            }
        }

        val assetsFactory = object : AssetFactory {
            override fun getContent(params: HashMap<RouteElement, String>): Asset {
                val assetName = params[assets]
                val input = resourceProvider.getResource("${Labels.FOLDER_ASSETS}/$assetName")
                return Asset(getBytes(input), 200)
            }
        }

        dispatcher.registerRoute(rootRoute, homepage)
        dispatcher.registerRoute(assetsRoute, assetsFactory)
    }

    @Synchronized
    fun start() {
        dispatcher.start()
    }

    @Synchronized
    fun stop() {
        dispatcher.stop()
    }

    private fun getBytes(input: InputStream?): ByteArray? {
        return input?.readBytes()
    }

}