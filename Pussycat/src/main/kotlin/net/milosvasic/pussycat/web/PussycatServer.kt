package net.milosvasic.pussycat.web

import net.milosvasic.dispatcher.Dispatcher
import net.milosvasic.dispatcher.response.Asset
import net.milosvasic.dispatcher.response.AssetFactory
import net.milosvasic.dispatcher.response.Response
import net.milosvasic.dispatcher.response.ResponseFactory
import net.milosvasic.dispatcher.route.*
import net.milosvasic.pussycat.gui.content.Labels
import net.milosvasic.pussycat.resources.ResourceProvider
import java.io.InputStream
import java.util.*


class PussycatServer(port: Int, resourceProvider: ResourceProvider) {

    //    private val logger = Logger()
    //    private val TAG = PussycatServer::class
    private val dispatcher = Dispatcher(Labels.PUSSYCAT, port)

    init {
        val rootRoute = Route.Builder()
                .addRouteElement(RootRouteElement())
                .build()

        val assetParameter = DynamicRouteElement(Labels.ROUTE_ASSET)
        val assetsRoute = AssetsRoute.Builder()
                .addRouteElement(StaticRouteElement(Labels.FOLDER_ASSETS))
                .addRouteElement(assetParameter)
                .build()

        val javascriptRoute = AssetsRoute.Builder()
                .addRouteElement(StaticRouteElement(Labels.FOLDER_ASSETS))
                .addRouteElement(StaticRouteElement(Labels.FOLDER_JAVASCRIPT))
                .addRouteElement(assetParameter)
                .build()

        val homepage = object : ResponseFactory {
            override fun getResponse(params: HashMap<RouteElement, String>): Response {
                val input = resourceProvider.getResource("${Labels.FOLDER_WEB_ROOT}/${Labels.PAGE_WEB_ROOT}")
                val bytes = input?.readBytes()
                if (bytes != null) {
                    return Response(String(bytes))
                }
                return Response("")
            }
        }

        val assetsFactory = object : AssetFactory {
            override fun getContent(params: HashMap<RouteElement, String>): Asset {
                val assetName = params[assetParameter]
                val input = resourceProvider.getResource("${Labels.FOLDER_ASSETS}/$assetName")
                return Asset(getBytes(input))
            }
        }

        val javascriptFactory = object : AssetFactory {
            override fun getContent(params: HashMap<RouteElement, String>): Asset {
                val assetName = params[assetParameter]
                val input = resourceProvider.getResource(
                        "${Labels.FOLDER_ASSETS}/${Labels.FOLDER_JAVASCRIPT}/$assetName"
                )
                return Asset(getBytes(input))
            }
        }

        dispatcher.registerRoute(rootRoute, homepage)
        dispatcher.registerRoute(assetsRoute, assetsFactory)
        dispatcher.registerRoute(javascriptRoute, javascriptFactory)
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