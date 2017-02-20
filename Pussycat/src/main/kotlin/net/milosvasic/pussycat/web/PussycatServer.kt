package net.milosvasic.pussycat.web

import net.milosvasic.dispatcher.Dispatcher
import net.milosvasic.dispatcher.response.Response
import net.milosvasic.dispatcher.response.ResponseFactory
import net.milosvasic.dispatcher.response.assets.AssetFactory
import net.milosvasic.dispatcher.response.assets.application.AssetJS
import net.milosvasic.dispatcher.response.assets.image.AssetICON
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
        val assetParameter = DynamicRouteElement(Labels.ROUTE_ASSET)

        val rootRoute = Route.Builder()
                .addRouteElement(RootRouteElement())
                .build()

        val javascriptRoute = AssetsRoute.Builder()
                .addRouteElement(StaticRouteElement(Labels.FOLDER_ASSETS))
                .addRouteElement(StaticRouteElement(Labels.FOLDER_JAVASCRIPT))
                .addRouteElement(assetParameter)
                .build()

        val iconsRoute = AssetsRoute.Builder()
                .addRouteElement(StaticRouteElement(Labels.FOLDER_ASSETS))
                .addRouteElement(StaticRouteElement(Labels.FOLDER_ICONS))
                .addRouteElement(assetParameter)
                .build()

        val homepage = object : ResponseFactory {
            override fun getResponse(params: HashMap<String, String>): Response {
                val input = resourceProvider.getResource("${Labels.FOLDER_WEB_ROOT}/${Labels.PAGE_WEB_ROOT}")
                val bytes = input?.readBytes()
                if (bytes != null) {
                    return Response(String(bytes))
                }
                return Response("")
            }
        }

        val javascriptFactory = object : AssetFactory {
            override fun getContent(params: HashMap<String, String>): AssetJS {
                val assetName = params[assetParameter.name]
                val input = resourceProvider.getResource(
                        "${Labels.FOLDER_ASSETS}/${Labels.FOLDER_JAVASCRIPT}/$assetName"
                )
                return AssetJS(getBytes(input))
            }
        }

        val iconsFactory = object : AssetFactory {
            override fun getContent(params: HashMap<String, String>): AssetICON {
                val assetName = params[assetParameter.name]
                val input = resourceProvider.getResource(
                        "${Labels.FOLDER_ASSETS}/${Labels.FOLDER_ICONS}/$assetName"
                )
                return AssetICON(getBytes(input))
            }
        }

        dispatcher.registerRoute(rootRoute, homepage)
        dispatcher.registerRoute(iconsRoute, iconsFactory)
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