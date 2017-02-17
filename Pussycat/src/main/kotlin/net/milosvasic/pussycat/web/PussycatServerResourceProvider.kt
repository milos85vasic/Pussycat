package net.milosvasic.pussycat.web

import net.milosvasic.pussycat.gui.content.Labels
import java.io.InputStream

abstract class PussycatServerResourceProvider : ResourceProvider {

    override fun getResource(resource: String): InputStream? {
        return when (resource) {
            "${Labels.FOLDER_WEB_ROOT}/${Labels.PAGE_WEB_ROOT}" -> {
                javaClass.classLoader.getResourceAsStream(resource)
            }
            else -> null
        }
    }

}
