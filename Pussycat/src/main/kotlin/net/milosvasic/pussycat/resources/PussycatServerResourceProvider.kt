package net.milosvasic.pussycat.resources

import net.milosvasic.pussycat.gui.content.Labels
import net.milosvasic.pussycat.web.ResourceProvider
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
