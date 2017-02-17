package net.milosvasic.pussycat.resources

import net.milosvasic.pussycat.web.ResourceProvider
import java.io.InputStream

abstract class ResourceProvider : ResourceProvider {

    override fun getResource(resource: String): InputStream? {
        return javaClass.classLoader.getResourceAsStream(resource)
    }

}
