package net.milosvasic.pussycat.android.resources


import net.milosvasic.pussycat.resources.ResourceProvider
import java.io.InputStream

class LocalResourceProvider : ResourceProvider() {

    override fun getResource(resource: String): InputStream? {
        var result = javaClass.classLoader.getResourceAsStream(resource)
        if (result == null) {
            result = super.getResource(resource)
        }
        return result
    }

}
