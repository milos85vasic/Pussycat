package net.milosvasic.pussycat.android.resources

import net.milosvasic.pussycat.gui.content.Labels
import net.milosvasic.pussycat.web.PussycatServerResourceProvider
import java.io.InputStream

class WebGuiPussycatServerResourceProvider : PussycatServerResourceProvider() {

    override fun getResource(resource: String): InputStream? {
        return when(resource){
            "${Labels.FOLDER_ASSETS}/${Labels.FILE_FAVICON}" -> {
                javaClass.classLoader.getResourceAsStream(resource)
            }
            else -> super.getResource(resource)
        }
    }
}
