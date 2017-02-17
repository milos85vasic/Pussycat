package net.milosvasic.pussycat.web

import java.io.InputStream

interface ResourceProvider {

    fun getResource(resource: String): InputStream?

}