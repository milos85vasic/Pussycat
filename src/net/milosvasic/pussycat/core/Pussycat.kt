package net.milosvasic.pussycat.core


import net.milosvasic.pussycat.logger
import java.io.File

/**
 * Pussycat main class
 */
class Pussycat : PussycatActions {

    val TAG = Pussycat::class

    override fun live() {
        logger.v(TAG, "live")
    }

    override fun filesystem(file: File) {
        logger.v(TAG, "file system")
    }

    override fun stop() {
        logger.v(TAG, "stop pussy")
    }

    override fun filter() {
        logger.v(TAG, "filter")
    }

    override fun filter(filter: String) {
        logger.v(TAG, "filter $filter")
    }

}