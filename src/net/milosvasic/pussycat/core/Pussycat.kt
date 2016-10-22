package net.milosvasic.pussycat.core


import java.io.File

/**
 * Pussycat main class
 */
class Pussycat : PussycatActions {

    override fun live() {
        println("live")
    }

    override fun filesystem(file: File) {
        println("file system")
    }

    override fun stop() {
        print("stop")
    }

    override fun filter() {
        println("filter")
    }

    override fun filter(filter: String) {
        println("filter $filter")
    }

}