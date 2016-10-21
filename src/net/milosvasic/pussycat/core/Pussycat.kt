package net.milosvasic.pussycat.core


import java.io.File

/**
 * Pussycat main class
 */
class Pussycat : LogcatActions {

    override fun live() {
        println("live")
    }

    override fun filesystem(file: File) {
        println("file system")
    }

}