package net.milosvasic.pussycat.core

import java.io.File

/**
 * Base action we may perform with logcat
 */
interface PussycatActions {

    fun live()

    fun filesystem(file: File)

    fun stop()

    fun pause()

    fun resume()

    fun filter()

    fun filter(filter: String)

    fun printFilter(): String

    fun clear()

}