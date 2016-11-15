package net.milosvasic.pussycat.core

import java.io.File

interface PussycatActions {

    fun live()

    fun filesystem(file: File)

    fun stop()

    fun pause()

    fun resume()

    fun filter()

    fun filter(filter: String)

    fun getFilter(): String

    fun clear()

}