package net.milosvasic.pussycat.core

import java.io.File

/**
 * Base action we may perform with logcat
 */
interface LogcatActions {

    fun live()

    fun filesystem(file: File)

}