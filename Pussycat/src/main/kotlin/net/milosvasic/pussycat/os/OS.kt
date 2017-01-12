package net.milosvasic.pussycat.os


object OS {

    val MACOS = "mac"
    val LINUX = "linux"
    val WINDOWS = "windows"

    fun getOS(): String {
        return System.getProperty("os.name").toLowerCase()
    }

}