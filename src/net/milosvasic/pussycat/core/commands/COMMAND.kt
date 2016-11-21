package net.milosvasic.pussycat.core.commands


enum class COMMAND(val value: String) {
    LIVE("@@live"),
    FILESYSTEM("@@filesystem"),
    PAUSE("@@pause"),
    RESUME("@@resume"),
    STOP("@@stop"),
    CLEAR("@@clear"),
    RESET("@@reset"),
    STATUS("@@status")
}