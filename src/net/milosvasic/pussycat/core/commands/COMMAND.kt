package net.milosvasic.pussycat.core.commands


enum class COMMAND(val value: String) {
    EXIT("@@exit"),
    PAUSE("@@pause"),
    RESUME("@@resume"),
    CLEAR("@@clear"),
    RESET("@@reset"),
    STATUS("@@status")
}