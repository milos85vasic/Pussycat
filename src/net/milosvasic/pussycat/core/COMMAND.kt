package net.milosvasic.pussycat.core

/**
 * Defines commands may be sent to Pussycat.
 */
enum class COMMAND(val value: String) {
    EXIT("@@exit"),
    PAUSE("@@pause"),
    RESUME("@@resume"),
    CLEAR("@@clear"),
    RESET("@@reset")
}