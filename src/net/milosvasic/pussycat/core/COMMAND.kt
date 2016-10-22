package net.milosvasic.pussycat.core

/**
 * Defines commands may be sent to Pussycat.
 */
enum class COMMAND(val value: String) {
    STOP("@@stop"),
    CLEAR("@@clear"),
    RESET("@@reset")
}