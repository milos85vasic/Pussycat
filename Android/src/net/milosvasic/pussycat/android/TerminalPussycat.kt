package net.milosvasic.pussycat.android

import net.milosvasic.pussycat.color.Color
import net.milosvasic.pussycat.android.data.LOGCAT_TAG_TYPE

class TerminalPussycat : AndroidPussycat() {

    override fun clear() {
        println(27.toChar() + "[2J")
    }

    override fun printLine(line: String) {
        if (paused.get()) {
            return
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.V_LIVE) || line.containsIgnoreCase(LOGCAT_TAG_TYPE.V_FILESYSTEM)) {
            color = Color.WHITE
            println("$color$line${Color.RESET}")
            return
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.D_LIVE) || line.containsIgnoreCase(LOGCAT_TAG_TYPE.D_FILESYSTEM)) {
            color = Color.YELLOW
            println("$color$line${Color.RESET}")
            return
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.I_LIVE) || line.containsIgnoreCase(LOGCAT_TAG_TYPE.I_FILESYSTEM)) {
            color = Color.CYAN
            println("$color$line${Color.RESET}")
            return
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.W_LIVE) || line.containsIgnoreCase(LOGCAT_TAG_TYPE.W_FILESYSTEM)) {
            color = Color.PURPLE
            println("$color$line${Color.RESET}")
            return
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.E_LIVE) || line.containsIgnoreCase(LOGCAT_TAG_TYPE.E_FILESYSTEM)) {
            color = Color.RED
            println("$color$line${Color.RESET}")
            return
        }
        println("\t$color$line${Color.RESET}")
    }

    override fun status() {
        logger.v(TAG, "Filter applied [ ${getFilter()} ]\n\n")
    }

    fun String.containsIgnoreCase(word: String): Boolean {
        return this.toLowerCase().contains(word.toLowerCase())
    }

}