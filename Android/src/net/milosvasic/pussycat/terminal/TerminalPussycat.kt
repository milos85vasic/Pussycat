package net.milosvasic.pussycat.terminal

import net.milosvasic.pussycat.color.Color
import net.milosvasic.pussycat.core.LogcatTagType

class TerminalPussycat : AndroidPussycat() {

    override fun printLine(line: String) {
        if (paused.get()) {
            return
        }
        if (line.containsIgnoreCase(LogcatTagType.V_LIVE) || line.containsIgnoreCase(LogcatTagType.V_FILESYSTEM)) {
            color = Color.WHITE
            println("$color$line${Color.RESET}")
            return
        }
        if (line.containsIgnoreCase(LogcatTagType.D_LIVE) || line.containsIgnoreCase(LogcatTagType.D_FILESYSTEM)) {
            color = Color.YELLOW
            println("$color$line${Color.RESET}")
            return
        }
        if (line.containsIgnoreCase(LogcatTagType.I_LIVE) || line.containsIgnoreCase(LogcatTagType.I_FILESYSTEM)) {
            color = Color.CYAN
            println("$color$line${Color.RESET}")
            return
        }
        if (line.containsIgnoreCase(LogcatTagType.W_LIVE) || line.containsIgnoreCase(LogcatTagType.W_FILESYSTEM)) {
            color = Color.PURPLE
            println("$color$line${Color.RESET}")
            return
        }
        if (line.containsIgnoreCase(LogcatTagType.E_LIVE) || line.containsIgnoreCase(LogcatTagType.E_FILESYSTEM)) {
            color = Color.RED
            println("$color$line${Color.RESET}")
            return
        }
        println("\t$color$line${Color.RESET}")
    }

    fun String.containsIgnoreCase(word: String): Boolean {
        return this.toLowerCase().contains(word.toLowerCase())
    }

}