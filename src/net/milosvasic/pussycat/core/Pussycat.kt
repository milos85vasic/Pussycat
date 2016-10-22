package net.milosvasic.pussycat.core


import net.milosvasic.pussycat.color.Color
import net.milosvasic.pussycat.logger
import net.milosvasic.pussycat.utils.Text
import java.io.BufferedReader
import java.io.File
import java.io.InputStreamReader
import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.atomic.AtomicBoolean

/**
 * Pussycat main class
 */
class Pussycat : PussycatActions {

    var filter = ""
    val TAG = Pussycat::class
    var run = AtomicBoolean(true)
    var refreshing = AtomicBoolean(false)
    val data = CopyOnWriteArrayList<String>()

    override fun live() {
        Thread(Runnable {
            var line: String
            val process = Runtime.getRuntime().exec("adb logcat")
            val input = process.inputStream
            val buffered = BufferedReader(InputStreamReader(input))
            while (run.get()) {
                line = buffered.readLine()
                if (!Text.isEmpty(line)) {
                    line = line.trim()
                    if (!Text.isEmpty(line)) {
                        data.add(line)
                        if (!refreshing.get() && filterOk(line)) {
                            printLine(line)
                        }
                    } else {
                        Thread.sleep(1000)
                    }
                } else {
                    Thread.sleep(1000)
                }
            }
            process.destroy()
        }).start()
    }

    override fun filesystem(file: File) {
        logger.v(TAG, "file system")
    }

    override fun stop() {
        run.set(false)
    }

    override fun filter() {
        filter = ""
        applyFilter()
    }

    override fun filter(filter: String) {
        this.filter = filter
        applyFilter()
    }

    private fun filterOk(line: String): Boolean {
        if (Text.isEmpty(filter)) {
            return true
        }
        if (!filter.contains("&&") && !filter.contains("||")) {
            return line.contains(filter)
        }
        if (filter.contains("&&")) {
            val params = filter.split("&&")
            for (item in params) {
                val check = item.trim()
                if (!line.contains(check)) {
                    return false
                }
            }
            return true
        }
        if (filter.contains("||")) {
            val params = filter.split("||")
            for (item in params) {
                val check = item.trim()
                if (line.contains(check)) {
                    return true
                }
            }
            return false
        }
        return true
    }

    private fun applyFilter() {
        refreshing.set(true)
        println(27.toChar() + "[2J")
        if (data.isEmpty()) {
            logger.w(TAG, "No data available, filter [ $filter ]")
        } else {
            var x = 0
            for (line in data) {
                if (filterOk(line)) {
                    printLine(line)
                    x++
                }
            }
            if (x == 0) logger.w(TAG, "No data matching, filter [ $filter ]")
        }
        refreshing.set(false)
    }

    private fun printLine(line: String) {
        if (line.contains(" V/") || line.contains(" V ")) {
            println("${Color.WHITE}$line${Color.RESET}")
            return
        }
        if (line.contains(" D/") || line.contains(" D ")) {
            println("${Color.YELLOW}$line${Color.RESET}")
            return
        }
        if (line.contains(" I/") || line.contains(" I ")) {
            println("${Color.CYAN}$line${Color.RESET}")
            return
        }
        if (line.contains(" W/") || line.contains(" W ")) {
            println("${Color.PURPLE}$line${Color.RESET}")
            return
        }
        if (line.contains(" E/") || line.contains(" E ")) {
            println("${Color.RED}$line${Color.RESET}")
            return
        }
        println(line)
    }

    override fun printFilter(): String {
        if (Text.isEmpty(filter)) {
            return "No filter applied"
        } else {
            return filter
        }
    }

}