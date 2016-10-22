package net.milosvasic.pussycat.core


import net.milosvasic.pussycat.color.Color
import net.milosvasic.pussycat.logger
import net.milosvasic.pussycat.utils.Text
import java.io.*
import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.atomic.AtomicBoolean

/**
 * Pussycat main class
 */
class Pussycat : PussycatActions {

    private var filter = ""
    private val TAG = Pussycat::class
    private var run = AtomicBoolean(true)
    private val paused = AtomicBoolean(false)
    private var refreshing = AtomicBoolean(false)
    private val data = CopyOnWriteArrayList<String>()

    override fun live() {
        Thread(Runnable {
            Thread.currentThread().name = "Live adb reading thread"
            val process = Runtime.getRuntime().exec("adb logcat")
            val input = process.inputStream
            read(input)
            process.destroy()
        }).start()
    }

    override fun filesystem(file: File) {
        Thread(Runnable {
            Thread.currentThread().name = "Filesystem reading thread"
            read(FileInputStream(file.absoluteFile))
        }).start()
    }

    override fun stop() {
        run.set(false)
    }

    override fun pause() {
        paused.set(true)
    }

    override fun resume() {
        applyFilter()
    }

    override fun filter() {
        filter = ""
        applyFilter()
    }

    override fun filter(filter: String) {
        this.filter = filter
        applyFilter()
    }

    override fun clear() {
        println(27.toChar() + "[2J")
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
                if (line.startsWith("!")) {
                    if (line.contains(check)) {
                        return false
                    }
                } else {
                    if (!line.contains(check)) {
                        return false
                    }
                }
            }
            return true
        }
        if (filter.contains("||")) {
            val params = filter.split("||")
            for (item in params) {
                val check = item.trim()
                if (line.startsWith("!")) {
                    if (!line.contains(check)) {
                        return true
                    }
                } else {
                    if (line.contains(check)) {
                        return true
                    }
                }
            }
            return false
        }
        return true
    }

    private fun applyFilter() {
        refreshing.set(true)
        paused.set(false)
        clear()
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
        if (paused.get()) {
            return
        }
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

    private fun read(input: InputStream) {
        var line = ""
        val reader = InputStreamReader(input)
        val buffered = BufferedReader(reader)
        while (run.get()) {
            try {
                line = buffered.readLine()
            } catch (e: Exception) {
                run.set(false)
            }
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
        buffered.close()
        reader.close()
        input.close()
    }

}