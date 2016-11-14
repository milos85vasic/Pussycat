package net.milosvasic.pussycat.core


import net.milosvasic.pussycat.color.Color
import net.milosvasic.pussycat.logger
import net.milosvasic.pussycat.utils.Text
import java.io.*
import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.atomic.AtomicBoolean


class Pussycat : PussycatActions {

    private var filter = ""
    private val TAG = Pussycat::class
    private var run = AtomicBoolean(true)
    private var color: String = Color.BLACK
    private val paused = AtomicBoolean(false)
    private var refreshing = AtomicBoolean(false)
    private val data = CopyOnWriteArrayList<String>()

    override fun live() {
        Thread(Runnable {
            Thread.currentThread().name = "Live adb reading thread"
            val process = Runtime.getRuntime().exec("adb logcat")
            val input = process.inputStream
            readLive(input)
            process.destroy()
        }).start()
    }

    override fun filesystem(file: File) {
        Thread(Runnable {
            Thread.currentThread().name = "Filesystem reading thread"
            readFilesystem(FileInputStream(file.absoluteFile))
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

    private fun filterOk(line: String): Boolean {
        if (Text.isEmpty(filter)) {
            return true
        }
        if (!filter.contains("&&") && !filter.contains("||")) {
            if (filter.startsWith("!")) {
                val check = filter.replace("!", "")
                if (line.containsIgnoreCase(check)) {
                    return false
                }
            } else {
                if (!line.containsIgnoreCase(filter)) {
                    return false
                }
            }
            return true
        }
        if (filter.contains("&&")) {
            val params = filter.split("&&")
            for (item in params) {
                var check = item.trim()
                if (check.startsWith("!")) {
                    check = check.replace("!", "")
                    if (line.containsIgnoreCase(check)) {
                        return false
                    }
                } else {
                    if (!line.containsIgnoreCase(check)) {
                        return false
                    }
                }
            }
            return true
        }
        if (filter.contains("||")) {
            val params = filter.split("||")
            for (item in params) {
                var check = item.trim()
                if (check.startsWith("!")) {
                    check = check.replace("!", "")
                    if (!line.containsIgnoreCase(check)) {
                        return true
                    }
                } else {
                    if (line.containsIgnoreCase(check)) {
                        return true
                    }
                }
            }
            return false
        }
        return true
    }

    private fun printLine(line: String) {
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

    override fun printFilter(): String {
        if (Text.isEmpty(filter)) {
            return "No filter applied"
        } else {
            return filter
        }
    }

    private fun readLive(input: InputStream) {
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
                    addData(line)
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

    private fun readFilesystem(input: InputStream) {
        val reader = InputStreamReader(input)
        val buffered = BufferedReader(reader)
        var line = ""
        while (run.get() && line != null) {
            try {
                line = buffered.readLine()
            } catch (e: Exception) {
                run.set(false)
            }
            if (!Text.isEmpty(line)) {
                line = line.trim()
                if (!Text.isEmpty(line)) {
                    addData(line)
                    if (!refreshing.get() && filterOk(line)) {
                        printLine(line)
                    }
                }
            }
        }
        buffered.close()
        reader.close()
        input.close()
    }

    private fun addData(line: String) {
        val tag = getTag(line)
        if (tag != null) {
            val firstTagOccurance = line.indexOf(tag)
            var splited = line.substring(firstTagOccurance + tag.length, line.lastIndex + 1)
            val classEnd = splited.indexOf(":")
            splited = splited.substring(classEnd + 1, splited.lastIndex + 1)
            if (splited.startsWith(" \t")) {
                data[data.lastIndex] = "${data.last()}\n$line"
            } else {
                data.add(line)
            }
        } else {
            if (data.isEmpty()) {
                data.add(line)
            } else {
                val replace = "${data.last()}\n\t$line"
                data[data.lastIndex] = replace
            }
        }
    }

    private fun getTag(line: String): String? {
        if (line.containsIgnoreCase(LogcatTagType.V_LIVE)) {
            return LogcatTagType.V_LIVE
        }
        if (line.containsIgnoreCase(LogcatTagType.V_FILESYSTEM)) {
            return LogcatTagType.V_FILESYSTEM
        }
        if (line.containsIgnoreCase(LogcatTagType.D_LIVE)) {
            return LogcatTagType.D_LIVE
        }
        if (line.containsIgnoreCase(LogcatTagType.D_FILESYSTEM)) {
            return LogcatTagType.D_FILESYSTEM
        }
        if (line.containsIgnoreCase(LogcatTagType.I_LIVE)) {
            return LogcatTagType.I_LIVE
        }
        if (line.containsIgnoreCase(LogcatTagType.I_FILESYSTEM)) {
            return LogcatTagType.I_FILESYSTEM
        }
        if (line.containsIgnoreCase(LogcatTagType.W_LIVE)) {
            return LogcatTagType.W_LIVE
        }
        if (line.containsIgnoreCase(LogcatTagType.W_FILESYSTEM)) {
            return LogcatTagType.W_FILESYSTEM
        }
        if (line.containsIgnoreCase(LogcatTagType.E_LIVE)) {
            return LogcatTagType.E_LIVE
        }
        if (line.containsIgnoreCase(LogcatTagType.E_FILESYSTEM)) {
            return LogcatTagType.E_FILESYSTEM
        }
        return null
    }

    fun String.containsIgnoreCase(word: String): Boolean {
        return this.toLowerCase().contains(word.toLowerCase())
    }

}