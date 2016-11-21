package net.milosvasic.pussycat

import net.milosvasic.pussycat.color.Color
import net.milosvasic.pussycat.core.LogcatTagType
import net.milosvasic.pussycat.core.commands.COMMAND
import java.util.concurrent.atomic.AtomicBoolean
import net.milosvasic.pussycat.core.common.DataFilter
import net.milosvasic.pussycat.core.data.Data
import net.milosvasic.pussycat.logging.ConsoleLogger
import net.milosvasic.pussycat.utils.Text
import java.util.concurrent.CopyOnWriteArrayList
import net.milosvasic.pussycat.core.common.Execute
import java.io.*


class Pussycat() : Execute<COMMAND, String>, DataFilter<CopyOnWriteArrayList<String>, String> {

    private val data = Data(this)
    private val TAG = Pussycat::class
    private val run = AtomicBoolean()
    private var logger = ConsoleLogger()
    private var color: String = Color.BLACK
    private val paused = AtomicBoolean(false)
    private var refreshing = AtomicBoolean(false)

    override fun execute(executable: COMMAND, vararg params: String?) {
        when (executable) {
            COMMAND.LIVE -> live()
            COMMAND.FILESYSTEM -> filesystem(params)
            COMMAND.STOP -> stop()
            COMMAND.CLEAR -> clear()
            COMMAND.RESET -> filter()
            COMMAND.PAUSE -> pause()
            COMMAND.RESUME -> resume()
            COMMAND.STATUS -> logger.v(TAG, "Filter applied [ ${getFilter()} ]\n\n")
            else -> logger.w(TAG, "Unknown command: " + executable)
        }
    }

    private fun filesystem(params: Array<out String?>) {
        val logcat = File(params[0] as String)
        if (logcat.exists()) {
            run.set(false)
            Thread(Runnable {
                Thread.currentThread().name = "Filesystem reading thread"
                val input = FileInputStream(logcat.absoluteFile)
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
                            data.addData(line)
                            if (!refreshing.get() && filterOk(line)) {
                                printLine(line)
                            }
                        }
                    }
                }

                buffered.close()
                reader.close()
                input.close()
            }).start()
        } else {
            logger.e(TAG, "Logcat: ${logcat.absoluteFile} does not exist")
            stop()
        }
    }

    fun live() {
        run.set(false)
        Thread(Runnable {
            Thread.currentThread().name = "Live adb reading thread"
            val process = Runtime.getRuntime().exec("adb logcat")
            val input = process.inputStream
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
                        data.addData(line)
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
            process.destroy()
        }).start()
    }

    fun stop() {

    }

    fun pause() {

    }

    fun resume() {

    }

    fun filter() {
        data.apply()
    }

    fun filter(filter: String) {
        data.apply(filter)
    }

    fun getFilter(): String {
        if (Text.isEmpty(data.getFilterPattern())) {
            return "No apply applied"
        } else {
            return data.getFilterPattern()
        }
    }

    fun clear() {
        execute(COMMAND.RESET)
    }

    override fun apply(data: CopyOnWriteArrayList<String>, pattern: String?) {
        // TODO: Implement this.
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

    fun String.containsIgnoreCase(word: String): Boolean {
        return this.toLowerCase().contains(word.toLowerCase())
    }

}