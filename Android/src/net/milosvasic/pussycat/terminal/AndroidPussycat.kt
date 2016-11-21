package net.milosvasic.pussycat.terminal

import net.milosvasic.pussycat.PussycatAbstract
import net.milosvasic.pussycat.logging.ConsoleLogger
import net.milosvasic.pussycat.utils.Text
import java.io.BufferedReader
import java.io.File
import java.io.FileInputStream
import java.io.InputStreamReader
import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.atomic.AtomicBoolean


abstract class AndroidPussycat : PussycatAbstract() {

    private val run = AtomicBoolean()
    protected val paused = AtomicBoolean(false)
    protected var refreshing = AtomicBoolean(false)

    init {
        data = Data(this)
        logger = ConsoleLogger()
        TAG = AndroidPussycat::class
    }

    override fun live() {
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
                        if (!refreshing.get() && data.filterOk(line)) {
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

    override fun filesystem(params: Array<out String?>) {
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
                            if (!refreshing.get() && data.filterOk(line)) {
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

    override fun stop() {
        run.set(false)
    }

    override fun pause() {
        paused.set(true)
    }

    override fun resume() {
        apply(data.get(), data.getFilterPattern())
    }

    override fun apply(data: CopyOnWriteArrayList<String>, pattern: String?) {
        refreshing.set(true)
        paused.set(false)
        println(27.toChar() + "[2J")
        if (data.isEmpty()) {
            logger.w(TAG, "No data available [ filter: ${this.data.getFilterPattern()} ]")
        } else {
            var x = 0
            for (line in data) {
                if (this.data.filterOk(line)) {
                    printLine(line)
                    x++
                }
            }
            if (x == 0) logger.w(TAG, "No data matching [ filter: ${this.data.getFilterPattern()} ]")
        }
        refreshing.set(false)
    }

    abstract protected fun printLine(line: String)

}