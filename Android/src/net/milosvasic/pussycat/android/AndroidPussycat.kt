package net.milosvasic.pussycat.android

import com.android.ddmlib.AndroidDebugBridge
import com.android.ddmlib.IDevice
import com.android.ddmlib.logcat.LogCatListener
import com.android.ddmlib.logcat.LogCatMessage
import com.android.ddmlib.logcat.LogCatReceiverTask
import net.milosvasic.pussycat.PussycatAbstract
import net.milosvasic.pussycat.android.data.AndroidData
import net.milosvasic.pussycat.core.COMMAND
import net.milosvasic.pussycat.logging.ConsoleLogger
import java.io.File
import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.atomic.AtomicBoolean


abstract class AndroidPussycat : PussycatAbstract<LogCatMessage, AndroidData>() {

    protected var device: IDevice?
    protected val paused = AtomicBoolean(false)
    protected var refreshing = AtomicBoolean(false)
    protected var logcatTask: LogCatReceiverTask? = null
    protected lateinit var deviceChangeListener: AndroidDebugBridge.IDeviceChangeListener

    protected val logcatListener = LogCatListener { messages ->
        if (messages != null) {
            data.addData(messages)
            if (!refreshing.get()) {
                for (message in messages) {
                    if (data.evaluate(message)) printLine(message)
                }
            }
        }
    }

    init {
        device = null
        data = AndroidData(this)
        logger = ConsoleLogger()
        TAG = AndroidPussycat::class
    }

    override fun live() {
        Thread(Runnable {
            Thread.currentThread().name = "Live adb reading thread"
            deviceChangeListener = object : AndroidDebugBridge.IDeviceChangeListener {
                override fun deviceChanged(iDevice: IDevice?, changeMask: Int) {
                }

                override fun deviceConnected(iDevice: IDevice?) {
                    println("Device connected [ $iDevice ]")
                    assignDevice()
                }

                override fun deviceDisconnected(iDevice: IDevice?) {
                    println("Device disconnected [ $iDevice ]")
                }
            }
            AndroidDebugBridge.init(false)
            try {
                val debugBridge = AndroidDebugBridge.createBridge("adb", true)
                if (debugBridge == null) {
                    logger.e(TAG, "Invalid ADB path")
                } else {
                    AndroidDebugBridge.addDeviceChangeListener(deviceChangeListener)
                }
            } catch (e: Exception) {
                logger.e(TAG, "ADB error: $e")
            }
        }).start()
    }

    override fun filesystem(params: Array<out String?>) {
        if (params.isEmpty()) {
            return
        }
        val logcat = File(params[0] as String)
        if (logcat.exists()) {
            Thread(Runnable {
                Thread.currentThread().name = "Filesystem reading thread"
                val rawLines = logcat.readLines()
                val parsedLines = mutableListOf<String>()
                for (line in rawLines) {
                    val tag = getTag(line)
                    if (tag != null) {
                        val firstTagOccurance = line.indexOf(tag)
                        var splited = line.substring(firstTagOccurance + tag.length, line.lastIndex + 1)
                        val classEnd = splited.indexOf(":")
                        splited = splited.substring(classEnd + 1, splited.lastIndex + 1)
                        if (splited.startsWith(" \t")) {
                            parsedLines[parsedLines.lastIndex] = "${parsedLines.last()}\n$line"
                        } else {
                            parsedLines.add(line)
                        }
                    } else {
                        if (parsedLines.isEmpty()) {
                            parsedLines.add(line)
                        } else {
                            val replace = "${parsedLines.last()}\n\t$line"
                            parsedLines[parsedLines.lastIndex] = replace
                        }
                    }
                }
                data.addData(Array(parsedLines.size, { i -> parsedLines[i] }))
                if (!refreshing.get()) {
                    for (message in data.get()) {
                        if (data.evaluate(message)) printLine(message)
                    }
                }
            }).start()
        } else {
            println("Logcat: ${logcat.absoluteFile} does not exist")
            stop()
        }
    }

    override fun stop() {
        stopLogsReceiving()
        super.stop()
    }

    override fun pause() {
        if (paused.get()) {
            return
        }
        paused.set(true)
        super.pause()
    }

    override fun resume() {
        apply(data.get(), data.getFilterPattern())
        super.resume()
    }

    override fun apply(data: CopyOnWriteArrayList<LogCatMessage>, pattern: String?) {
        refreshing.set(true)
        paused.set(false)
        println(27.toChar() + "[2J")
        if (data.isEmpty()) {
            logger.w(TAG, "No data available [ filter: ${this.data.getFilterPattern()} ]")
        } else {
            var x = 0
            for (line in data) {
                if (this.data.evaluate(line)) {
                    printLine(line)
                    x++
                }
            }
            if (x == 0) logger.w(TAG, "No data matching [ filter: ${this.data.getFilterPattern()} ]")
        }
        refreshing.set(false)
    }

    abstract protected fun printLine(line: LogCatMessage)

    private fun assignDevice() {
        val devices = mutableListOf<IDevice>()
        devices.addAll(AndroidDebugBridge.getBridge().devices)
        if (!devices.isEmpty()) {
            if (devices.size > 1) {
                println("More than one device connected. Use @@Choose command to select device.")
                for (x in devices.indices) {
                    println("[ $x ] ${devices[x]}")
                }
            } else {
                choseDevice(AndroidDebugBridge.getBridge().devices[0])
            }
        } else {
            println("No devices connected.")
        }
    }

    private fun choseDevice(iDevice: IDevice?) {
        var connect = true
        if (device != null) {
            val connectedDevice = iDevice as IDevice
            val existingDevice: IDevice = device as IDevice
            if (connectedDevice.serialNumber == existingDevice.serialNumber) {
                println("Device is the same.")
                connect = false
            }
        }
        if (connect) {
            device = iDevice
            stopLogsReceiving()
            if (!data.get().isEmpty()) {
                data.get().clear()
                execute(COMMAND.CLEAR)
            }
            startLogsReceiving()
            println("We connected new device.")
        }
    }

    private fun startLogsReceiving() {
        logcatTask = LogCatReceiverTask(device)
        logcatTask?.addLogCatListener(logcatListener)
        logcatTask?.run()
    }

    private fun stopLogsReceiving() {
        logcatTask?.stop()
        logcatTask?.removeLogCatListener(logcatListener)
    }

    private fun getTag(line: String): String? {
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.V_LIVE)) {
            return LOGCAT_TAG_TYPE.V_LIVE
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.V_FILESYSTEM)) {
            return LOGCAT_TAG_TYPE.V_FILESYSTEM
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.D_LIVE)) {
            return LOGCAT_TAG_TYPE.D_LIVE
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.D_FILESYSTEM)) {
            return LOGCAT_TAG_TYPE.D_FILESYSTEM
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.I_LIVE)) {
            return LOGCAT_TAG_TYPE.I_LIVE
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.I_FILESYSTEM)) {
            return LOGCAT_TAG_TYPE.I_FILESYSTEM
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.W_LIVE)) {
            return LOGCAT_TAG_TYPE.W_LIVE
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.W_FILESYSTEM)) {
            return LOGCAT_TAG_TYPE.W_FILESYSTEM
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.E_LIVE)) {
            return LOGCAT_TAG_TYPE.E_LIVE
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.E_FILESYSTEM)) {
            return LOGCAT_TAG_TYPE.E_FILESYSTEM
        }
        return null
    }

    private object LOGCAT_TAG_TYPE {
        val V_LIVE = " V/"
        val V_FILESYSTEM = " V "
        val D_LIVE = " D/"
        val D_FILESYSTEM = " D "
        val I_LIVE = " I/"
        val I_FILESYSTEM = " I "
        val W_LIVE = " W/"
        val W_FILESYSTEM = " W "
        val E_LIVE = " E/"
        val E_FILESYSTEM = " E "
    }

    fun String.containsIgnoreCase(word: String): Boolean {
        return this.toLowerCase().contains(word.toLowerCase())
    }

}
