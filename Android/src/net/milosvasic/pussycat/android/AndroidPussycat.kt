package net.milosvasic.pussycat.android

import com.android.ddmlib.AndroidDebugBridge
import com.android.ddmlib.IDevice
import com.android.ddmlib.logcat.LogCatListener
import com.android.ddmlib.logcat.LogCatMessage
import com.android.ddmlib.logcat.LogCatReceiverTask
import com.sun.xml.internal.bind.v2.model.core.ID
import net.milosvasic.pussycat.PussycatAbstract
import net.milosvasic.pussycat.android.data.AndroidData
import net.milosvasic.pussycat.core.COMMAND
import net.milosvasic.pussycat.logging.ConsoleLogger
import net.milosvasic.pussycat.utils.Text
import java.io.BufferedReader
import java.io.File
import java.io.FileInputStream
import java.io.InputStreamReader
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
            val debugBridge = AndroidDebugBridge.createBridge("adb", true)
            if (debugBridge == null) {
                logger.e(TAG, "Invalid ADB path")
            }
            AndroidDebugBridge.addDeviceChangeListener(deviceChangeListener)
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
                val input = FileInputStream(logcat.absoluteFile)
                val reader = InputStreamReader(input)
                val buffered = BufferedReader(reader)
                var line = ""

                while (line != null) {
                    try {
                        line = buffered.readLine()
                    } catch (e: Exception) {
                        break
                    }
                    if (!Text.isEmpty(line)) {
                        line = line.trim()
                        if (!Text.isEmpty(line)) {
                            data.addData(line)
                            if (!refreshing.get() && data.evaluate(line)) {
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

    override fun apply(data: CopyOnWriteArrayList<String>, pattern: String?) {
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

    abstract protected fun printLine(line: LogCatMessage)

}
