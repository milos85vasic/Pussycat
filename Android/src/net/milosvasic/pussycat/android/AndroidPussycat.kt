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
import net.milosvasic.pussycat.utils.Text
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

    override fun executeOther(executable: COMMAND, params: Array<out String?>) {
        when (executable) {
            COMMAND.CHOOSE -> chooseDevice(params)
            else -> super.executeOther(executable, params)
        }
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
                    assignDevice() // For example we had 2 connected device, 1 left connected.
                }
            }
            AndroidDebugBridge.init(false)
            try {
                val debugBridge = AndroidDebugBridge.createBridge("adb", true)
                if (debugBridge == null) {
                    logger.e(TAG, "Invalid ADB path")
                } else {
                    assignDevice()
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
                val lines = logcat.readLines()
                data.addData(Array(lines.size, { i -> lines[i] }))
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
        val bridge = AndroidDebugBridge.getBridge()
        var timeOut: Long = 30 * 1000
        val sleepTime: Long = 1000
        while (!bridge.hasInitialDeviceList() && timeOut > 0) {
            Thread.sleep(sleepTime)
            timeOut -= sleepTime
        }
        if (timeOut <= 0 && !bridge.hasInitialDeviceList()) {
            println("Timeout getting device list.")
        }
        devices.addAll(bridge.devices)
        if (!devices.isEmpty()) {
            if (devices.size > 1) {
                stopLogsReceiving()
                println("More than one device connected. Use @@Choose command to select device.")
                for (x in devices.indices) {
                    println("[ $x ] ${devices[x]}")
                }
            } else {
                choseDevice(AndroidDebugBridge.getBridge().devices[0])
            }
        } else {
            println("No devices connected.")
            stopLogsReceiving()
        }
    }

    private fun choseDevice(iDevice: IDevice?) {
        var cleanup = true
        if (device != null) {
            val connectedDevice = iDevice as IDevice
            val existingDevice: IDevice = device as IDevice
            if (connectedDevice.serialNumber == existingDevice.serialNumber) {
                println("Device is the same.")
                cleanup = false
            }
        }
        device = iDevice
        stopLogsReceiving()
        if (!data.get().isEmpty() && cleanup) {
            data.get().clear()
            execute(COMMAND.CLEAR)
        }
        println("Device is ready [ $device ]")
        startLogsReceiving()
    }

    private fun chooseDevice(params: Array<out String?>) {
        val messageInvalid = "Invalid arguments passed for @@${COMMAND.CHOOSE} command."
        if (params.isEmpty()) {
            println(messageInvalid)
            return
        }
        val arg: Int? = params[0]?.toInt()
        if (arg != null) {
            val bridgedDevice = AndroidDebugBridge.getBridge().devices
            if (bridgedDevice.size > arg) {
                choseDevice(bridgedDevice[arg])
            } else {
                println("No device to choose at position $arg")
            }
        } else {
            println(messageInvalid)
        }
    }

    private fun startLogsReceiving() {
        Thread(Runnable {
            logcatTask = LogCatReceiverTask(device)
            logcatTask?.addLogCatListener(logcatListener)
            logcatTask?.run()
        }).start()
    }

    private fun stopLogsReceiving() {
        logcatTask?.stop()
        logcatTask?.removeLogCatListener(logcatListener)
    }

}
