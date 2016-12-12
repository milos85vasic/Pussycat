package net.milosvasic.pussycat.android

import com.android.ddmlib.AndroidDebugBridge
import com.android.ddmlib.IDevice
import com.android.ddmlib.logcat.LogCatListener
import com.android.ddmlib.logcat.LogCatMessage
import com.android.ddmlib.logcat.LogCatReceiverTask
import net.milosvasic.pussycat.PussycatAbstract
import net.milosvasic.pussycat.android.command.ANDROID_COMMAND
import net.milosvasic.pussycat.android.data.AndroidData
import net.milosvasic.pussycat.core.COMMAND
import net.milosvasic.pussycat.logging.ConsoleLogger
import java.io.File
import java.util.*
import java.util.concurrent.atomic.AtomicBoolean


abstract class AndroidPussycat : PussycatAbstract<LogCatMessage, AndroidData>() {

    protected var device: IDevice?
    protected val paused = AtomicBoolean(false)
    protected var refreshing = AtomicBoolean(false)
    protected var logcatTask: LogCatReceiverTask? = null

    protected var deviceChangeListener: AndroidDebugBridge.IDeviceChangeListener = object : AndroidDebugBridge.IDeviceChangeListener {
        override fun deviceChanged(iDevice: IDevice?, changeMask: Int) {
        }

        override fun deviceConnected(iDevice: IDevice?) {
            printLine("Device connected [ $iDevice ]")
            assignDevice()
        }

        override fun deviceDisconnected(iDevice: IDevice?) {
            printLine("Device disconnected [ $iDevice ]")
            assignDevice() // For example we had 2 connected device, 1 left connected.
        }
    }

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
            ANDROID_COMMAND.CHOOSE -> chooseDevice(params)
            ANDROID_COMMAND.DEVICES -> showDevices()
            else -> super.executeOther(executable, params)
        }
    }

    override fun live() {
        Thread(Runnable {
            Thread.currentThread().name = "Live adb reading thread"
            var debugBridge = initAndroidDebugBridge()
            if (debugBridge == null) {
                logger.e(TAG, "Invalid ADB path")
            } else {
                data.clear()
                assignDevice()
                AndroidDebugBridge.addDeviceChangeListener(deviceChangeListener)
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
                logcatTask?.removeLogCatListener(logcatListener)
                AndroidDebugBridge.removeDeviceChangeListener(deviceChangeListener)
                data.clear()
                data.addData(Array(lines.size, { i -> lines[i] }))
                if (!refreshing.get()) {
                    for (message in data.get().values) {
                        if (data.evaluate(message)) printLine(message)
                    }
                }
            }).start()
        } else {
            printLine("Logcat: ${logcat.absoluteFile} does not exist")
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

    override fun apply(data: LinkedHashMap<String, LogCatMessage>, pattern: String?) {
        refreshing.set(true)
        paused.set(false)
        printLine(27.toChar() + "[2J")
        if (data.isEmpty()) {
            printLine("No data available [ filter: ${this.data.getFilterPattern()} ]")
        } else {
            var x = 0
            for (line in data.values) {
                if (this.data.evaluate(line)) {
                    printLine(line)
                    x++
                }
            }
            if (x == 0) logger.w(TAG, "No data matching [ filter: ${this.data.getFilterPattern()} ]")
        }
        refreshing.set(false)
    }

    abstract protected fun printLine(text: String)

    abstract protected fun printLine(line: LogCatMessage)

    private fun assignDevice() {
        val devices = mutableListOf<IDevice>()
        val bridge = AndroidDebugBridge.getBridge()
        waitForDevices(bridge)
        devices.addAll(bridge.devices)
        if (!devices.isEmpty()) {
            if (devices.size > 1) {
                stopLogsReceiving()
                printLine("More than one device connected. Use @@Choose command to select device.")
                for (x in devices.indices) {
                    printLine("[ $x ] ${devices[x]}")
                }
            } else {
                choseDevice(AndroidDebugBridge.getBridge().devices[0])
            }
        } else {
            printLine("No devices connected.")
            stopLogsReceiving()
        }
    }

    private fun showDevices() {
        val devices = mutableListOf<IDevice>()
        var bridge = AndroidDebugBridge.getBridge()
        if (bridge == null) {
            bridge = initAndroidDebugBridge()
            waitForDevices(bridge)
        }
        if (bridge.devices != null && !bridge.devices.isEmpty()) {
            devices.addAll(bridge.devices)
            printLine("Available devices:")
            for (x in devices.indices) {
                printLine("[ $x ] ${devices[x]}")
            }
        } else {
            printLine("No devices connected.")
        }
    }

    private fun choseDevice(iDevice: IDevice?) {
        var cleanup = true
        if (device != null) {
            val connectedDevice = iDevice as IDevice
            val existingDevice: IDevice = device as IDevice
            if (connectedDevice.serialNumber == existingDevice.serialNumber) {
                printLine("Device is the same.")
                cleanup = false
            }
        }
        device = iDevice
        stopLogsReceiving()
        if (!data.get().isEmpty() && cleanup) {
            data.get().clear()
            execute(COMMAND.CLEAR)
        }
        printLine("Device is ready [ $device ]")
        startLogsReceiving()
    }

    private fun chooseDevice(params: Array<out String?>) {
        val messageInvalid = "Invalid arguments passed for @@${ANDROID_COMMAND.CHOOSE} command."
        if (params.isEmpty()) {
            printLine(messageInvalid)
            return
        }
        val arg: Int? = params[0]?.toInt()
        if (arg != null) {
            val bridgedDevice = AndroidDebugBridge.getBridge().devices
            if (bridgedDevice.size > arg) {
                choseDevice(bridgedDevice[arg])
            } else {
                printLine("No device to choose at position $arg")
            }
        } else {
            printLine(messageInvalid)
        }
    }

    private fun initAndroidDebugBridge(): AndroidDebugBridge? {
        try {
            AndroidDebugBridge.init(false)
        } catch (e: IllegalStateException) {
            // Android debug bridge is already initialized.
        }
        return AndroidDebugBridge.createBridge("adb", true)
    }

    private fun waitForDevices(bridge: AndroidDebugBridge) {
        var timeOut: Long = 30 * 1000
        val sleepTime: Long = 1000
        var timeOut1 = timeOut
        while (!bridge.hasInitialDeviceList() && timeOut1 > 0) {
            Thread.sleep(sleepTime)
            timeOut1 -= sleepTime
        }
        if (timeOut1 <= 0 && !bridge.hasInitialDeviceList()) {
            printLine("Timeout getting device list.")
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
