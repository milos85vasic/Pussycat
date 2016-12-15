package net.milosvasic.pussycat.android

import com.android.ddmlib.AndroidDebugBridge
import com.android.ddmlib.IDevice
import com.android.ddmlib.Log
import com.android.ddmlib.logcat.LogCatListener
import com.android.ddmlib.logcat.LogCatMessage
import com.android.ddmlib.logcat.LogCatReceiverTask
import net.milosvasic.pussycat.PussycatAbstract
import net.milosvasic.pussycat.android.command.ANDROID_COMMAND
import net.milosvasic.pussycat.android.data.AndroidData
import net.milosvasic.pussycat.android.data.AndroidLogCatMessage
import net.milosvasic.pussycat.android.data.LogCatMessageParser
import net.milosvasic.pussycat.core.COMMAND
import net.milosvasic.pussycat.logging.ConsoleLogger
import net.milosvasic.pussycat.utils.Text
import java.io.File
import java.util.*
import java.util.concurrent.atomic.AtomicBoolean
import java.io.FileOutputStream
import java.io.ObjectOutputStream


abstract class AndroidPussycat : PussycatAbstract<AndroidLogCatMessage, AndroidData>() {

    protected var device: IDevice?
    protected val paused = AtomicBoolean(false)
    protected var refreshing = AtomicBoolean(false)
    protected var logcatTask: LogCatReceiverTask? = null

    companion object {
        val FILE_EXTENSION = "psct"
    }

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
                    val androidMessage = AndroidLogCatMessage.getFrom(message)
                    if (data.evaluate(androidMessage)) printLine(androidMessage)
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
            ANDROID_COMMAND.LOG_LEVEL ->
                if (params.isEmpty()) {
                    printLogLevel()
                } else {
                    filterByLogLevel(params)
                }
            else -> super.executeOther(executable, params)
        }
    }

    override fun live() {
        Thread(Runnable {
            Thread.currentThread().name = "Live adb reading thread"
            var debugBridge = initAndroidDebugBridge()
            if (debugBridge == null) {
                printLine("Pussycat, invalid ADB path")
            } else {
                data.clear()
                assignDevice()
                AndroidDebugBridge.addDeviceChangeListener(deviceChangeListener)
                if (paused.get()) {
                    paused.set(false)
                }
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
                logcatTask?.removeLogCatListener(logcatListener)
                AndroidDebugBridge.removeDeviceChangeListener(deviceChangeListener)
                data.clear()

                if (paused.get()) {
                    paused.set(false)
                }

                if (logcat.extension == FILE_EXTENSION) {

                } else {
                    var messageIndex = 0
                    var lastIdentifier = ""
                    logcat.forEachLine { line ->
                        data.addData(Array(1, { i -> line }))
                        val message = data.get().values.last()
                        val newIdentifier = LogCatMessageParser.getIdentifier(message)
                        if (!refreshing.get() && data.evaluate(message)) {
                            if (lastIdentifier == newIdentifier) {
                                messageIndex++
                                val lines = message.msg.split("\n")
                                printLine(lines[messageIndex], message.logLevel)
                            } else {
                                messageIndex = 0
                                printLine(message)
                            }
                        }
                        lastIdentifier = newIdentifier
                    }
                }
            }).start()
        } else {
            printLine("Logcat: ${logcat.absoluteFile} does not exist")
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

    override fun apply(data: LinkedHashMap<String, AndroidLogCatMessage>, pattern: String?) {
        refreshing.set(true)
        paused.set(false)
        clear()
        if (data.isEmpty()) {
            val noDataMessage: String
            if (Text.isEmpty(this.data.getFilterPattern())) {
                noDataMessage = "no filter applied"
            } else {
                noDataMessage = "filter: ${this.data.getFilterPattern()}"
            }
            printLine("Pussycat, no data available [ $noDataMessage ]")
        } else {
            var x = 0
            fun printLineAndIncrement(line: AndroidLogCatMessage) {
                printLine(line)
                x++
            }
            for (line in data.values) {
                if (this.data.evaluate(line)) {
                    if (this.data.getLogLevel() != null) {
                        if (line.logLevel == this.data.getLogLevel()) {
                            printLineAndIncrement(line)
                        }
                    } else {
                        printLineAndIncrement(line)
                    }
                }
            }
            if (x == 0) printLine("Pussycat, no data matching parameters [ filter: ${this.data.getFilterPattern()} ][ log level: ${getPrintableLogLevelValue()} ]")
        }
        refreshing.set(false)
    }

    override fun export(params: Array<out String?>) {
        Thread(Runnable {
            Thread.currentThread().name = "Exporting thread."
            println("Pussycat, export [ STARTED ]")
            val root: File
            var name: String
            val destination: File
            if (params.isEmpty() || Text.isEmpty(params[0])) {
                name = "export_${System.currentTimeMillis()}.$FILE_EXTENSION"
            } else {
                name = params[0]?.trim()?.replace(File.separator, "_") as String
                if (!name.endsWith(".$FILE_EXTENSION")) {
                    name = "$name.$FILE_EXTENSION"
                }
            }
            val home = System.getProperty("user.home")
            root = File("$home${File.separator}Pussycat")
            if (!root.exists()) {
                if (!root.mkdirs()) {
                    printLine("Couldn't create directory [ ${root.absolutePath} ]")
                    return@Runnable
                }
            }
            destination = File(root.absolutePath, name)
            println("Pussycat, saving to destination [ ${destination.absolutePath} ]")
            try {
                val fout = FileOutputStream(destination)
                val out = ObjectOutputStream(fout)
                out.writeObject(data.get())
            } catch (e: Exception) {
                printLine("Pussycat, error saving data: $e")
            }
            println("Pussycat, export [ COMPLETED ]")
        }).start()
    }

    abstract protected fun printLine(text: String)

    abstract protected fun printLine(text: String, logLevel: Log.LogLevel)

    abstract protected fun printLine(line: AndroidLogCatMessage)

    abstract protected fun getPrintableLogLevelValue(): String

    abstract protected fun getPrintableFilterValue(): String

    private fun filterByLogLevel(params: Array<out String?>) {
        if (!params.isEmpty()) {
            val param = params[0]
            when (param) {
                null -> printLogLevel()
                "clear" -> data.clearLogLevel()
                else -> {
                    val logLevel: Log.LogLevel?
                    if (param.length == 1) {
                        logLevel = Log.LogLevel.getByLetter(param[0].toUpperCase())
                    } else {
                        logLevel = Log.LogLevel.getByString(param.toLowerCase())
                    }
                    if (logLevel != null) {
                        data.setLogLevel(logLevel)
                    } else {
                        printLine("Invalid parameter: $param")
                    }
                }
            }
        }
    }

    private fun printLogLevel() {
        printLine("Pussycat, log level [ ${getPrintableLogLevelValue()} ]")
    }

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
            AndroidDebugBridge.initIfNeeded(false)
        } catch (e: IllegalStateException) {
            // Android debug bridge is already initialized.
        }
        return AndroidDebugBridge.createBridge("adb", false)
    }

    private fun waitForDevices(bridge: AndroidDebugBridge) {
        val sleepTime: Long = 1000
        var timeOut: Long = 30 * 1000
        while (!bridge.hasInitialDeviceList() && timeOut > 0) {
            Thread.sleep(sleepTime)
            timeOut -= sleepTime
        }
        if (timeOut <= 0 && !bridge.hasInitialDeviceList()) {
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
