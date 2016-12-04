package net.milosvasic.pussycat.android

import com.android.ddmlib.AndroidDebugBridge
import com.android.ddmlib.IDevice
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


abstract class AndroidPussycat : PussycatAbstract() {

    protected val run = AtomicBoolean()
    protected var device: IDevice?
    protected val paused = AtomicBoolean(false)
    protected var refreshing = AtomicBoolean(false)
    protected lateinit var deviceCHangeListener: AndroidDebugBridge.IDeviceChangeListener

    init {
        device = null
        data = AndroidData(this)
        logger = ConsoleLogger()
        TAG = AndroidPussycat::class
    }

    override fun live() {
        run.set(false)
        Thread(Runnable {
            Thread.currentThread().name = "Live adb reading thread"
            deviceCHangeListener = object : AndroidDebugBridge.IDeviceChangeListener {
                override fun deviceChanged(iDevice: IDevice?, changeMask: Int) {
                    println("Device changed [ $iDevice ]")
                    assignDevice()
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
            AndroidDebugBridge.addDeviceChangeListener(deviceCHangeListener)

//            AndroidDebugBridge.addDeviceChangeListener(AndroidDebugBridge.IDeviceChangeListener)
//            val devices = AndroidDeviceStore.getInstance()
//                    .getDevices()
//
//            for (d in devices) {
//                System.out.println(d.getSerialNumber())
//            }
//            val device = devices.pollFirst()
//            System.out.println(device.getName())


            val process = Runtime.getRuntime().exec("adb logcat")
            val input = process.inputStream
            var line = ""
            val reader = InputStreamReader(input)
            val buffered = BufferedReader(reader)

            run.set(true)
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
                        if (!refreshing.get() && data.evaluate(line)) {
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
        if (params.isEmpty()) {
            return
        }
        run.set(false)
        val logcat = File(params[0] as String)
        if (logcat.exists()) {
            Thread(Runnable {
                Thread.currentThread().name = "Filesystem reading thread"
                val input = FileInputStream(logcat.absoluteFile)
                val reader = InputStreamReader(input)
                val buffered = BufferedReader(reader)
                var line = ""

                run.set(true)
                while (run.get() && line != null) {
                    try {
                        line = buffered.readLine()
                    } catch (e: Exception) {
                        run.set(false)
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
        if (!run.get()) {
            return
        }
        run.set(false)
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
        run.set(true)
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
        if (device != null) {
            val existingDevice: IDevice = device as IDevice
            if (iDevice == existingDevice) {
                println("Device is the same.")
            } else {
                println("We connected new device.")
            }
        } else {
            device = iDevice
            if (!data.get().isEmpty()) {
                data.get().clear()
                execute(COMMAND.CLEAR)
            }
            println("We connected new device.")
        }
    }

    abstract protected fun printLine(line: String)

}
