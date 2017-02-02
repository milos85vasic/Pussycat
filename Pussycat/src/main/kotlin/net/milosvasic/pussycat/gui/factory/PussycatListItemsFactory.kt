package net.milosvasic.pussycat.gui.factory

import net.milosvasic.pussycat.gui.PussycatListItem
import net.milosvasic.pussycat.gui.content.Labels
import java.util.*
import java.util.concurrent.*


class PussycatListItemsFactory<T>(val factory: PussycatListItemFactory<T>) {

    companion object {
        val REQUEST_DELTA = 100
    }

    private val executor = Executor()
    private val raw = ConcurrentHashMap<Int, T>()
    private val data = LinkedHashMap<Int, PussycatListItem>()
    private var activeRequest: PussycatListItemsRequest? = null

    fun addRawData(value: T, index: Int) {
        if (!data.keys.contains(index)) {
            raw.put(index, value)
            processData()
        }
    }

    fun requestData(request: PussycatListItemsRequest) {
        if (activeRequest != null) {
            request.callback.onDataRequestRejected(request)
            return
        } else {
            processData(request)
        }
    }

    private fun processData(request: PussycatListItemsRequest) {
        println("Request accepted")  // TODO: Remove this
        activeRequest = request
        processData()
    }

    private fun processData() {
        val task = Runnable {
            Thread.currentThread().name = Labels.PROCESSING_THREAD
            var processed = 0
            while (!Thread.currentThread().isInterrupted && !raw.isEmpty() && processed < REQUEST_DELTA) {
                val key = raw.keys().nextElement()
                println("Key [ $key ]") // TODO: Remove this.
                if (key != null) {
                    if (data[key] == null) {
                        val item = raw[key]
                        if (item != null) {
                            val view = factory.obtain(item, key)
                            data.put(key, view)
                            raw.remove(key)
                            processed++
                        }
                    }
                }
            }
            sendData()
        }

        try {
            executor.submit(task)
        } catch (e: RejectedExecutionException) {
            // Ignore
        }
    }

    private fun sendData() {
        if (activeRequest != null) {
            val request = activeRequest as PussycatListItemsRequest
            val from = request.from
            val amount = request.amount
            val callback = request.callback
            val items = mutableListOf<PussycatListItem>()
            if (request.direction == DIRECTION.DOWN) {
                var to = from + amount
                if (to >= data.values.size) {
                    to = data.values.size - 1
                }
                for (x in from..to) {
                    items.add(data.values.elementAt(x))
                }
            } else {
                var to = from - amount
                if (to < 0) {
                    to = 0
                }
                for (x in from downTo to) {
                    items.add(data.values.elementAt(x))
                }
            }
            println("Send data ${items.size} ${request.direction}") // TODO: Remove this.
            callback.onData(items, request.direction)
            activeRequest = null
        } else {
//            println("No active request.") // TODO: Remove this.
        }
    }

    inner class Executor : ThreadPoolExecutor(1, 1, 0, TimeUnit.MILLISECONDS, LimitedQueue<Runnable>(1))

    inner class LimitedQueue<T>(maxSize: Int) : LinkedBlockingQueue<T>(maxSize) {
        override fun offer(e: T): Boolean {
            if (size == 0) {
                put(e)
                return true
            }
            return false
        }
    }

}
