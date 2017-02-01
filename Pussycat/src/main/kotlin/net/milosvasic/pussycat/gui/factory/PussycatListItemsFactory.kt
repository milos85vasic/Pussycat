package net.milosvasic.pussycat.gui.factory

import net.milosvasic.pussycat.gui.PussycatListItem
import net.milosvasic.pussycat.gui.content.Labels
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.AtomicInteger


class PussycatListItemsFactory<T>(val factory: PussycatListItemFactory<T>) {

    companion object {
        val REQUEST_DELTA = 100
    }

    private var pollingThread: Thread? = null
    private val requested = AtomicInteger(REQUEST_DELTA)
    private val queue = LinkedBlockingQueue<Pair<T, Int>>()
    private var activeRequest: PussycatListItemsRequest? = null
    private val data = ConcurrentHashMap<Int, PussycatListItem>()

    fun addRawData(value: T, index: Int) {
        if (!data.keys.contains(index)) {
            queue.add(Pair(value, index))
            processData()
        }
    }

    fun requestData(request: PussycatListItemsRequest) {
        if (activeRequest != null) {
            return
        } else {
            activeRequest = request
            requested.set(request.from + request.amount)
            processData()
        }
    }

    private fun processData() {
        if (pollingThread == null && data.size < requested.get() + REQUEST_DELTA) {
            pollingThread = Thread(Runnable {
                Thread.currentThread().name = Labels.POLLING_THREAD
                while (!Thread.currentThread().isInterrupted && !queue.isEmpty() && data.size < requested.get() + REQUEST_DELTA) {
                    val polledItem = queue.poll()
                    val item = polledItem?.first
                    val index = polledItem?.second
                    if (item != null && index != null) {
                        val view = factory.obtain(item, index)
                        data.put(index, view)
                    }
                }
                pollingThread = null
                sendData()
            })
            pollingThread?.start()
        }
        if (data.size >= requested.get() + REQUEST_DELTA) {
            sendData()
        }
    }

    private fun sendData() {
        if (activeRequest != null) {
            val request = activeRequest as PussycatListItemsRequest
            val from = request.from
            val amount = request.amount
            val callback = request.callback
            if (from + amount <= requested.get()) {
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
                println(">>> 2")
                callback.onData(items, request.direction)
                activeRequest = null
            }
        }
    }

}
