package net.milosvasic.pussycat.gui.factory

import net.milosvasic.pussycat.gui.PussycatListItem
import net.milosvasic.pussycat.gui.content.Labels
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.LinkedBlockingQueue


class PussycatListItemsFactory<T>(val factory: PussycatListItemFactory<T>) {

    companion object {
        val REQUEST_DELTA = 100
    }

    private var processingThread: Thread? = null
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
        if (processingThread == null) {
            processingThread = Thread(Runnable {
                Thread.currentThread().name = Labels.PROCESSING_THREAD
                println(">>>>>> ${queue.size}") // TODO: Remove this
                while (!Thread.currentThread().isInterrupted && !queue.isEmpty()) {
                    val polledItem = queue.poll()
                    val index = polledItem?.second
                    if (index != null && data[index] == null) {
                        val item = polledItem?.first
                        if (item != null) {
                            val view = factory.obtain(item, index)
                            data.put(index, view)
                        }
                    }
                }
                processingThread = null
                sendData()
            })
            processingThread?.start()
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
            println(">>> 2 ${items.size}") // TODO: Remove this.
            callback.onData(items, request.direction)
            activeRequest = null
        } else {
            println("No active request.") // TODO: Remove this.
        }
    }

}
