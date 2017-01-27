package net.milosvasic.pussycat.gui.factory

import net.milosvasic.pussycat.gui.PussycatListItem
import net.milosvasic.pussycat.gui.content.Labels
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.AtomicInteger


class PussycatListItemsFactory<T>(val factory: PussycatListItemFactory<T>) {

    companion object {
        val REQUEST_DELTA = 100
    }

    private var pollingThread: Thread? = null
    private val requested = AtomicInteger(REQUEST_DELTA)
    private val queue = LinkedBlockingQueue<Pair<T, Int>>()
    private val data = ConcurrentHashMap<Int, PussycatListItem>()
    private val requests = CopyOnWriteArrayList<PussycatListItemsRequest>()

    fun addRawData(value: T, index: Int) {
        if (!data.keys.contains(index)) {
            queue.add(Pair(value, index))
            processData()
        } else {
            println("Already processed [ $index ]") // TODO: Remove this.
        }
    }

    fun requestData(request: PussycatListItemsRequest) {
        requests.add(request)
        processData()
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
                        println("start [ $index ]")
                        val view = factory.obtain(item, index)
                        data.put(index, view)
                        println("end [ $index ][ ${data.size} ]")
                    }
                }
                pollingThread = null
                sendData()
            })
            pollingThread?.start()
        } else {
            sendData()
        }
    }

    private fun sendData() {
        for (request in requests) {
            val from = request.from
            val amount = request.amount
            val callback = request.callback
            if (from + amount <= requested.get()) {
                val items = mutableListOf<PussycatListItem>()
                var to = from + amount
                if (to > data.values.size) {
                    to = data.values.size - 1
                }
                for (x in from..to) {
                    items.add(data.values.elementAt(x))
                }
                callback.onData(items)
                requests.remove(request)
                println("Send data")
            }
        }
    }

}