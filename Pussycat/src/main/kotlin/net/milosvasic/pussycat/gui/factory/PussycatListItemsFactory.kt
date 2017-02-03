package net.milosvasic.pussycat.gui.factory

import net.milosvasic.pussycat.gui.PussycatListItem
import net.milosvasic.pussycat.gui.content.Labels
import net.milosvasic.pussycat.gui.data.DIRECTION
import java.util.concurrent.*


class PussycatListItemsFactory<T>(val factory: PussycatListItemFactory<T>) {

    companion object {
        val REQUEST_DELTA = 100
    }

    private var workingThread: Thread? = null
    private val raw = ConcurrentHashMap<Int, T>()
    private val data = ConcurrentHashMap<Int, PussycatListItem>()

    private val processingCallback = object : ProcessingCallback {
        override fun onProcessingComplete() {
            workingThread = null
        }
    }

    fun addRawData(value: T, index: Int) {
        if (!data.keys.contains(index)) {
            raw.put(index, value)
            if (data.size < REQUEST_DELTA && workingThread == null) {
                workingThread = processData(null, processingCallback)
            }
        }
    }

    fun requestData(request: PussycatListItemsRequest) {
        processData(request)
    }

    private fun processData(request: PussycatListItemsRequest? = null, callback: ProcessingCallback? = null): Thread {
        val task = Runnable {
            Thread.currentThread().name = Labels.PROCESSING_THREAD

            fun processKey(key: Int): Boolean {
                val item = raw.remove(key)
                if (item != null) {
                    val view = factory.obtain(item, key)
                    data.put(key, view)
                    return true
                }
                return false
            }

            if (request != null) {

                val from = request.from
                val amount = request.amount
                if (request.direction == DIRECTION.DOWN) {
                    val to = from + amount
                    for (key in from..to) {
                        if (processKey(key)) {
                            // Key processed
                        }
                    }
                } else {
                    var to = from - amount
                    if (to < 0) {
                        to = 0
                    }
                    for (key in from downTo to) {
                        if (processKey(key)) {
                            // Key processed
                        }
                    }
                }

            } else {

                var key = 0
                while (!Thread.currentThread().isInterrupted && !raw.isEmpty() && key <= REQUEST_DELTA) {
                    if (processKey(key)) {
                        // Key processed
                    }
                    key++
                }

            }

            sendData(request)
            callback?.onProcessingComplete()
        }

        val thread = Thread(task)
        thread.start()
        return thread
    }


    private fun sendData(request: PussycatListItemsRequest? = null) {
        if (request != null) {
            val from = request.from
            val amount = request.amount
            val callback = request.callback
            val items = mutableListOf<PussycatListItem>()

            fun addItem(x: Int?) {
                val item = data[x]
                if (item != null) {
                    items.add(item)
                }
            }

            if (request.direction == DIRECTION.DOWN) {
                var to = from + amount
                if (to >= data.values.size) {
                    to = data.values.size - 1
                }
                for (x in from..to) {
                    addItem(x)
                }
            } else {
                var to = from - amount
                if (to < 0) {
                    to = 0
                }
                for (x in from downTo to) {
                    addItem(x)
                }
            }

            callback.onData(request, items, request.direction)
        }
    }

    private interface ProcessingCallback {
        fun onProcessingComplete()
    }

}
