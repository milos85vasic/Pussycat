package net.milosvasic.pussycat.gui.factory

import net.milosvasic.pussycat.gui.PussycatListItem
import net.milosvasic.pussycat.gui.content.Labels
import net.milosvasic.pussycat.gui.data.DIRECTION
import net.milosvasic.pussycat.gui.data.DataSizeObtain
import java.util.concurrent.*


class PussycatListItemsFactory<T>(val dataSize: DataSizeObtain, val factory: PussycatListItemFactory<T>) {

    companion object {
        val REQUEST_DELTA = 100
    }

    private var workingThread: Thread? = null
    private val raw = ConcurrentHashMap<Int, T>()
    private val indexLimits = CopyOnWriteArrayList<Int>()
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
                println("RAW DATA") // TODO: Remove this
                workingThread = processData(null, processingCallback)
            }
        }
    }

    fun requestData(request: PussycatListItemsRequest) {
        println("PROCESS DATA [ ${request.from} ][ ${request.amount} ]") // TODO: Remove this
        processData(request)
    }

    fun applyIndexLimits(indexes: List<Int>) {
        indexLimits.clear()
        indexLimits.addAll(indexes)
    }

    fun getFirstIndex(): Int {
        if (!indexLimits.isEmpty()) {
            return indexLimits[0]
        }
        return 0
    }

    fun getLastIndex(): Int {
        if (!indexLimits.isEmpty()) {
            return indexLimits.last()
        }
        return dataSize.getDataSize() - 1
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
                var key : Int
                var keyOk = true
                var processed = 0
                val from = request.from
                val amount = request.amount
                if (request.direction == DIRECTION.DOWN) {
                    key = from - 1
                } else {
                    key = from + 1
                }
                fun getNextKey(): Int {
                    // TODO: Take into account index limits.
                    if (request.direction == DIRECTION.DOWN) {
                        return ++key
                    } else {
                        return --key
                    }
                }
                while (processed <= amount && keyOk) {
                    key = getNextKey()
                    if (request.direction == DIRECTION.DOWN) {
                        keyOk = key <= getLastIndex()
                    } else {
                        keyOk = key >= getFirstIndex()
                    }
                    if (keyOk && processKey(key)) {
                        processed++
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
            var key: Int
            var added = 0
            var keyOk = true
            val amount = request.amount
            val callback = request.callback
            val items = mutableListOf<PussycatListItem>()
            fun addItem(x: Int) {
                val item = data[x]
                if (item != null) {
                    items.add(item)
                    added++
                }
            }
            if (request.direction == DIRECTION.DOWN) {
                key = request.from - 1
            } else {
                key = request.from + 1
            }
            fun getNextKey(): Int {
                // TODO: Take into account index limits.
                if (request.direction == DIRECTION.DOWN) {
                    return ++key
                } else {
                    return --key
                }
            }
            while (added <= amount && keyOk) {
                key = getNextKey()
                if (request.direction == DIRECTION.DOWN) {
                    keyOk = key <= getLastIndex()
                } else {
                    keyOk = key >= getFirstIndex()
                }
                if (keyOk) {
                    addItem(key)
                }
            }
            callback.onData(request, items, request.direction)
        }
    }

    private interface ProcessingCallback {
        fun onProcessingComplete()
    }

}
