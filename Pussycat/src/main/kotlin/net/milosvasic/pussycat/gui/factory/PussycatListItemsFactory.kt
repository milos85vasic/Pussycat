package net.milosvasic.pussycat.gui.factory

import net.milosvasic.pussycat.gui.PussycatListItem
import net.milosvasic.pussycat.gui.content.Labels
import java.util.*
import java.util.concurrent.*


class PussycatListItemsFactory<T>(val factory: PussycatListItemFactory<T>) {

    companion object {
        val REQUEST_DELTA = 100
    }

    private val raw = ConcurrentHashMap<Int, T>()
    private val data = ConcurrentHashMap<Int, PussycatListItem>()

    fun addRawData(value: T, index: Int) {
        if (!data.keys.contains(index)) {
            raw.put(index, value)
            processData()
        }
    }

    fun requestData(request: PussycatListItemsRequest) {
        processData(request)
    }

    private fun processData(request: PussycatListItemsRequest? = null) {
        val task = Runnable {
            Thread.currentThread().name = Labels.PROCESSING_THREAD

            fun processKey(key: Int): Boolean {
                println("Key [ $key ]") // TODO: Remove this.
                if (data[key] == null) {
                    val item = raw[key]
                    if (item != null) {
                        val view = factory.obtain(item, key)
                        data.put(key, view)
                        raw.remove(key)
                        return true
                    }
                }
                return false
            }

            if (request != null) {

                val from = request.from
                val amount = request.amount
                if (request.direction == DIRECTION.DOWN) {
                    var to = from + amount
                    if (to >= data.values.size) {
                        to = data.values.size - 1
                    }
                    for (key in from..to) {
                        processKey(key)
                    }
                } else {
                    var to = from - amount
                    if (to < 0) {
                        to = 0
                    }
                    for (key in from downTo to) {
                        processKey(key)
                    }
                }

            } else {

                var key = 0
                while (!Thread.currentThread().isInterrupted && !raw.isEmpty() && key < REQUEST_DELTA) {
                    println("Key [ $key ]") // TODO: Remove this.
                    if (data[key] == null) {
                        val item = raw[key]
                        if (item != null) {
                            val view = factory.obtain(item, key)
                            data.put(key, view)
                            raw.remove(key)
                            key++
                        }
                    }
                }

            }
            sendData(request)
        }
        Thread(task).start()
    }


    private fun sendData(request: PussycatListItemsRequest? = null) {
        if (request != null) {
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
