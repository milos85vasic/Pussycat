package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.content.Labels
import net.milosvasic.pussycat.listeners.Listener
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.AtomicInteger


class PussycatListItemsFactory<T>(val factory: PussycatListItemFactory<T>) : Listener<Pair<T, Int>> {

    val requested = AtomicInteger(100)
    var pollingThread: Thread? = null
    val queue = LinkedBlockingQueue<Pair<T, Int>?>()
    val data = ConcurrentHashMap<Int, PussycatListItem>()

    override fun onEvent(value: Pair<T, Int>?) {
        queue.add(value)
        processData()
    }

    private fun processData() {
        if (pollingThread == null) {
            pollingThread = Thread(Runnable {
                Thread.currentThread().name = Labels.POLLING_THREAD
                while (!Thread.currentThread().isInterrupted && !queue.isEmpty() && data.size <= requested.get()) {
                    val polledItem = queue.poll()
                    val item = polledItem?.first
                    val index = polledItem?.second
                    if (item != null && index != null) {
                        println("start [ $index ]")
                        val view = factory.obtain(item, index)
                        view.isVisible = true
                        view.validate()
                        data.put(index, view)
                        println("end [ $index ][ ${data.size} ]")
                    }
                }
                pollingThread = null
            })
            pollingThread?.start()
        }
    }

}