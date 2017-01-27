package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.content.Labels
import net.milosvasic.pussycat.listeners.Listener
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.LinkedBlockingQueue


class PussycatListItemsFactory<T>(val factory: PussycatListItemFactory<T>) : Listener<Pair<T, Int>> {

    var pollingThread: Thread? = null
    val queue = LinkedBlockingQueue<Pair<T, Int>?>()
    val executor: ExecutorService = Executors.newFixedThreadPool(1)
    val data = ConcurrentHashMap<Int, PussycatListItem>()

    override fun onEvent(value: Pair<T, Int>?) {
        queue.add(value)
        if (pollingThread == null) {
            pollingThread = Thread(Runnable {
                Thread.currentThread().name = Labels.POLLING_THREAD
                while (!Thread.currentThread().isInterrupted && !queue.isEmpty()) {
                    val polledItem = queue.poll()
                    val item = polledItem?.first
                    val index = polledItem?.second
                    if (item != null && index != null) {
                        executor.submit {
                            val view = factory.obtain(item, index)
                            data.put(index, view)
                        }
                    }
                }
                pollingThread = null
            })
            pollingThread?.start()
        }
    }

}