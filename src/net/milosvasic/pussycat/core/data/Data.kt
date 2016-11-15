package net.milosvasic.pussycat.core.data

import java.util.concurrent.CopyOnWriteArrayList
import net.milosvasic.pussycat.core.common.Filter

class Data(val filter: Filter<String>) : Filter<String> {

    private val data = CopyOnWriteArrayList<String>()

    override fun filter(pattern: String?) {
        filter.filter(pattern)
    }

}