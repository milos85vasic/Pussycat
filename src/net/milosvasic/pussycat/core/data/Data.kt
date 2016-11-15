package net.milosvasic.pussycat.core.data

import java.util.concurrent.CopyOnWriteArrayList
import net.milosvasic.pussycat.core.common.Filter
import net.milosvasic.pussycat.core.common.DataFilter

class Data(val filter: DataFilter<CopyOnWriteArrayList<String>, String>) : Filter<String> {

    private var pattern = ""
    private val data = CopyOnWriteArrayList<String>()

    override fun apply(pattern: String?) {
        this.pattern = pattern as String
        filter.apply(data, pattern)
    }

    fun getFilterPattern(): String {
        return pattern
    }

}