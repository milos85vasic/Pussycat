package net.milosvasic.pussycat.core.data

import java.util.concurrent.CopyOnWriteArrayList
import net.milosvasic.pussycat.core.common.Filter
import net.milosvasic.pussycat.core.common.DataFilter

abstract class DataAbstract(val filter: DataFilter<CopyOnWriteArrayList<String>, String>) : Filter<String> {

    protected var pattern = ""
    protected val data = CopyOnWriteArrayList<String>()

    override fun apply(pattern: String?) {
        this.pattern = pattern ?: ""
        filter.apply(data, pattern)
    }

    fun getFilterPattern(): String {
        return pattern
    }

    fun get(): CopyOnWriteArrayList<String> {
        return data
    }

    abstract fun addData(line: String)

}