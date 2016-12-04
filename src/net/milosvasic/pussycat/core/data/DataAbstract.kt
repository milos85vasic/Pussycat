package net.milosvasic.pussycat.core.data

import java.util.concurrent.CopyOnWriteArrayList
import net.milosvasic.pussycat.core.common.Filter
import net.milosvasic.pussycat.core.common.DataFilter

abstract class DataAbstract<T>(val filter: DataFilter<CopyOnWriteArrayList<T>, String>) : Filter<String, T> {

    protected var pattern = ""
    protected val data = CopyOnWriteArrayList<T>()

    override fun apply(pattern: String?) {
        this.pattern = pattern ?: ""
        filter.apply(data, pattern)
    }

    fun getFilterPattern(): String {
        return pattern
    }

    fun get(): CopyOnWriteArrayList<T> {
        return data
    }

    abstract fun addData(message: T)

}