package net.milosvasic.pussycat.core.data

import net.milosvasic.pussycat.core.common.Filter
import net.milosvasic.pussycat.core.common.DataFilter
import net.milosvasic.pussycat.logging.LOG_TYPE
import java.util.*

abstract class Data<T>(@Transient val filter: DataFilter<LinkedHashMap<String, T>, String>) : Filter<String, T> {

    @Transient protected var pattern = ""
    protected val data = LinkedHashMap<String, T>()

    override fun apply(pattern: String?) {
        this.pattern = pattern ?: ""
        filter.apply(data, pattern)
    }

    fun getFilterPattern(): String {
        return pattern
    }

    fun clear() {
        data.clear()
        pattern = ""
    }

    fun get(): LinkedHashMap<String, T> {
        return data
    }

    protected fun evaluable(pattern: String): Boolean {
        return evaluable(listOf(pattern))
    }

    protected fun evaluable(pattern: String, operator: OPERATOR): Boolean {
        return evaluable(listOf(pattern), operator)
    }

    abstract fun addData(message: T)

    protected abstract fun evaluable(elements: List<String>): Boolean

    protected abstract fun evaluable(elements: List<String>, operator: OPERATOR): Boolean

    protected abstract fun evaluateAnd(line: T, pattern: String): Boolean

    protected abstract fun evaluateOr(line: T, pattern: String): Boolean

    protected abstract fun getTag(message: T): LOG_TYPE?

    protected abstract fun getIdentifier(message: T): String

    abstract fun T.containsIgnoreCase(word: String): Boolean

    fun String.containsIgnoreCase(word: String): Boolean {
        return this.toLowerCase().contains(word.toLowerCase())
    }

}