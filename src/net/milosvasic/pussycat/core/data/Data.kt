package net.milosvasic.pussycat.core.data

import net.milosvasic.pussycat.core.common.DataFilter
import net.milosvasic.pussycat.logging.LOG_LEVEL
import java.util.concurrent.CopyOnWriteArrayList

abstract class Data<T>(filter: DataFilter<CopyOnWriteArrayList<T>, String>) : DataAbstract<T>(filter) {

    protected fun evaluable(pattern: String): Boolean {
        return evaluable(listOf(pattern))
    }

    protected fun evaluable(pattern: String, operator: OPERATOR): Boolean {
        return evaluable(listOf(pattern), operator)
    }

    protected abstract fun evaluable(elements: List<String>): Boolean

    protected abstract fun evaluable(elements: List<String>, operator: OPERATOR): Boolean

    protected abstract fun evaluateAnd(line: T, pattern: String): Boolean

    protected abstract fun evaluateOr(line: T, pattern: String): Boolean

    protected abstract fun getTag(message: T): LOG_LEVEL?

    abstract fun T.containsIgnoreCase(word: String): Boolean

    fun String.containsIgnoreCase(word: String): Boolean {
        return this.toLowerCase().contains(word.toLowerCase())
    }

}