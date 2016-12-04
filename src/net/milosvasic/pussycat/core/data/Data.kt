package net.milosvasic.pussycat.core.data

import net.milosvasic.pussycat.core.common.DataFilter
import net.milosvasic.pussycat.logging.LOG_LEVEL
import java.util.concurrent.CopyOnWriteArrayList

abstract class Data<T>(filter: DataFilter<CopyOnWriteArrayList<T>, String>) : DataAbstract<T>(filter) {

    protected fun evaluable(element: T): Boolean {
        return evaluable(listOf(element))
    }

    protected fun evaluable(element: T, operator: OPERATOR): Boolean {
        return evaluable(listOf(element), operator)
    }

    protected abstract fun evaluable(elements: List<T>): Boolean

    protected abstract fun evaluable(elements: List<T>, operator: OPERATOR): Boolean

    protected fun evaluateAnd(line: String, pattern: String): Boolean {
        val ands = pattern.split(OPERATOR.AND.value)
        for (element in ands) {
            var check = element.trim()
            if (check.startsWith(OPERATOR.NOT.value)) {
                check = check.replace(OPERATOR.NOT.value, "")
                if (line.containsIgnoreCase(check)) {
                    return false
                }
            } else {
                if (!line.containsIgnoreCase(check)) {
                    return false
                }
            }
        }
        return true
    }

    protected fun evaluateOr(line: String, pattern: String): Boolean {
        val params = pattern.split(OPERATOR.OR.value)
        for (item in params) {
            var check = item.trim()
            if (check.startsWith(OPERATOR.NOT.value)) {
                check = check.replace(OPERATOR.NOT.value, "")
                if (!line.containsIgnoreCase(check)) {
                    return true
                }
            } else {
                if (line.containsIgnoreCase(check)) {
                    return true
                }
            }
        }
        return false
    }

    protected abstract fun getTag(message: T): LOG_LEVEL?

    fun String.containsIgnoreCase(word: String): Boolean {
        return this.toLowerCase().contains(word.toLowerCase())
    }

}