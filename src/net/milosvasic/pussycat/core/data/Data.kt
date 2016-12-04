package net.milosvasic.pussycat.core.data

import net.milosvasic.pussycat.core.common.DataFilter
import net.milosvasic.pussycat.logging.LOG_LEVEL
import java.util.concurrent.CopyOnWriteArrayList

abstract class Data<T>(filter: DataFilter<CopyOnWriteArrayList<T>, String>) : DataAbstract<T>(filter) {

    protected fun evaluable(element: String): Boolean {
        return evaluable(Array(1, { element }))
    }

    protected fun evaluable(element: String, operator: OPERATOR): Boolean {
        return evaluable(Array(1, { element }), operator)
    }

    protected fun evaluable(elements: Array<String>): Boolean {
        for (element in elements) {
            if (element.contains(OPERATOR.AND.value) || element.contains(OPERATOR.OR.value)) {
                return true
            }
        }
        return false
    }

    protected fun evaluable(elements: Array<String>, operator: OPERATOR): Boolean {
        for (element in elements) {
            if (element.contains(operator.value)) {
                return true
            }
        }
        return false
    }

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