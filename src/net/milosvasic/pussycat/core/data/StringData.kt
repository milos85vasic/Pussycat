package net.milosvasic.pussycat.core.data

import net.milosvasic.pussycat.core.common.DataFilter
import java.util.concurrent.CopyOnWriteArrayList


abstract class StringData<T>(filter: DataFilter<CopyOnWriteArrayList<T>, String>) : Data<T>(filter) {

    override fun addData(message: T) {
        data.add(message)
    }

    override fun evaluate(message: T): Boolean {
        if (pattern.isEmpty()) {
            return true
        }
        if (evaluable(pattern)) {
            if (!evaluable(pattern, OPERATOR.OR)) {
                return evaluateAnd(message, pattern)
            } else {
                val elements = pattern.split(OPERATOR.OR.value)
                for (element in elements) {
                    val trimmed = element.trim()
                    if (evaluable(trimmed, OPERATOR.AND)) {
                        if (evaluateAnd(message, trimmed)) {
                            return true
                        }
                    } else {
                        if (evaluateOr(message, trimmed)) {
                            return true
                        }
                    }
                }
                return false
            }
        } else {
            if (pattern.startsWith(OPERATOR.NOT.value)) {
                val check = pattern.replace(OPERATOR.NOT.value, "")
                if (message.containsIgnoreCase(check)) {
                    return false
                }
            } else {
                if (!message.containsIgnoreCase(pattern)) {
                    return false
                }
            }
            return true
        }
    }

    override fun evaluable(elements: List<String>): Boolean {
        for (element in elements) {
            if (element.contains(OPERATOR.AND.value) || element.contains(OPERATOR.OR.value)) {
                return true
            }
        }
        return false
    }

    override fun evaluable(elements: List<String>, operator: OPERATOR): Boolean {
        for (element in elements) {
            if (element.contains(operator.value)) {
                return true
            }
        }
        return false
    }

    override fun evaluateAnd(line: T, pattern: String): Boolean {
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

    override fun evaluateOr(line: T, pattern: String): Boolean {
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

}