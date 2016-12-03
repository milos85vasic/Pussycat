package net.milosvasic.pussycat.core.data

import net.milosvasic.pussycat.core.common.DataFilter
import java.util.concurrent.CopyOnWriteArrayList

abstract class Data(filter: DataFilter<CopyOnWriteArrayList<String>, String>) : DataAbstract(filter) {

    override fun addData(line: String) {
        val tag = getTag(line)
        if (tag != null) {
            val firstTagOccurance = line.indexOf(tag)
            var splited = line.substring(firstTagOccurance + tag.length, line.lastIndex + 1)
            val classEnd = splited.indexOf(":")
            splited = splited.substring(classEnd + 1, splited.lastIndex + 1)
            if (splited.startsWith(" \t")) {
                data[data.lastIndex] = "${data.last()}\n$line"
            } else {
                data.add(line)
            }
        } else {
            if (data.isEmpty()) {
                data.add(line)
            } else {
                val replace = "${data.last()}\n\t$line"
                data[data.lastIndex] = replace
            }
        }
    }

    override fun evaluate(line: String): Boolean {
        if (evaluable(pattern)) {
            if (!evaluable(pattern, OPERATOR.OR)) {
                return evaluateAnd(line, pattern)
            } else {
                val elements = pattern.split(OPERATOR.OR.value)
                for (element in elements) {
                    if (evaluable(element, OPERATOR.AND)) {
                        if (evaluateAnd(line, element)) {
                            return true
                        }
                    } else {
                        if (evaluateOr(line, element)) {
                            return true
                        }
                    }
                }
                return false
            }
        }
        return true
    }

    private fun evaluable(element: String): Boolean {
        return evaluable(Array(1, { element }))
    }

    private fun evaluable(element: String, operator: OPERATOR): Boolean {
        return evaluable(Array(1, { element }), operator)
    }

    private fun evaluable(elements: Array<String>): Boolean {
        return elements.none { it.contains(OPERATOR.AND.value) || it.contains(OPERATOR.OR.value) }
    }

    private fun evaluable(elements: Array<String>, operator: OPERATOR): Boolean {
        return elements.none { it.contains(operator.value) }
    }

    private fun evaluateAnd(line: String, pattern: String): Boolean {
        val ands = pattern.split(OPERATOR.AND.value)
        for (element in ands) {
            if (element.startsWith(OPERATOR.NOT.value)) {
                val check = element.replace(OPERATOR.NOT.value, "")
                if (line.containsIgnoreCase(check)) {
                    return false
                }
            } else {
                if (!line.containsIgnoreCase(pattern)) {
                    return false
                }
            }
        }
        return true
    }

    private fun evaluateOr(line: String, pattern: String): Boolean {
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

    fun String.containsIgnoreCase(word: String): Boolean {
        return this.toLowerCase().contains(word.toLowerCase())
    }

    protected abstract fun getTag(line: String): String?

}