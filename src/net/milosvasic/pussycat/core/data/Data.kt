package net.milosvasic.pussycat.core.data

import net.milosvasic.pussycat.core.common.DataFilter
import net.milosvasic.pussycat.utils.Text
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

    override fun filterOk(line: String): Boolean {
        if (Text.isEmpty(pattern)) {
            return true
        }
        if (!pattern.contains("&&") && !pattern.contains("||")) {
            if (pattern.startsWith("!")) {
                val check = pattern.replace("!", "")
                if (line.containsIgnoreCase(check)) {
                    return false
                }
            } else {
                if (!line.containsIgnoreCase(pattern)) {
                    return false
                }
            }
            return true
        }
        if (pattern.contains("&&")) {
            val params = pattern.split("&&")
            for (item in params) {
                var check = item.trim()
                if (check.startsWith("!")) {
                    check = check.replace("!", "")
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
        if (pattern.contains("||")) {
            val params = pattern.split("||")
            for (item in params) {
                var check = item.trim()
                if (check.startsWith("!")) {
                    check = check.replace("!", "")
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
        return true
    }

    fun String.containsIgnoreCase(word: String): Boolean {
        return this.toLowerCase().contains(word.toLowerCase())
    }

    protected abstract fun getTag(line: String): String?

}