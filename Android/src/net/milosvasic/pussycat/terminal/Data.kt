package net.milosvasic.pussycat.terminal

import net.milosvasic.pussycat.core.LOGCAT_TAG_TYPE
import net.milosvasic.pussycat.core.common.DataFilter
import net.milosvasic.pussycat.core.data.DataAbstract
import net.milosvasic.pussycat.utils.Text
import java.util.concurrent.CopyOnWriteArrayList

class Data(filter: DataFilter<CopyOnWriteArrayList<String>, String>) : DataAbstract(filter) {

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

    private fun getTag(line: String): String? {
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.V_LIVE)) {
            return LOGCAT_TAG_TYPE.V_LIVE
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.V_FILESYSTEM)) {
            return LOGCAT_TAG_TYPE.V_FILESYSTEM
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.D_LIVE)) {
            return LOGCAT_TAG_TYPE.D_LIVE
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.D_FILESYSTEM)) {
            return LOGCAT_TAG_TYPE.D_FILESYSTEM
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.I_LIVE)) {
            return LOGCAT_TAG_TYPE.I_LIVE
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.I_FILESYSTEM)) {
            return LOGCAT_TAG_TYPE.I_FILESYSTEM
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.W_LIVE)) {
            return LOGCAT_TAG_TYPE.W_LIVE
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.W_FILESYSTEM)) {
            return LOGCAT_TAG_TYPE.W_FILESYSTEM
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.E_LIVE)) {
            return LOGCAT_TAG_TYPE.E_LIVE
        }
        if (line.containsIgnoreCase(LOGCAT_TAG_TYPE.E_FILESYSTEM)) {
            return LOGCAT_TAG_TYPE.E_FILESYSTEM
        }
        return null
    }

    fun String.containsIgnoreCase(word: String): Boolean {
        return this.toLowerCase().contains(word.toLowerCase())
    }

}