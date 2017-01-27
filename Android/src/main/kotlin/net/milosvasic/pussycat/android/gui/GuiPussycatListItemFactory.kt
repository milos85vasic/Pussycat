package net.milosvasic.pussycat.android.gui

import com.android.ddmlib.Log
import net.milosvasic.pussycat.android.data.AndroidLogCatMessage
import net.milosvasic.pussycat.gui.PussycatListItem
import net.milosvasic.pussycat.gui.PussycatListItemFactory
import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.logging.LOG_TYPE

class GuiPussycatListItemFactory(val theme: Theme) : PussycatListItemFactory<AndroidLogCatMessage> {

    // data.get().indexOf(line)

    fun getPussycatListItem(line: AndroidLogCatMessage, index: Int): PussycatListItem {
        val color = when (line.logLevel) {
            Log.LogLevel.DEBUG -> {
                theme.getTextColor(LOG_TYPE.DEBUG)
            }
            Log.LogLevel.INFO -> {
                theme.getTextColor(LOG_TYPE.INFORMATION)
            }
            Log.LogLevel.WARN -> {
                theme.getTextColor(LOG_TYPE.WARNING)
            }
            Log.LogLevel.ERROR -> {
                theme.getTextColor(LOG_TYPE.ERROR)
            }
            else -> {
                theme.getTextColor(LOG_TYPE.VERBOSE)
            }
        }

        val itemPairs = mutableListOf<Pair<String, Int>>()
        itemPairs.add(Pair(line.time, AndroidLogCatMessage.LENGTHS.SPACING_DEFAULT))
        itemPairs.add(Pair("${line.pid}", AndroidLogCatMessage.LENGTHS.SPACING_SHORT))
        itemPairs.add(Pair("${line.tid}", AndroidLogCatMessage.LENGTHS.SPACING_SHORT))
        itemPairs.add(Pair(line.appName, AndroidLogCatMessage.LENGTHS.SPACING_LONG))
        itemPairs.add(Pair(line.tag, AndroidLogCatMessage.LENGTHS.SPACING_LONG))

        var message = line.msg
        for (stacktraceItem in line.getStacktrace()) {
            val builder = StringBuilder()
            for (itemPair in itemPairs) {
                val spaces = itemPair.second
                for (x in 0..spaces) {
                    builder.append(PussycatListItem.SPACING)
                }
            }
            for (x in 0..(AndroidLogCatMessage.LENGTHS.SPACING_SHORT)) {
                builder.append(PussycatListItem.SPACING)
            }
            builder.append(stacktraceItem)
            message += "\n" + builder.toString()
        }

        itemPairs.add(Pair(message, AndroidLogCatMessage.LENGTHS.NO_SPACING_APPLIED))
        return PussycatListItem.obtain(
                theme,
                itemPairs,
                index,
                color
        )
    }

    override fun obtain(item: AndroidLogCatMessage, index: Int): PussycatListItem {
        return getPussycatListItem(item, index)
    }

}