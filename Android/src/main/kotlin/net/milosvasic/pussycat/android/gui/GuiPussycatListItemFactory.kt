package net.milosvasic.pussycat.android.gui

import com.android.ddmlib.Log
import net.milosvasic.pussycat.android.data.AndroidLogCatMessage
import net.milosvasic.pussycat.gui.PussycatListItem
import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.logging.LOG_TYPE
import java.awt.Color


class GuiPussycatListItemFactory {

    companion object {
        fun create(theme: Theme, value: AndroidLogCatMessage, index: Int): PussycatListItem {

            fun getTextColor(): Color {
                return when (value.logLevel) {
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
            }

            val color = getTextColor()
            val item = PussycatListItem(theme, index, color)
                    .append(value.time, AndroidLogCatMessage.LENGTHS.SPACING_DEFAULT)
                    .append("${value.pid}", AndroidLogCatMessage.LENGTHS.SPACING_SHORT)
                    .append("${value.tid}", AndroidLogCatMessage.LENGTHS.SPACING_SHORT)
                    .append(value.appName, AndroidLogCatMessage.LENGTHS.SPACING_LONG)
                    .append(value.tag, AndroidLogCatMessage.LENGTHS.SPACING_LONG)
                    .append(value.msg)

            return item
        }
    }

}