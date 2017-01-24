package net.milosvasic.pussycat.android.gui

import com.android.ddmlib.Log
import net.milosvasic.pussycat.android.data.AndroidLogCatMessage
import net.milosvasic.pussycat.gui.PussycatLabel
import net.milosvasic.pussycat.gui.PussycatListItem
import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.logging.LOG_TYPE
import java.awt.Color


class GuiPussycatListItem(theme: Theme, value: AndroidLogCatMessage, index: Int) : PussycatListItem<AndroidLogCatMessage>(theme, value, index) {

    val logLevel = PussycatLabel(value.logLevel.stringValue.substring(0, 1).toUpperCase(), theme)

    init {
        logLevel.background = background
        logLevel.foreground = getTextColor(value)
        add(logLevel)
    }

    override fun getColumns(): Int {
        return 7
    }

    override fun getRows(): Int {
        return 1
    }

    override fun getTextColor(value: AndroidLogCatMessage): Color {
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

}