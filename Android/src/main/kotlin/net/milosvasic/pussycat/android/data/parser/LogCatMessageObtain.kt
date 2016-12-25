package net.milosvasic.pussycat.android.data.parser

import com.android.ddmlib.logcat.LogCatMessage
import net.milosvasic.pussycat.android.data.AndroidLogCatMessage
import java.util.regex.Matcher


interface LogCatMessageObtain {

    fun getMessage(matcher: Matcher): AndroidLogCatMessage

}