package net.milosvasic.pussycat.android.data

import com.android.ddmlib.logcat.LogCatMessage
import java.util.regex.Matcher


interface LogCatMessageObtain {

    fun getMessage(matcher: Matcher): LogCatMessage

}