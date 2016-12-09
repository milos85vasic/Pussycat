package net.milosvasic.pussycat.android.data

import com.android.ddmlib.logcat.LogCatMessage
import java.util.regex.Matcher
import java.util.regex.Pattern


class LogCatMessagePattern(val regex: String, val messageObtain: LogCatMessageObtain) {

    fun compile(): Pattern {
        return Pattern.compile(regex)
    }

    fun getMessage(matcher: Matcher): LogCatMessage {
        return messageObtain.getMessage(matcher)
    }

}