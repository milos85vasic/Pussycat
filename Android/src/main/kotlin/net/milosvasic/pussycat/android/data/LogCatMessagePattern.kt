package net.milosvasic.pussycat.android.data

import java.util.regex.Matcher
import java.util.regex.Pattern


open class LogCatMessagePattern(val regex: String, val messageObtain: LogCatMessageObtain) {

    fun compile(): Pattern {
        return Pattern.compile(regex)
    }

    fun getMessage(matcher: Matcher): AndroidLogCatMessage {
        return messageObtain.getMessage(matcher)
    }

}