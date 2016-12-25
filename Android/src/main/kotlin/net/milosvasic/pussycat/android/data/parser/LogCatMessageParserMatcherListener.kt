package net.milosvasic.pussycat.android.data.parser

import net.milosvasic.pussycat.android.data.AndroidLogCatMessage


interface LogCatMessageParserMatcherListener {

    fun onMatch(success: Boolean, message: AndroidLogCatMessage?)

}