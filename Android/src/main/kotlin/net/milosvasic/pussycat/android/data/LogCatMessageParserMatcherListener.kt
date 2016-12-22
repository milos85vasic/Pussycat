package net.milosvasic.pussycat.android.data


interface LogCatMessageParserMatcherListener {

    fun onMatch(success: Boolean, message: AndroidLogCatMessage?)

}