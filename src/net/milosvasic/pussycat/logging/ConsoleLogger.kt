package net.milosvasic.pussycat.logging

import net.milosvasic.pussycat.color.Color
import java.io.PrintStream
import java.util.*
import kotlin.reflect.KClass

class ConsoleLogger : Logger {

    private var output: PrintStream? = null
    private val loggingPattern: String

    init {
        output = System.out
        loggingPattern = "%s[ %s ][ %s ] %s: %s%s"
    }

    override fun v(tag: KClass<*>, message: String) {
        output!!.println(String.format(loggingPattern, Color.WHITE, LOG_LEVEL.VERBOSE, Date(), tag.simpleName, message, Color.RESET))
    }

    override fun d(tag: KClass<*>, message: String) {
        output!!.println(String.format(loggingPattern, Color.YELLOW, LOG_LEVEL.DEBUG, Date(), tag.simpleName, message, Color.RESET))
    }

    override fun i(tag: KClass<*>, message: String) {
        output!!.println(String.format(loggingPattern, Color.CYAN, LOG_LEVEL.INFORMATION, Date(), tag.simpleName, message, Color.RESET))
    }

    override fun w(tag: KClass<*>, message: String) {
        output!!.println(String.format(loggingPattern, Color.PURPLE, LOG_LEVEL.WARNING, Date(), tag.simpleName, message, Color.RESET))
    }

    override fun e(tag: KClass<*>, message: String) {
        output!!.println(String.format(loggingPattern, Color.RED, LOG_LEVEL.ERROR, Date(), tag.simpleName, message, Color.RESET))
    }

}