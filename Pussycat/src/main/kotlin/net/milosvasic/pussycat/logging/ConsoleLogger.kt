package net.milosvasic.pussycat.logging

import net.milosvasic.pussycat.color.Color
import java.io.PrintStream
import java.util.*
import kotlin.reflect.KClass

public class ConsoleLogger : Logger {

    private var output: PrintStream? = null
    private val loggingPattern: String

    init {
        output = System.out
        loggingPattern = "%s[ %s ][ %s ] %s: %s%s"
    }

    override fun v(tag: KClass<*>, message: String) {
        output!!.println(String.format(loggingPattern, Color.WHITE, LOG_TYPE.VERBOSE, Date(), tag.simpleName, message, Color.RESET))
    }

    override fun d(tag: KClass<*>, message: String) {
        output!!.println(String.format(loggingPattern, Color.YELLOW, LOG_TYPE.DEBUG, Date(), tag.simpleName, message, Color.RESET))
    }

    override fun i(tag: KClass<*>, message: String) {
        output!!.println(String.format(loggingPattern, Color.CYAN, LOG_TYPE.INFORMATION, Date(), tag.simpleName, message, Color.RESET))
    }

    override fun w(tag: KClass<*>, message: String) {
        output!!.println(String.format(loggingPattern, Color.PURPLE, LOG_TYPE.WARNING, Date(), tag.simpleName, message, Color.RESET))
    }

    override fun e(tag: KClass<*>, message: String) {
        output!!.println(String.format(loggingPattern, Color.RED, LOG_TYPE.ERROR, Date(), tag.simpleName, message, Color.RESET))
    }

}