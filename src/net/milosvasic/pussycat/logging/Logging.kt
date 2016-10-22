package net.milosvasic.pussycat.logging

import kotlin.reflect.KClass

/**
 * Base logging mechanism.
 */
interface Logger {

    /**
     * Verbose logging.

     * @param tag     Log tag.
     * @param message Log message.
     */
    fun v(tag: KClass<*>, message: String)

    /**
     * Debug logging.

     * @param tag     Log tag.
     * @param message Log message.
     */
    fun d(tag: KClass<*>, message: String)

    /**
     * Information logging.

     * @param tag     Log tag.
     * @param message Log message.
     */
    fun i(tag: KClass<*>, message: String)

    /**
     * Warning logging.

     * @param tag     Log tag.
     * @param message Log message.
     */
    fun w(tag: KClass<*>, message: String)

    /**
     * Error logging.

     * @param tag     Log tag.
     * @param message Log message.
     */
    fun e(tag: KClass<*>, message: String)

}