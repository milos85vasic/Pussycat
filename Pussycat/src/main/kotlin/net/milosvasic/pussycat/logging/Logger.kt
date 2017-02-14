package net.milosvasic.pussycat.logging


import net.milosvasic.logger.ConsoleLogger
import net.milosvasic.logger.FilesystemLogger
import net.milosvasic.logger.Logger
import net.milosvasic.pussycat.PussycatAbstract
import net.milosvasic.pussycat.gui.content.Labels
import java.io.File
import kotlin.reflect.KClass

class Logger : Logger {

    val console = ConsoleLogger()
    val filesystem = FilesystemLogger(getHome())

    override fun c(tag: KClass<*>, message: String) {
        console.c(tag, message)
        filesystem.c(tag, message)
    }

    override fun d(tag: KClass<*>, message: String) {
        console.d(tag, message)
        filesystem.d(tag, message)
    }

    override fun e(tag: KClass<*>, message: String) {
        console.e(tag, message)
        filesystem.e(tag, message)
    }

    override fun i(tag: KClass<*>, message: String) {
        console.i(tag, message)
        filesystem.i(tag, message)
    }

    override fun n(tag: KClass<*>, message: String) {
        console.n(tag, message)
        filesystem.n(tag, message)
    }

    override fun v(tag: KClass<*>, message: String) {
        console.v(tag, message)
        filesystem.v(tag, message)
    }

    override fun w(tag: KClass<*>, message: String) {
        console.w(tag, message)
        filesystem.w(tag, message)
    }

    fun getHome(): File {
        val builder = StringBuilder()
                .append(PussycatAbstract.getPussycatHome().absolutePath)
                .append(File.separator)
                .append(Labels.LOGS)
        val root = File(builder.toString())
        if (!root.exists()) {
            root.mkdirs()
        }
        return root
    }

}