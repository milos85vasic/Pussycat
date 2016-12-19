package net.milosvasic.pussycat.terminal

import java.io.BufferedWriter
import java.io.FileDescriptor
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import java.util.concurrent.LinkedBlockingQueue


class TerminalPrinter : Printer {

    var printThread: Thread? = null
    val queue = LinkedBlockingQueue<String>()
    val out = BufferedWriter(OutputStreamWriter(FileOutputStream(FileDescriptor.out), "ASCII"), 512)

    override fun printLine(text: String?) {
        queue.add(text)
        if (printThread == null) {
            printThread = Thread(Runnable {
                Thread.currentThread().name = "Printing thread"
                while (!Thread.currentThread().isInterrupted) {
                    val item = queue.poll()
                    if (item != null) {
                        out.write("$item\n")
                        out.flush()
                    }
                }
            })
            printThread?.start()
        }
    }

}