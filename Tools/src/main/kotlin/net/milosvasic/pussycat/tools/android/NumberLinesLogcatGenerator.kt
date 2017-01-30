package net.milosvasic.pussycat.tools.android

import net.milosvasic.pussycat.PussycatAbstract
import net.milosvasic.pussycat.tools.LogGenerator
import java.io.File
import java.text.SimpleDateFormat
import java.util.*


class NumberLinesLogcatGenerator : LogGenerator {

    override fun generate(count: Int) {
        val home = PussycatAbstract.getPussycatHome()
        val localFile = File(home.absolutePath, count.toString() + "_lines.txt")
        if (!localFile.exists()) {
            for (x in 0..count) {
                val dateFormat = SimpleDateFormat("M-d H:m:s.S")
                val date = dateFormat.format(Date())
                localFile.appendText("$date $x $x I ${this.javaClass.simpleName}: [ $x ] Line\n")
            }
        }
    }

}