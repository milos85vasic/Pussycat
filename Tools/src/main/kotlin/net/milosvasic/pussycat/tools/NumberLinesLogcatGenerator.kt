package net.milosvasic.pussycat.tools

import net.milosvasic.pussycat.PussycatAbstract
import java.io.File


class NumberLinesLogcatGenerator : LogcatGenerator {

    override fun generate(count: Int) {
        val home = PussycatAbstract.getPussycatHome()
        val localFile = File(home.absolutePath, count.toString() + "_lines.txt")
        if (!localFile.exists()) {
            for (x in 0..count) {
                localFile.appendText("Line $x\n")
            }
        }
    }

}