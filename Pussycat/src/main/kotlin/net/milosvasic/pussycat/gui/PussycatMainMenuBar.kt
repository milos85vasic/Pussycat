package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.content.Labels
import java.awt.BorderLayout
import java.awt.Dimension


class PussycatMainMenuBar(width: Int, height: Int) : PussycatBar(width, height) {

    val file = PussycatMenuButton(Labels.FILE)
    val help = PussycatMenuButton(Labels.PUSSYCAT)

//    val about = PussycatMenuItem("About")
//    val t1 = PussycatMenuItem("T 1")
//    val t2 = PussycatMenuItem("T 2")
//    val t3 = PussycatMenuItem("T 3")

    init {
//        file.add(t1)
//        file.add(t2)
//        help.add(about)
//        help.add(t3)
        add(file, BorderLayout.WEST)
        add(help, BorderLayout.WEST)
    }

}