package net.milosvasic.pussycat.gui.menu


import java.awt.BorderLayout
import java.awt.Dimension
import javax.swing.JMenuBar


class PussycatMainMenuPanel(preferedHeight: Int) : JMenuBar() {

    val file = PussycatMenu("File")
    val help = PussycatMenu("Pussycat")
    val about = PussycatMenuItem("About")
    val t1 = PussycatMenuItem("T 1")
    val t2 = PussycatMenuItem("T 2")
    val t3 = PussycatMenuItem("T 3")

    init {
        preferredSize = Dimension(width, preferedHeight)
        layout = BorderLayout()
        file.add(t1)
        file.add(t2)
        help.add(about)
        help.add(t3)
        add(file, BorderLayout.WEST)
//        add(help)
    }

}