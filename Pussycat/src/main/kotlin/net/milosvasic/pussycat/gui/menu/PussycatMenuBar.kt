package net.milosvasic.pussycat.gui.menu

import java.awt.Dimension
import javax.swing.JPanel


class PussycatMenuBar(width: Int, height: Int) : JPanel() {

    init {
        preferredSize = Dimension(width, height)
    }

}