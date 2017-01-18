package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.content.Labels
import net.milosvasic.pussycat.gui.themes.Theme

class PussycatMainMenuBar(theme: Theme, width: Int, height: Int) : PussycatMenuBar(theme, width, height) {

    val file = PussycatMenu(Labels.FILE)
    val commands = PussycatMenu(Labels.COMMANDS)
    val help = PussycatMenu(Labels.PUSSYCAT)

    val about = PussycatMenuItem("About")
    val t1 = PussycatMenuItem("T 1")
    val t2 = PussycatMenuItem("T 2")
    val t3 = PussycatMenuItem("T 3")

    init {
        file.addChild(t1)
        file.addChild(t2)
        help.addChild(about)
        help.addChild(t3)
        addChild(file)
        addChild(commands)
        addChild(help)
    }

}