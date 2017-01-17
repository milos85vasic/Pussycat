package net.milosvasic.pussycat.gui.menu


import net.milosvasic.pussycat.gui.themes.Theme


class PussycatMainMenu(theme: Theme) : PussycatMenuBar(theme) {

    val file = PussycatMenu(theme, "File")
    val help = PussycatMenu(theme, "Pussycat")

    init {
        val about = PussycatMenuItem(theme, "About")
        help.add(about)
        add(file)
        add(help)
    }

}