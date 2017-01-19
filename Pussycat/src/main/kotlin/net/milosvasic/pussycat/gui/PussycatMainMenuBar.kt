package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.themes.Theme


class PussycatMainMenuBar(theme: Theme, width: Int, height: Int, items: List<PussycatMenu>) : PussycatMenuBar(theme, width, height) {

    val file = PussycatMenu("ZZZZ")

    init {
        file.add(PussycatMenuItem("ssss"))
        file.add(PussycatMenuItem("mmmm"))
        add(file)
        for (item in items) {
            add(item)
        }
    }

}