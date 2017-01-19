package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.gui.themes.Theme


class PussycatMainMenuBar(theme: Theme, width: Int, height: Int, items: List<PussycatMenu>) : PussycatMenuBar(theme, width, height) {

    init {
        for (item in items) {
            addChild(item)
        }
    }

}