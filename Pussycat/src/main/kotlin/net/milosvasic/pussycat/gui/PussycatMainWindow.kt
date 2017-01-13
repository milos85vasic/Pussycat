package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.application.ApplicationInformation
import net.milosvasic.pussycat.gui.themes.Theme

class PussycatMainWindow(theme: Theme, information: ApplicationInformation) : PussycatWindow(theme) {

    init {
        title = "${information.name} V${information.version} by ${information.author}"
    }

    override fun initialize() {
        super.initialize()
        // TODO: Add the rest of gui elements
    }

}