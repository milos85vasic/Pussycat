package net.milosvasic.pussycat.gui

import java.awt.event.ActionListener

data class PussycatIconButtonDefinition(
        val size: Int,
        val defaultIcon: String,
        val activeIcon: String,
        val toolTip: String,
        val action: ActionListener
)