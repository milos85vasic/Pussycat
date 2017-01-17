package net.milosvasic.pussycat.gui.themes

import net.milosvasic.pussycat.gui.themes.color.INTENSITY
import net.milosvasic.pussycat.gui.themes.color.TYPE
import java.awt.Color

abstract class Theme {

    abstract fun getColor(type: TYPE, intensity: INTENSITY): Color

    abstract fun getTextColor(type: TYPE, intensity: INTENSITY): Color

}