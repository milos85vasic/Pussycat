package net.milosvasic.pussycat.gui.themes

import net.milosvasic.pussycat.gui.themes.color.INTENSITY
import net.milosvasic.pussycat.gui.themes.color.TYPE
import net.milosvasic.pussycat.gui.themes.font.FONT_WEIGHT
import java.awt.Color
import java.awt.Font

abstract class Theme {

    abstract fun getColor(type: TYPE, intensity: INTENSITY): Color

    abstract fun getTextColor(type: TYPE, intensity: INTENSITY): Color

    abstract fun getTextColor(type: TYPE, intensity: INTENSITY, interactionTYPE: UI_INTERACTION_TYPE): Color

    abstract fun getFont(weight: FONT_WEIGHT): Font

    abstract fun getFontSize(): Float

}