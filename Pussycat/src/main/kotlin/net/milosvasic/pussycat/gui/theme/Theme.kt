package net.milosvasic.pussycat.gui.theme

import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import net.milosvasic.pussycat.gui.theme.font.FONT_WEIGHT
import java.awt.Color
import java.awt.Font

abstract class Theme {

    abstract fun getColor(type: TYPE, intensity: INTENSITY): Color

    abstract fun getTextColor(type: TYPE, intensity: INTENSITY): Color

    abstract fun getTextColor(type: TYPE, intensity: INTENSITY, interactionTYPE: UI_INTERACTION_TYPE): Color

    abstract fun getFont(weight: FONT_WEIGHT): Font

    abstract fun getFontSize(): Float

}