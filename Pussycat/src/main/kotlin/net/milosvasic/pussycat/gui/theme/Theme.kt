package net.milosvasic.pussycat.gui.theme

import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import net.milosvasic.pussycat.gui.theme.font.FONT_WEIGHT
import net.milosvasic.pussycat.logging.LOG_TYPE
import java.awt.Color
import java.awt.Dimension
import java.awt.Font
import javax.swing.JComponent
import javax.swing.border.Border

abstract class Theme {

    abstract fun getName(): String

    abstract fun getColor(type: TYPE, intensity: INTENSITY): Color

    abstract fun getColor(type: TYPE, intensity: INTENSITY, opacity: Int): Color

    abstract fun getTextColor(type: TYPE, intensity: INTENSITY): Color

    abstract fun getTextColor(type: TYPE, intensity: INTENSITY, interactionTYPE: UI_INTERACTION_TYPE): Color

    abstract fun getTextColor(logType: LOG_TYPE): Color

    abstract fun getFont(weight: FONT_WEIGHT): Font?

    abstract fun getFont(weight: FONT_WEIGHT, size: Float): Font?

    abstract fun getFontSize(): Float

    abstract fun getBorder(comp: JComponent): Border

}