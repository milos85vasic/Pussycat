package net.milosvasic.pussycat.gui.themes

import net.milosvasic.pussycat.gui.themes.color.INTENSITY
import net.milosvasic.pussycat.gui.themes.color.TYPE
import java.awt.Color

class Darcula : Theme() {

    override fun getColor(type: TYPE, intensity: INTENSITY): Color {
        return when (type) {
            TYPE.BASE -> {
                when (intensity) {
                    INTENSITY.LIGHT -> Color(60, 63, 65)
                    INTENSITY.MEDIUM -> Color(49, 51, 53)
                    INTENSITY.DARK -> Color(43, 43, 43)
                }
            }
        }
    }

}