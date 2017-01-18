package net.milosvasic.pussycat.gui.themes

import net.milosvasic.pussycat.gui.themes.color.INTENSITY
import net.milosvasic.pussycat.gui.themes.color.TYPE
import net.milosvasic.pussycat.gui.themes.font.FONT_WEIGHT
import java.awt.Color
import java.awt.Font
import java.io.InputStream

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

    override fun getTextColor(type: TYPE, intensity: INTENSITY): Color {
        return when (type) {
            TYPE.BASE -> {
                when (intensity) {
                    INTENSITY.LIGHT -> Color.WHITE
                    INTENSITY.MEDIUM -> Color.WHITE
                    INTENSITY.DARK -> Color.WHITE
                }
            }
        }
    }

    override fun getFont(weight: FONT_WEIGHT): Font {
        val input = when (weight) {
            FONT_WEIGHT.BLACK -> {
                javaClass.classLoader.getResourceAsStream("fonts/Roboto-Black.ttf")
            }
            FONT_WEIGHT.BLACK_ITALIC -> {
                javaClass.classLoader.getResourceAsStream("fonts/Roboto-BlackItalic.ttf")
            }
            FONT_WEIGHT.BOLD -> {
                javaClass.classLoader.getResourceAsStream("fonts/Roboto-Bold.ttf")
            }
            FONT_WEIGHT.BOLD_ITALIC -> {
                javaClass.classLoader.getResourceAsStream("fonts/Roboto-BoldItalic.ttf")
            }
            FONT_WEIGHT.ITALIC -> {
                javaClass.classLoader.getResourceAsStream("fonts/Roboto-Italic.ttf")
            }
            FONT_WEIGHT.LIGHT -> {
                javaClass.classLoader.getResourceAsStream("fonts/Roboto-Light.ttf")
            }
            FONT_WEIGHT.LIGHT_ITALIC -> {
                javaClass.classLoader.getResourceAsStream("fonts/Roboto-LightItalic.ttf")
            }
            FONT_WEIGHT.MEDIUM -> {
                javaClass.classLoader.getResourceAsStream("fonts/Roboto-Medium.ttf")
            }
            FONT_WEIGHT.MEDIUM_ITALIC -> {
                javaClass.classLoader.getResourceAsStream("fonts/Roboto-MediumItalic.ttf")
            }
            FONT_WEIGHT.THIN -> {
                javaClass.classLoader.getResourceAsStream("fonts/Roboto-Thin.ttf")
            }
            FONT_WEIGHT.THIN_ITALIC -> {
                javaClass.classLoader.getResourceAsStream("fonts/Roboto-ThinItalic.ttf")
            }
            else -> {
                javaClass.classLoader.getResourceAsStream("fonts/Roboto-Regular.ttf")
            }
        }
        return Font.createFont(Font.TRUETYPE_FONT, input)
    }

}