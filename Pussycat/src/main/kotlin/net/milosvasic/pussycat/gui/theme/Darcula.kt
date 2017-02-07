package net.milosvasic.pussycat.gui.theme

import net.milosvasic.pussycat.gui.*
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import net.milosvasic.pussycat.gui.theme.font.FONT_WEIGHT
import net.milosvasic.pussycat.logging.LOG_TYPE
import java.awt.Color
import java.awt.Font
import java.util.*
import javax.swing.JComponent
import javax.swing.border.Border
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder


open class Darcula : Theme() {

    private val fonts = HashMap<String, Font>()

    override fun getName(): String {
        return this.javaClass.simpleName
    }

    override fun getColor(type: TYPE, intensity: INTENSITY): Color {
        return getColor(type, intensity, 255)
    }

    override fun getColor(type: TYPE, intensity: INTENSITY, opacity: Int): Color {
        return when (type) {
            TYPE.BASE -> {
                when (intensity) {
                    INTENSITY.LIGHT -> Color(60, 63, 65, opacity)
                    INTENSITY.MEDIUM -> Color(49, 51, 53, opacity)
                    INTENSITY.DARK -> Color(43, 43, 43, opacity)
                }
            }
            TYPE.MAIN_COLOR_1 -> {
                when (intensity) {
                    INTENSITY.LIGHT -> Color(224, 54, 198, opacity)
                    INTENSITY.MEDIUM -> Color(156, 50, 140, opacity)
                    INTENSITY.DARK -> Color(95, 47, 99, opacity)
                }
            }
            TYPE.MAIN_COLOR_2 -> {
                when (intensity) {
                    INTENSITY.LIGHT -> Color(102, 77, 255, opacity)
                    INTENSITY.MEDIUM -> Color(77, 57, 190, opacity)
                    INTENSITY.DARK -> Color(57, 54, 133, opacity)
                }
            }
        }
    }

    override fun getTextColor(type: TYPE, intensity: INTENSITY): Color {
        return when (type) {
            TYPE.BASE -> {
                when (intensity) {
                    INTENSITY.LIGHT -> Color(201, 201, 201)
                    INTENSITY.MEDIUM -> Color(201, 201, 201)
                    INTENSITY.DARK -> Color(201, 201, 201)
                }
            }
            TYPE.MAIN_COLOR_1 -> {
                when (intensity) {
                    INTENSITY.LIGHT -> Color.WHITE
                    INTENSITY.MEDIUM -> Color.WHITE
                    INTENSITY.DARK -> Color.WHITE
                }
            }
            TYPE.MAIN_COLOR_2 -> {
                when (intensity) {
                    INTENSITY.LIGHT -> Color.WHITE
                    INTENSITY.MEDIUM -> Color.WHITE
                    INTENSITY.DARK -> Color.WHITE
                }
            }
        }
    }

    override fun getTextColor(type: TYPE, intensity: INTENSITY, interactionTYPE: UI_INTERACTION_TYPE): Color {
        return when (type) {
            TYPE.BASE -> {
                when (intensity) {
                    INTENSITY.LIGHT -> when (interactionTYPE) {
                        UI_INTERACTION_TYPE.HOVER -> {
                            getColor(TYPE.MAIN_COLOR_1, INTENSITY.LIGHT)
                        }
                        UI_INTERACTION_TYPE.PRESSED -> {
                            getColor(TYPE.MAIN_COLOR_2, INTENSITY.LIGHT)
                        }
                        else -> Color.WHITE
                    }
                    INTENSITY.MEDIUM -> when (interactionTYPE) {
                        UI_INTERACTION_TYPE.HOVER -> {
                            getColor(TYPE.MAIN_COLOR_1, INTENSITY.MEDIUM)
                        }
                        UI_INTERACTION_TYPE.PRESSED -> {
                            getColor(TYPE.MAIN_COLOR_2, INTENSITY.MEDIUM)
                        }
                        else -> Color.WHITE
                    }
                    INTENSITY.DARK -> when (interactionTYPE) {
                        UI_INTERACTION_TYPE.HOVER -> {
                            getColor(TYPE.MAIN_COLOR_1, INTENSITY.DARK)
                        }
                        UI_INTERACTION_TYPE.PRESSED -> {
                            getColor(TYPE.MAIN_COLOR_2, INTENSITY.DARK)
                        }
                        else -> Color.WHITE
                    }
                }
            }
            TYPE.MAIN_COLOR_1 -> {
                when (intensity) {
                    INTENSITY.LIGHT -> when (interactionTYPE) {
                        UI_INTERACTION_TYPE.HOVER -> {
                            Color.WHITE
                        }
                        UI_INTERACTION_TYPE.PRESSED -> {
                            Color.WHITE
                        }
                        else -> Color.WHITE
                    }
                    INTENSITY.MEDIUM -> when (interactionTYPE) {
                        UI_INTERACTION_TYPE.HOVER -> {
                            Color.WHITE
                        }
                        UI_INTERACTION_TYPE.PRESSED -> {
                            Color.WHITE
                        }
                        else -> Color.WHITE
                    }
                    INTENSITY.DARK -> when (interactionTYPE) {
                        UI_INTERACTION_TYPE.HOVER -> {
                            Color.WHITE
                        }
                        UI_INTERACTION_TYPE.PRESSED -> {
                            Color.WHITE
                        }
                        else -> Color.WHITE
                    }
                }
            }
            TYPE.MAIN_COLOR_2 -> {
                when (intensity) {
                    INTENSITY.LIGHT -> when (interactionTYPE) {
                        UI_INTERACTION_TYPE.HOVER -> {
                            Color.WHITE
                        }
                        UI_INTERACTION_TYPE.PRESSED -> {
                            Color.WHITE
                        }
                        else -> Color.WHITE
                    }
                    INTENSITY.MEDIUM -> when (interactionTYPE) {
                        UI_INTERACTION_TYPE.HOVER -> {
                            Color.WHITE
                        }
                        UI_INTERACTION_TYPE.PRESSED -> {
                            Color.WHITE
                        }
                        else -> Color.WHITE
                    }
                    INTENSITY.DARK -> when (interactionTYPE) {
                        UI_INTERACTION_TYPE.HOVER -> {
                            Color.WHITE
                        }
                        UI_INTERACTION_TYPE.PRESSED -> {
                            Color.WHITE
                        }
                        else -> Color.WHITE
                    }
                }
            }
        }
    }

    override fun getTextColor(logType: LOG_TYPE): Color {
        return when (logType) {
            LOG_TYPE.DEBUG -> {
                Color.WHITE
            }
            LOG_TYPE.INFORMATION -> {
                Color.CYAN
            }
            LOG_TYPE.WARNING -> {
                Color.ORANGE
            }
            LOG_TYPE.ERROR -> {
                Color.RED
            }
            else -> {
                Color.GRAY
            }
        }
    }

    override fun getFont(weight: FONT_WEIGHT, size: Float): Font? {
        val fontName = getFontName(weight)
        val identifier = "$fontName@$size"
        var font = fonts[identifier]
        if (font == null) {
            font = getFont(weight)
            font = font?.deriveFont(size)
            if (font != null) {
                fonts.put(identifier, font)
            }
        }
        return font
    }

    override fun getFont(weight: FONT_WEIGHT): Font? {
        val fontName = getFontName(weight)
        var font = fonts[fontName]
        if (font == null) {
            val input = javaClass.classLoader.getResourceAsStream(fontName)
            font = Font.createFont(Font.TRUETYPE_FONT, input)
            fonts.put(fontName, font)
        }
        return font
    }

    override fun getFontSize(): Float {
        return 14f
    }

    override fun getBorder(comp: JComponent): Border {
        return when (comp) {
            is PussycatContent -> {
                CompoundBorder(comp.border, EmptyBorder(5, 5, 5, 5))
            }
            is PussycatListItem -> {
                CompoundBorder(comp.border, EmptyBorder(5, 5, 5, 5))
            }
            is PussycatMenu -> {
                CompoundBorder(comp.border, EmptyBorder(10, 10, 10, 10))
            }
            is PussycatToolbar -> {
                EmptyBorder(0, 13, 0, 13)
            }
            is PussycatTextField -> {
                CompoundBorder(comp.border, EmptyBorder(5, 5, 5, 5))
            }
            else -> {
                CompoundBorder(comp.border, EmptyBorder(0, 0, 0, 0))
            }
        }
    }

    private fun getFontName(weight: FONT_WEIGHT): String {
        return when (weight) {
            FONT_WEIGHT.BLACK -> {
                "Darcula/fonts/Roboto-Black.ttf"
            }
            FONT_WEIGHT.BLACK_ITALIC -> {
                "Darcula/fonts/Roboto-BlackItalic.ttf"
            }
            FONT_WEIGHT.BOLD -> {
                "Darcula/fonts/Roboto-Bold.ttf"
            }
            FONT_WEIGHT.BOLD_ITALIC -> {
                "Darcula/fonts/Roboto-BoldItalic.ttf"
            }
            FONT_WEIGHT.ITALIC -> {
                "Darcula/fonts/Roboto-Italic.ttf"
            }
            FONT_WEIGHT.LIGHT -> {
                "Darcula/fonts/Roboto-Light.ttf"
            }
            FONT_WEIGHT.LIGHT_ITALIC -> {
                "Darcula/fonts/Roboto-LightItalic.ttf"
            }
            FONT_WEIGHT.MEDIUM -> {
                "Darcula/fonts/Roboto-Medium.ttf"
            }
            FONT_WEIGHT.MEDIUM_ITALIC -> {
                "Darcula/fonts/Roboto-MediumItalic.ttf"
            }
            FONT_WEIGHT.THIN -> {
                "Darcula/fonts/Roboto-Thin.ttf"
            }
            FONT_WEIGHT.THIN_ITALIC -> {
                "Darcula/fonts/Roboto-ThinItalic.ttf"
            }
            else -> {
                "Darcula/fonts/Roboto-Regular.ttf"
            }
        }
    }

}