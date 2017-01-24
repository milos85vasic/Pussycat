package net.milosvasic.pussycat.gui.theme

import net.milosvasic.pussycat.gui.PussycatContent
import net.milosvasic.pussycat.gui.PussycatListItem
import net.milosvasic.pussycat.gui.PussycatMenu
import net.milosvasic.pussycat.gui.PussycatScrollPane
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import net.milosvasic.pussycat.gui.theme.font.FONT_WEIGHT
import net.milosvasic.pussycat.logging.LOG_TYPE
import java.awt.Color
import java.awt.Font
import javax.swing.JComponent
import javax.swing.border.Border
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder


open class Darcula : Theme() {

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
                    INTENSITY.LIGHT -> Color.WHITE
                    INTENSITY.MEDIUM -> Color.WHITE
                    INTENSITY.DARK -> Color.WHITE
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

    override fun getFontSize(): Float {
        return 14f
    }

    override fun getBorder(comp: JComponent): Border {
        return when (comp) {
            is PussycatContent -> {
                CompoundBorder(comp.border, EmptyBorder(5, 5, 5, 5))
            }
            is PussycatListItem<*> -> {
                CompoundBorder(comp.border, EmptyBorder(5, 5, 5, 5))
            }
            is PussycatMenu -> {
                CompoundBorder(comp.border, EmptyBorder(10, 10, 10, 10))
            }
//            is PussycatScrollPane -> {
//                CompoundBorder(comp.border, EmptyBorder(10, 0, 10, 0))
//            }
            else -> {
                CompoundBorder(comp.border, EmptyBorder(0, 0, 0, 0))
            }
        }
    }

}