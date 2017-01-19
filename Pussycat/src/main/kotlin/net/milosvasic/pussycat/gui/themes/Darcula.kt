package net.milosvasic.pussycat.gui.themes

import net.milosvasic.pussycat.gui.themes.color.INTENSITY
import net.milosvasic.pussycat.gui.themes.color.TYPE
import net.milosvasic.pussycat.gui.themes.font.FONT_WEIGHT
import java.awt.Color
import java.awt.Font


open class Darcula : Theme() {

    override fun getColor(type: TYPE, intensity: INTENSITY): Color {
        return when (type) {
            TYPE.BASE -> {
                when (intensity) {
                    INTENSITY.LIGHT -> Color(60, 63, 65)
                    INTENSITY.MEDIUM -> Color(49, 51, 53)
                    INTENSITY.DARK -> Color(43, 43, 43)
                }
            }
            TYPE.MAIN_COLOR_1 -> {
                when (intensity) {
                    INTENSITY.LIGHT -> Color(224, 54, 198)
                    INTENSITY.MEDIUM -> Color(156, 50, 140)
                    INTENSITY.DARK -> Color(95, 47, 99)
                }
            }
            TYPE.MAIN_COLOR_2 -> {
                when (intensity) {
                    INTENSITY.LIGHT -> Color(102, 77, 255)
                    INTENSITY.MEDIUM -> Color(77, 57, 190)
                    INTENSITY.DARK -> Color(57, 54, 133)
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

    override fun getTextColor(type: TYPE, intensity: INTENSITY, interactionState: UI_INTERACTION_STATE): Color {
        return when (type) {
            TYPE.BASE -> {
                when (intensity) {
                    INTENSITY.LIGHT -> when (interactionState) {
                        UI_INTERACTION_STATE.HOVER -> {
                            getColor(TYPE.MAIN_COLOR_1, INTENSITY.LIGHT)
                        }
                        UI_INTERACTION_STATE.PRESSED -> {
                            getColor(TYPE.MAIN_COLOR_2, INTENSITY.LIGHT)
                        }
                        else -> Color.WHITE
                    }
                    INTENSITY.MEDIUM -> when (interactionState) {
                        UI_INTERACTION_STATE.HOVER -> {
                            getColor(TYPE.MAIN_COLOR_1, INTENSITY.MEDIUM)
                        }
                        UI_INTERACTION_STATE.PRESSED -> {
                            getColor(TYPE.MAIN_COLOR_2, INTENSITY.MEDIUM)
                        }
                        else -> Color.WHITE
                    }
                    INTENSITY.DARK -> when (interactionState) {
                        UI_INTERACTION_STATE.HOVER -> {
                            getColor(TYPE.MAIN_COLOR_1, INTENSITY.DARK)
                        }
                        UI_INTERACTION_STATE.PRESSED -> {
                            getColor(TYPE.MAIN_COLOR_2, INTENSITY.DARK)
                        }
                        else -> Color.WHITE
                    }
                }
            }
            TYPE.MAIN_COLOR_1 -> {
                when (intensity) {
                    INTENSITY.LIGHT -> when (interactionState) {
                        UI_INTERACTION_STATE.HOVER -> {
                            Color.WHITE
                        }
                        UI_INTERACTION_STATE.PRESSED -> {
                            Color.WHITE
                        }
                        else -> Color.WHITE
                    }
                    INTENSITY.MEDIUM -> when (interactionState) {
                        UI_INTERACTION_STATE.HOVER -> {
                            Color.WHITE
                        }
                        UI_INTERACTION_STATE.PRESSED -> {
                            Color.WHITE
                        }
                        else -> Color.WHITE
                    }
                    INTENSITY.DARK -> when (interactionState) {
                        UI_INTERACTION_STATE.HOVER -> {
                            Color.WHITE
                        }
                        UI_INTERACTION_STATE.PRESSED -> {
                            Color.WHITE
                        }
                        else -> Color.WHITE
                    }
                }
            }
            TYPE.MAIN_COLOR_2 -> {
                when (intensity) {
                    INTENSITY.LIGHT -> when (interactionState) {
                        UI_INTERACTION_STATE.HOVER -> {
                            Color.WHITE
                        }
                        UI_INTERACTION_STATE.PRESSED -> {
                            Color.WHITE
                        }
                        else -> Color.WHITE
                    }
                    INTENSITY.MEDIUM -> when (interactionState) {
                        UI_INTERACTION_STATE.HOVER -> {
                            Color.WHITE
                        }
                        UI_INTERACTION_STATE.PRESSED -> {
                            Color.WHITE
                        }
                        else -> Color.WHITE
                    }
                    INTENSITY.DARK -> when (interactionState) {
                        UI_INTERACTION_STATE.HOVER -> {
                            Color.WHITE
                        }
                        UI_INTERACTION_STATE.PRESSED -> {
                            Color.WHITE
                        }
                        else -> Color.WHITE
                    }
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

    override fun getFontSize(): Float {
        return 14f
    }

}