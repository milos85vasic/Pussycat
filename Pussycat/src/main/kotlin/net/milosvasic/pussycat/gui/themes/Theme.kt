package net.milosvasic.pussycat.gui.themes

import net.milosvasic.pussycat.gui.*
import net.milosvasic.pussycat.gui.themes.color.INTENSITY
import net.milosvasic.pussycat.gui.themes.color.TYPE
import java.awt.Color
import java.awt.Component
import javax.swing.JComponent
import javax.swing.JFrame
import javax.swing.JPanel
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder

abstract class Theme {

    abstract fun getColor(type: TYPE, intensity: INTENSITY): Color

    abstract fun getTextColor(type: TYPE, intensity: INTENSITY): Color

    fun apply(comp: Component?){
        if (comp is JComponent) {
            comp.isOpaque = true
        }
        when (comp) {
            is PussycatBar -> {
                comp.background = getColor(TYPE.BASE, INTENSITY.MEDIUM)
                comp.border = CompoundBorder(comp.border, EmptyBorder(0, 0, 0, 0))
            }
            is PussycatMenu -> {
                comp.background = getColor(TYPE.BASE, INTENSITY.MEDIUM)
                comp.foreground = getTextColor(TYPE.BASE, INTENSITY.MEDIUM)
                comp.border = CompoundBorder(comp.border, EmptyBorder(0, 0, 0, 0))
            }
            is PussycatMenuItem -> {
                comp.background = getColor(TYPE.BASE, INTENSITY.MEDIUM)
                comp.foreground = getTextColor(TYPE.BASE, INTENSITY.MEDIUM)
                comp.border = CompoundBorder(comp.border, EmptyBorder(0, 0, 0, 0))
            }
            is PussycatContent -> {
                comp.background = getColor(TYPE.BASE, INTENSITY.DARK)
                comp.border = CompoundBorder(comp.border, EmptyBorder(5, 5, 5, 5))
            }
            is PussycatWindow -> {
                comp.background = getColor(TYPE.BASE, INTENSITY.LIGHT)
            }
        }
    }

}