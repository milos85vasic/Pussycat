package net.milosvasic.pussycat.gui


import net.milosvasic.pussycat.application.ApplicationInformation
import net.milosvasic.pussycat.gui.content.Labels
import net.milosvasic.pussycat.gui.theme.Theme
import net.milosvasic.pussycat.gui.theme.UI_INTERACTION_TYPE
import net.milosvasic.pussycat.gui.theme.color.INTENSITY
import net.milosvasic.pussycat.gui.theme.color.TYPE
import java.awt.Color
import java.awt.event.MouseEvent
import java.awt.event.MouseListener
import javax.swing.JButton
import javax.swing.JComponent
import javax.swing.SwingConstants
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder


class PussycatAboutDialog(information: ApplicationInformation, theme: Theme) : PussycatAboutDialogAbstract(information, theme) {

    val closeButton = JButton()

    override fun generateFooter(): JComponent? {
        closeButton.isOpaque = true
        closeButton.background = Color.BLACK
        closeButton.foreground = theme.getTextColor(TYPE.BASE, INTENSITY.MEDIUM, UI_INTERACTION_TYPE.HOVER)
        closeButton.text = Labels.CLOSE
        closeButton.horizontalAlignment = SwingConstants.RIGHT
        closeButton.border = CompoundBorder(closeButton.border, EmptyBorder(5, 5, 5, 5))
        closeButton.addMouseListener(mouseListener)
        closeButton.isVisible = true
        closeButton.addActionListener {
            close()
        }
        return closeButton
    }

    val mouseListener = object : MouseListener {
        override fun mouseEntered(e: MouseEvent?) {
            closeButton.background = theme.getColor(TYPE.BASE, INTENSITY.MEDIUM)
            closeButton.foreground = theme.getTextColor(
                    TYPE.BASE,
                    INTENSITY.LIGHT,
                    UI_INTERACTION_TYPE.HOVER
            )
        }

        override fun mouseExited(e: MouseEvent?) {
            closeButton.background = Color.BLACK
            closeButton.foreground = theme.getTextColor(
                    TYPE.BASE,
                    INTENSITY.MEDIUM,
                    UI_INTERACTION_TYPE.HOVER
            )
        }

        override fun mouseClicked(e: MouseEvent?) {
            return
        }

        override fun mouseReleased(e: MouseEvent?) {
            return
        }

        override fun mousePressed(e: MouseEvent?) {
            return
        }
    }

}