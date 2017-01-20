package net.milosvasic.pussycat.gui


import net.milosvasic.pussycat.application.ApplicationInformation
import net.milosvasic.pussycat.gui.content.Labels
import net.milosvasic.pussycat.gui.theme.Theme
import java.awt.Color
import java.awt.Frame
import javax.swing.JButton
import javax.swing.JComponent
import javax.swing.SwingConstants
import javax.swing.border.CompoundBorder
import javax.swing.border.EmptyBorder


class PussycatAboutDialog(information: ApplicationInformation, theme: Theme, owner: Frame?) : PussycatAboutDialogAbstract(information, theme, owner) {

    override fun generateFooter(): JComponent? {
        val closeButton = JButton()
        closeButton.isOpaque = true
        closeButton.background = Color.BLACK
        closeButton.foreground = Color.RED
        closeButton.text = Labels.CLOSE
        closeButton.horizontalAlignment = SwingConstants.RIGHT
        closeButton.border = CompoundBorder(closeButton.border, EmptyBorder(5, 5, 5, 5))
        closeButton.isVisible = true
        closeButton.addActionListener {
            close()
        }
        return closeButton
    }

}