package net.milosvasic.pussycat.gui

import net.milosvasic.pussycat.application.ApplicationInformation
import java.awt.Frame
import javax.swing.JButton
import javax.swing.JComponent


class PussycatAboutDialog(information: ApplicationInformation, owner: Frame?) : PussycatAboutDialogAbstract(information, owner) {

    override fun generateFooter(): JComponent {
        val button = JButton()
        button.text = Labels.CLOSE
        button.addActionListener {
            close()
        }
        return button
    }

}