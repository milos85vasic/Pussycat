package net.milosvasic.pussycat.android.gui

import java.awt.Color
import java.awt.Graphics
import javax.swing.JPanel
import javax.imageio.ImageIO
import java.awt.image.BufferedImage


class PussycatSplashPanel(splashWidth: Int, splashHeight: Int) : JPanel() {

    private var image: BufferedImage? = null

    init {
        image = ImageIO.read(javaClass.classLoader.getResourceAsStream("splash/Pussycat.png"))
        background = Color.BLACK
        setSize(splashWidth, splashHeight)
    }

    override fun paintComponent(g: Graphics) {
        super.paintComponent(g)
        g.drawImage(image, 0, 0, this)
    }

}