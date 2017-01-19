package net.milosvasic.pussycat.gui



class PussycatMainMenuBar(width: Int, height: Int, items: List<PussycatMenu>) : PussycatMenuBar(width, height) {

    init {
        val file = PussycatMenu("ZZZZ")
        val item1 = PussycatMenuItem("ssss")
        val item2 = PussycatMenuItem("mmmm")
        file.add(item1)
        file.add(item2)
        add(file)
        for (item in items) {
            add(item)
        }
    }

}