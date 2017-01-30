package net.milosvasic.pussycat.tools

fun main(args: Array<String>) {
    val generator = NumberLinesLogcatGenerator()
    for (item in listOf(10, 50, 100, 150, 500, 1000, 3000, 5000)) {
        generator.generate(item)
    }
}