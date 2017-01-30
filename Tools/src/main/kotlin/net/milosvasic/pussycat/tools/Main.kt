package net.milosvasic.pussycat.tools

import net.milosvasic.pussycat.tools.android.NumberLinesLogcatGenerator

fun main(args: Array<String>) {
    if (args.size < 4) {
        println(Messages.INVALID_ARGUMENTS)
        return
    }
    when (args[0]) {
        "--generate" -> {
            when (args[1]) {
                "--logs" -> {
                    when (args[2]) {
                        "--android" -> {
                            val list = mutableListOf<Int>()
                            val generator = NumberLinesLogcatGenerator()
                            for (x in 3..args.size - 1) {
                                val value: Int = args[x].toInt()
                                if (value is Int) {
                                    list.add(value)
                                } else {
                                    println(Messages.INVALID_ARGUMENTS)
                                }
                            }
                            for (item in list) {
                                generator.generate(item)
                            }
                        }
                        else -> {
                            println(Messages.INVALID_ARGUMENTS)
                        }
                    }
                }
                else -> {
                    println(Messages.INVALID_ARGUMENTS)
                }
            }
        }
        else -> {
            println(Messages.INVALID_ARGUMENTS)
        }
    }

}