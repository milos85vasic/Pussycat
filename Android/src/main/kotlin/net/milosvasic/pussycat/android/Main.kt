package net.milosvasic.pussycat.android

import net.milosvasic.pussycat.application.APPLICATION_TYPE
import net.milosvasic.pussycat.android.application.Application

fun main(args: Array<String>) {
    val app = Application()
    val type = app.start(args)
    if(type == APPLICATION_TYPE.GUI){
        println("We are about to start GUI.")
    }
}



