package net.milosvasic.pussycat.actions

object Creator {

    interface Parametric<out R, in P> {
        fun create(params: P): R
    }

    interface Nonparametric<out R> {
        fun create(): R
    }

}