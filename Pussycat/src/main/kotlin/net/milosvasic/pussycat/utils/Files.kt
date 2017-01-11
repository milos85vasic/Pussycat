package net.milosvasic.pussycat.utils

import com.sun.org.apache.xalan.internal.utils.SecuritySupport
import java.io.BufferedReader
import java.io.InputStream
import java.io.InputStreamReader
import java.util.*


object Files {

    fun getResourceFiles(path: String): List<String> {
        val filenames = ArrayList<String>()
        getResourceAsStream(path).use({ input ->
            BufferedReader(InputStreamReader(input)).use { br ->
                var resource: String
                resource = br.readLine()
                while (!Text.isEmpty(resource)) {
                    filenames.add(resource)
                    try {
                        resource = br.readLine()
                    } catch (e: Exception) {
                        resource = ""
                    }
                }
            }
        })
        return filenames
    }

    fun getResourceAsStream(resource: String): InputStream {
        val input = SecuritySupport.getContextClassLoader().getResourceAsStream(resource)
        return input ?: javaClass.getResourceAsStream(resource)
    }

}