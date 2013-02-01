package visi

import core.{Visi, Compiler}
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._

import net.liftweb.common.{Failure, Full}

object Main {
  def main(argv: Array[String]) {
    val str = new String(Helpers.readWholeStream(System.in), "UTF-8")
    Visi.compile(str) match {
      case Full(prog) =>
        val strList: List[String] = (Compiler.jsLibrary + "\n\n" + prog+"\n\n").split("\n").toList
        System.out.print("@")
        strList.foreach(s => {
          System.out.print("\"")
          System.out.print(fixStr(s))
          System.out.println("\\n\"")

        })
        System.out.println(";")

      case Failure(msg, _, _) => System.err.println(msg)
      case _ => System.err.println("Failed for no good reason")
    }
  }

  private def fixStr(in: String): String = {
    val ret = new StringBuffer(in.length + 20)

    val len = in.length
    var pos = 0
    while (pos < len) {
      in.charAt(pos) match {
        case '"' => ret.append("\\\"")
        case '\\' => ret.append("\\\\")
        case c => ret.append(c)
      }
      pos += 1
    }

    ret.toString
  }
}