package visi

import core.Visi.RunnableInfo
import core.{ObjCCompiler, Visi, Compiler}
import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._

import net.liftweb.common.{Failure, Full}
import java.io.{FileOutputStream, File}

object Main {
  def main(argv: Array[String]) {
    val argl = argv.toList

    argl match {
      case "clean" :: _ =>
      case Nil =>
        val f = new File(".")
        val kids = f.listFiles().toList.filter(_.getName.toLowerCase().endsWith(".visi"))
        kids.foreach{f =>
          val fn = f.getName
          val name = fn.substring(0, fn.length - 5)
          val contents = new String(Helpers.readWholeFile(f), "UTF-8")

          Visi.compileWithRunnableInfo(contents) match {
            case Full((prog, RunnableInfo(funcs, types, deps, sources, sinks))) =>
              val hfile = ObjCCompiler.hFile(name, sources, sinks)
              val mfile = ObjCCompiler.mFile(name, prog, sources, sinks)


              val hf = new File(f.getParentFile, name+".h")
              val fos = new FileOutputStream(hf)
              fos.write(hfile.getBytes("UTF-8"))
              fos.close()

              val mf = new File(f.getParentFile, name+".m")
              val fosm = new FileOutputStream(mf)
              fosm.write(mfile.getBytes("UTF-8"))
              fosm.close()


            case Failure(msg, _, _) => System.err.println(msg)
            System.exit(1)

            case _ => System.err.println("Failed for no good reason")
            System.exit(1)
          }
        }
    }
  }
}