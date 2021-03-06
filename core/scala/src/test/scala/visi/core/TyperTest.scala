package visi.core

import org.specs2.mutable.Specification
import net.liftweb.common.{Box, Full}
import visi.core.Visi.RunnableInfo

/**
 * Created with IntelliJ IDEA.
 * User: dpp
 * Date: 12/22/12
 * Time: 4:08 PM
 * To change this template use File | Settings | File Templates.
 */
class TyperTest extends Specification {
  "The Typer" should {
    "Correctly type choose" in {
      Visi.parseAndType(
        """
          |choose b x y = if b then x else y
          |awombat = choose true 1 2
          |aslug = choose false "dog" "cat"
          |""".stripMargin).flatMap(x => (x.functions.get("choose").map(_.tpe): Box[Type]) ?~ "Choose not found") match {
        case Full(TOper(Expression.FuncOperName, List(TPrim(PrimBool),
        TOper(Expression.FuncOperName, List(TVar(x), TOper(Expression.FuncOperName, List(TVar(y), TVar(z)))))))) if
        x == y && y == z => true must_== true
        case bad =>
          true must_!= true
      }
    }

    "Correctly type a more complex choose" in {
      Visi.parseAndType(
        """
          |choose b x y = if b then x else y
          |choose2 b x y = if b then x else y
          |n = choose2 true true true
          |x = choose true 1 2
          |y = choose false "hi" "dude"
          |""".stripMargin).flatMap(x => (x.functions.get("choose").map(_.tpe): Box[Type]) ?~ "Choose not found") match {
        case Full(TOper(Expression.FuncOperName, List(TPrim(PrimBool),
        TOper(Expression.FuncOperName, List(TVar(x), TOper(Expression.FuncOperName, List(TVar(y), TVar(z)))))))) if
        x == y && y == z => true must_== true
        case bad =>
          true must_!= true
      }
    }


    "Correctly type a simple function" in {
      Visi.parseAndType(
        """
          |add x y = x + y
          |""".stripMargin).flatMap(x => (x.functions.get("add").map(_.tpe): Box[Type]) ?~ "Add not found") match {
        case Full(TOper(Expression.FuncOperName, List(TPrim(PrimDouble),
        TOper(Expression.FuncOperName, List(TPrim(PrimDouble), TPrim(PrimDouble)))))) => true must_== true
        case bad =>
          true must_!= true
      }
    }

    "Mixed string and number must fail typer" in {
      Visi.parseAndType("f = 3 & \"hi\"").isEmpty must_== true
    }

    "Complex Mixed string and number must fail typer" in {
      Visi.parseAndType(
        """
          |f = 3
          |d = f & "hi"
        """.stripMargin).isEmpty must_== true
    }

    "Complex string must pass typer" in {
      Visi.parseAndType(
        """
          |f = "3"
          |d = f & "hi"
        """.stripMargin).isDefined must_== true
    }

    "String concat must pass typer" in {
      Visi.parseAndType("f = \"3\" & \"hi\"").isDefined must_== true
    }

    "Calculate the type of multiple functions" in {
      val toTest =
        Visi.parseAndType(
        """
          |f n = if true then n else (n + 1)
          |f2 n = if true then n else (n & "foo")
        """.stripMargin)


      List("f" -> testDoubleFunc _, "f2" -> testStrFunc _).map {
        case (name, func) =>
          (for {
          it <- toTest
          tpe <- it.functions.get(name).map(_.tpe)
        } yield func(tpe)) must_== Full(true)
      }

    }

    "Some choose stuff" in {
      val toTest =
      Visi.parseAndType(
        """
          |Define our inputs:
          |
          |```
          |?number
          |?n2
          |
          |?str
          |
          |?choice
          |
          |```
          |
          |
          |Business logic
          |```
          |choose b x y = if b then x else y
          |
          |a1 x = 1
          |
          |sum = sum + number
          |
          |theLen = len str
          |
          |cnt = cnt + (a1 number)
          |
          |```
          |
          |
          |And here are the results:
          |
          |```
          |"Mult" = number / n2
          |"Add" = number + n2
          |
          |"Funky" = choose choice (+) (*) number n2
          |
          |"Monkey" = choose choice number n2
          |
          |"Choicer" = choice
          |
          |"Sum" = sum
          |
          |"Cnt" = cnt
          |
          |"Ave" = sum / cnt
          |
          |"Str" = str
          |
          |```
          |
          |
          |
        """.stripMargin)

      toTest.map{
        case RunnableInfo(_, _, _, sources, sinks) =>
          sources.filter(_.name == "choice").map(_.tpe) must_== List(TPrim(PrimBool))
          sources.filter(_.name == "str").map(_.tpe) must_== List(TPrim(PrimStr))
          sinks.filter(_.name == "Monkey").map(_.tpe) must_== List(TPrim(PrimDouble))
          sinks.filter(_.name == "Ave").map(_.tpe) must_== List(TPrim(PrimDouble))
          sinks.filter(_.name == "Sum").map(_.tpe) must_== List(TPrim(PrimDouble))
          sinks.filter(_.name == "Cnt").map(_.tpe) must_== List(TPrim(PrimDouble))
          sinks.filter(_.name == "Choicer").map(_.tpe) must_== List(TPrim(PrimBool))
          sinks.filter(_.name == "Str").map(_.tpe) must_== List(TPrim(PrimStr))
        true
      } must_== Full(true)
    }

    "Works with inner variable" in {
      val toTest = Visi.parseAndType(
        """
          |// the function
          |f n =
          |   fct n = if n == 0 then 1 else n * fct (n - 1)
          |   qzz = n * 2
          |   r = n * 4
          |   app (qzz) fct
          |
          |qzz = 7
          |
          |fact n = if n == 0 then 1 else n * fact (n - 1)
          |
          |run x = f x
          |
          |app v f = f v
        """.stripMargin)

      List("run" -> testDoubleFunc _).map {
        case (name, func) =>
          (for {
            it <- toTest
            tpe <- it.functions.get(name).map(_.tpe)
          } yield func(tpe)) must_== Full(true)
      }
    }

    "Partial application of operators" in {
      val toTest = Visi.parseAndType(
        """
          |// the function
          |
          |times = (*)
          |
          |run x = times x 44
          |
          |app v f = f v
        """.stripMargin)

      List("run" -> testDoubleFunc _).map {
        case (name, func) =>
          (for {
            it <- toTest
            tpe <- it.functions.get(name).map(_.tpe)
          } yield func(tpe)) must_== Full(true)
      }
    }

    "Partial application of operators #2" in {
      val toTest = Visi.parseAndType(
        """
          |// the function
          |
          |times = (* 44)
          |
          |run x = times x
          |
          |app v f = f v
        """.stripMargin)

      List("run" -> testDoubleFunc _).map {
        case (name, func) =>
          (for {
            it <- toTest
            tpe <- it.functions.get(name).map(_.tpe)
          } yield func(tpe)) must_== Full(true)
      }
    }

    "Partial application of operators #3" in {
      val toTest = Visi.parseAndType(
        """
          |// the function
          |
          |times = (44 *)
          |
          |run x = times x
          |
          |app v f = f v
        """.stripMargin)

      List("run" -> testDoubleFunc _).map {
        case (name, func) =>
          (for {
            it <- toTest
            tpe <- it.functions.get(name).map(_.tpe)
          } yield func(tpe)) must_== Full(true)
      }
    }

    "Factorial call" in {
      val toTest =
        Visi.parseAndType(
          """
            |fact n = if n == 0 then 1 else n * fact (n - 1)
            |res = fact 10
          """.stripMargin)


      List("fact" -> testDoubleFunc _, "res" -> testIsDouble _).map {
        case (name, func) =>
          (for {
            it <- toTest
            tpe <- it.functions.get(name).map(_.tpe)
          } yield func(tpe)) must_== Full(true)
      }
    }

    "Good or bad test" in {
      val toTest =
        Visi.parseAndType(
          """
            |fact n = if n == 0 then 1 else n * fact (n - 1)
            |res = fact 10
            |good = goodorbad true
            |bad = goodorbad false
            |goodorbad v = if v then "good" else "bad"
          """.stripMargin)

      List("fact" -> testDoubleFunc _, "good" -> testIsString _).map {
        case (name, func) =>
          (for {
            it <- toTest
            tpe <- it.functions.get(name).map(_.tpe)
          } yield func(tpe)) must_== Full(true)
      }
    }

    "Identity function" in {
      val toTest =
        Visi.parseAndType(
          """
            |fact n = if n == 0 then 1 else n * fact (n - 1)
            |res = fact 10
            |good = goodorbad true
            |bad = goodorbad false
            |q n = n
            |goodorbad v = if v then "good" else "bad"
          """.stripMargin)

      List("fact" -> testDoubleFunc _, "good" -> testIsString _,
      "q" -> testGenericFunc _).map {
        case (name, func) =>
          (for {
            it <- toTest
            tpe <- it.functions.get(name).map(_.tpe)
          } yield func(tpe)) must_== Full(true)
      }
    }


  }

  private def testGenericFunc(t: Type): Boolean = t match {
    case TOper(Expression.FuncOperName, List(TVar(a), TVar(b))) => a == b
    case _ => false
  }
  private def testIsString(t: Type): Boolean = t == TPrim(PrimStr)
  private def testIsDouble(t: Type): Boolean = t == TPrim(PrimDouble)
  private def testDoubleFunc(t: Type): Boolean = {
    val ret = t == Expression.tFun(TPrim(PrimDouble), TPrim(PrimDouble))
    if (!ret) {println("test "+t)}
    ret
  }
  private def testStrFunc(t: Type): Boolean =
    t == Expression.tFun(TPrim(PrimStr), TPrim(PrimStr))

}

class DependencyTest extends Specification {
  "Simple dependency" in {
    val toTest =
      Visi.parseAndType(
        """
          |a x = (d x) * 4
          |b x = a x
          |c x = b x
          |d x = c x
          |z x = d x
          |q foo =
          |  n = 5
          |  z foo
          |""".stripMargin)

    toTest.map{
      case Visi.RunnableInfo(a, b, c, _, _) =>
      val pred = Typer.findAllTopLevelPredicates(c)
      val rec = Typer.findRecursive(pred)
      rec.length
    } must_== Full(4)
  }

  "Recursive" in {
    val toTest =
      Visi.parseAndType(
        """
          |fact n = if n == 0 then 1 else n * fact (n - 1)
          |
          |dog n =
          |  cat q = q * n
          |  cat 44
          |
          |res = fact 10
          |good = goodorbad true
          |bad = goodorbad false
          |q n = n
          |goodorbad v = if v then "good" else "bad"
          |""".stripMargin)

    toTest.map{
      case Visi.RunnableInfo(a, b, c, _, _) =>
        val pred = Typer.findAllTopLevelPredicates(c)
        val rec = Typer.findRecursive(pred)
        rec.length
    } must_== Full(1)
  }


  "Recursive statement" in {
    val toTest =
      Visi.parseAndType(
        """
          |fact n = if n == 0 then 1 else n * fact (n - 1)
          |res = fact 10
          |good = goodorbad true
          |bad = goodorbad false
          |q n = n
          |goodorbad v = if v then "good" else "bad"
          |?source
          |recMe = recMe + source
          |""".stripMargin)

    toTest.map{
      case Visi.RunnableInfo(a, b, c, _, _) =>
        val pred = Typer.findAllTopLevelPredicates(c)
        val rec = Typer.findRecursive(pred)
        rec.length
    } must_== Full(2)
  }


  "Tracing sources and sinks" in {
    val toTest =
      Visi.parseAndType(
        """
          |?in1
          |
          |?in2
          |
          |thing = in1 + (if in2 then 1 else 0)
          |
          |spark = spark + in1
          |
          |"frog" = thing + spark
          |
          |"dog" = in1 * 5
          |
          |"moose" = in2
          |
          |""".stripMargin)

    val res =
      toTest.map{
      case Visi.RunnableInfo(a, b, c, _, _) =>
        val pred = Typer.findAllTopLevelPredicates(c)
        val deps = Typer.whatDependsOnSource(c, pred)

      deps
    }

    res.map(_.length) must_== Full(2)
    val lst = res.toList.flatMap{
      _.map{
      case (s, (snk, le)) => (s.name, (snk.map(_.name).sorted, le.map(_.name).sorted))
      }
    }

    val in1 = lst.filter(_._1 == "in1")
    val in2 = lst.filter(_._1 == "in2")

    in1 must_== List(("in1", (List("dog", "frog"), List("spark", "thing"))))
    in2 must_== List(("in2", (List("frog", "moose"), List("thing"))))
  }

}



/*

      ,("test_01_simple_assignment.md", testTypes [("a", testPrimDouble)] . checktype)
      ,("test_04_multi_literal.md", testTypes [("res", testPrimDouble)
                                  ,("f", testDoubleFunc)] . checktype)
      ,("test_04_multi_literal.md", testTypes [("res", testPrimDouble)
                                     ,("f2", testPrimStr)] . checktype)

      ,("test_04_multi_literal.md", testTypes [("f3", testStrFunc)
                                              ,("f4", testStrFunc)
                                              ,("id", testGenFunc)
                                              ,("cond", testGenFunc)] . checktype)

      ,("test_04_multi_literal.md", testTypes [("numberFunc", testDoubleFunc)
                                              ,("selfRefNumber", testDoubleFunc)] . checktype)

     ,("f n = n & \"hi\"\n\
        \q n = n", testTypes [("f", testStrFunc)
                             ,("q", testGenFunc)] . checktype)

     ,("a n = b n\n\
       \b n = c n\n\
       \c n = a n", testTypes [("a", testGenXFunc)
                              ,("b", testGenXFunc)
                              ,("c", testGenXFunc)] . checktype)

     ,("plus41 n = n + 41\n\
       \p41 = plus41\n\
       \f = p41 1\n", testTypes [("f", testPrimDouble)] . checktype)

     ,("a n = n == 1", testTypes [("a", testT $ tFun (TPrim PrimDouble) (TPrim PrimBool))] . checktype)

     ,("a n = n == 1\n\
       \b = a true", failsTyper . checktype)


     ,("a n = \"foo\" == 1", failsTyper . checktype)


     ,("a n = b n\n\
       \b n = c n\n\
       \c n = a n\n\
       \d n = a n", testTypes [("a", testGenXFunc)
                              ,("b", testGenXFunc)
                              ,("c", testGenXFunc)
                              ,("d", testGenXFunc)] . checktype)

     ,("a n f = if f n then n else b n f\n\
       \b n f = c n f\n\
       \c n f = if f n then n else a n f\n", testTypes [("a", testGenBoolFunc)] . checktype)

     ,("

     ,("test_04_multi_literal.md",
                  testResults [("oddRes", BoolValue True)] . checkResults)

     ,("res = \"10\"",
                  testResults [("res", StrValue $ T.pack "10")] . checkResults)


     ,("res = fact 10\n\
       \fact n = if n == 0 then 1 else n * fact n",
                  testResults [("res", UndefinedValue)] . checkResults)


     ,("res n = n.age", psuccess 1 . checkparse)

     ,("res n = n.fizzbin\n\
       \frog = (res 44) + 1", testTypes [("frog", testPrimDouble)] . checktype)

     ,("res n = n.fizzbin\n\
       \frog = (res \"foo\") & \"1\"", testTypes [("frog", testPrimStr)] . checktype)

     ,("res n = \n\
       \  x = n.fizzbin\n\
       \  y = n.meowfizz\n\
       \  x + y\n\
       \frog = (res 44) + 1", testTypes [("frog", testPrimDouble)] . checktype)

     ,("woof n =\n\
       \  x = n.fizzbin\n\
       \  show x\n\
       \res n = \n\
       \  x = n.fizzbin\n\
       \  y = n.meowfizz\n\
       \  z = woof n\n\
       \  x + y + (len z)\n\
       \frog = (res 44) + 1", testTypes [("frog", testPrimDouble)] . checktype)

     ,("woof n =\n\
       \  x = n.fizzbin\n\
       \  show x\n\
       \woof2 n =\n\
       \  x = n.fizzbin\n\
       \  x\n\
       \res n = \n\
       \  x = n.fizzbin\n\
       \  y = n.meowfizz\n\
       \  z = woof n\n\
       \  x + y + (len z)\n\
       \frog = (res 44) + 1\n\
       \dog = (woof2 \"44\") & (show (woof2 44))", testTypes [("frog", testPrimDouble), ("dog", testPrimStr)] . checktype)

     ,("woof n =\n\
       \  x = n.fizzbin\n\
       \  show x\n\
       \res n = \n\
       \  x = n.fizzbin\n\
       \  y = n.meowfizz\n\
       \  z = woof n\n\
       \  x + y + (len z)\n\
       \frog = (res \"44\") + 1", failsTyper . checktype)

     ,("res n = n.fizzbin\n\
       \dog = (res 44) + 1\n\
       \frog = (res \"foo\") & \"1\"", testTypes [("frog", testPrimStr), ("dog", testPrimDouble)] . checktype)

     ,("res n = n.fizzbin\n\
       \dog = (res 44) + 1\n\
       \frog = (res \"foo\") + 1", failsTyper . checktype)
 */
