package visi.core

import org.specs2.mutable.Specification
import net.liftweb.common.{Box, Full}

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
          |""".stripMargin).flatMap(x => (x.types.get(FuncName("choose")): Box[Type]) ?~ "Choose not found") match {
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
          |""".stripMargin).flatMap(x => (x.types.get(FuncName("choose")): Box[Type]) ?~ "Choose not found") match {
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
          |""".stripMargin).flatMap(x => (x.types.get(FuncName("add")): Box[Type]) ?~ "Add not found") match {
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
          tpe <- it.types.get(FuncName(name))
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
            tpe <- it.types.get(FuncName(name))
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
            tpe <- it.types.get(FuncName(name))
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
            tpe <- it.types.get(FuncName(name))
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
  private def testDoubleFunc(t: Type): Boolean =
    t == Expression.tFun(TPrim(PrimDouble), TPrim(PrimDouble))
  private def testStrFunc(t: Type): Boolean =
    t == Expression.tFun(TPrim(PrimStr), TPrim(PrimStr))

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
