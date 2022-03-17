package u03

import org.junit.Test
import org.junit.Assert.*
import Lists.*
import List.*
import u02.Optionals.*
import Option.*
import u02.AlgebraicDataTypes.*

class MyListTest :
  val lst = Cons(10 , Cons(20 , Cons(30 , Nil ())))
  val tail = Cons(40 , Nil ())
  val listFold = Cons(3, Cons(7, Cons(1, Cons(5, Nil ()))))

  @Test def dropTest(): Unit =
    assertEquals(Cons(20 , Cons (30 , Nil ())), drop( lst , 1))
    assertEquals(Cons(30 , Nil ()), drop( lst , 2))
    assertEquals(Nil(), drop( lst , 5))

  @Test def appendTest(): Unit =
    assertEquals(Cons(10 , Cons(20 , Cons (30 , Cons (40 , Nil ())))), append(lst, tail))

  @Test def flatMapTest(): Unit =
    assertEquals(Cons (11 , Cons (21 , Cons (31 , Nil ()))), List.flatMap( lst )(v => Cons ( v + 1, Nil () )))
    assertEquals(Cons (11 , Cons (12 , Cons (21 , Cons (22 , Cons (31 , Cons (32 , Nil ())))))), List.flatMap ( lst )(v => Cons ( v + 1, Cons (v + 2, Nil () ))))

  @Test def testMap(): Unit =
    assertEquals ( Cons (11 , Cons (21 , Cons (31 , Nil () ))) , map1 (lst)( _ +1) )
    assertEquals ( Cons ("10", Cons ("20", Cons ("30", Nil () )) ) , map1 (lst )(_+""))

  @Test def testFilter(): Unit =
    assertEquals ( Cons (20 , Cons (30 , Nil () )) , filter1 (lst) (_ >=20) )
    assertEquals ( Cons (10 , Cons (30 , Nil () )), filter1 (lst) (_ != 20))

  @Test def testMax(): Unit =
    assertEquals(Some(25), max(Cons (10 , Cons (25 , Cons (20 , Nil ())))))
    assertEquals(None(), max(Nil()))

  @Test def testFindCoursesFromTeachers(): Unit =
    val s1 = Person.Student("eleonora", 2021)
    val s2 = Person.Student("elisa", 2021)
    val p1 = Person.Teacher("ghini", "Sistemi Operativi")
    val p2 = Person.Teacher("carbonaro", "Programmazione")
    val personList = Cons(p1, Cons(s1, Cons(s2, Cons(s2, Cons(p2, Nil())))))
    assertEquals(Cons("Sistemi Operativi", Cons("Programmazione", Nil())), findCoursesFromTeachers(personList))

  @Test def testFoldLeft(): Unit =
    def stringList = Cons("ciao", Cons("paguro", Cons("e", Nil())))
    assertEquals(-16, foldLeft(listFold)(0)(_ - _))
    assertEquals(16, foldLeft(listFold)(0)(_ + _))
    assertEquals(17, foldLeft(listFold)(1)(_ + _))
    assertEquals(11, foldLeft(stringList)(0)( (a: Int, s: String) => s.length + a))



  @Test def testFoldRight(): Unit =
    assertEquals(-8, foldRight(listFold)(0)(_ - _))
