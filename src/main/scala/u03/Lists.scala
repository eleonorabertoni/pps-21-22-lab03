package u03
import u02.Optionals.*
import Option.*
import u02.AlgebraicDataTypes.*
import Person.*

import scala.annotation.tailrec
object Lists extends App:

  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()
  // a companion object (i.e., module) for List
  object List:

    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = l match
      case Cons(h, t) if n > 0 => drop(t, n-1)
      case _ => l

    def append[A](left: List[A], right: List[A]): List[A] = left match
      case Cons(h, t) => Cons(h, append(t, right))
      case Nil() => right

    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match
      case Cons(h, t) => append(f(h), flatMap(t)(f))
      case Nil() => Nil()

    def map1[A, B](l: List[A])(mapper: A => B): List[B] =
      flatMap(l)(e => Cons(mapper(e), Nil()))

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

    def filter1[A](l: List[A])(pred: A => Boolean): List[A] =
      flatMap(l)(e => pred(e) match
        case true => Cons(e, Nil())
        case _ => Nil()
      )

    def max(l: List[Int]): Option[Int] = l match
      case Cons(h, t) if h >= orElse(max(t), h) => Some(h)
      case Cons(h, t) => max(t)
      case _ => None()

    def findCoursesFromTeachers(l : List[Person]) : List[String] =
      flatMap(l)(x => x match
        case Teacher(name, course) => Cons(course, Nil())
        case _ => Nil())

    @tailrec
    def foldLeft[A, B](l : List[A])(acc : B)(f: (B, A) => B) : B = l match
      case Cons(h, t) => foldLeft(t)(f(acc, h))(f)
      case _ => acc

    def foldRight[A, B](l : List[A])(acc : B)(f: (A, B) => B) : B = l match
      case Cons(h,t) => f(h, foldRight(t)(acc)(f))
      case _ => acc

  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
