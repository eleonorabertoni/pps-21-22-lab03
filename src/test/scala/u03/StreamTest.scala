package u03
import org.junit.Test
import org.junit.Assert.*
import Streams.*

class StreamTest :

  @Test def dropTest(): Unit =
    val s = Stream.take(Stream.iterate(0)(_ + 1))(10)
    assertEquals(Stream.toList(Stream.filter(s)(x => x >= 6)), Stream.toList(Stream.drop(s)(6)))

  @Test def constantTest(): Unit =
    val s = Stream.toList(Stream.cons("x", Stream.cons("x", Stream.cons("x", Stream.cons("x", Stream.cons("x", Stream.empty() ))))))
    assertEquals(s, Stream.toList(Stream.take(Stream.constant("x"))(5)))

  @Test def fibTest(): Unit =
    val fibs:Stream[Int] = Stream.fib()
    assertEquals(Stream.toList(Stream.cons(0, Stream.cons(1, Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.cons(5, Stream.cons(8, Stream.cons(13, Stream.empty()))))))))),Stream.toList ( Stream.take(fibs)(8)))








