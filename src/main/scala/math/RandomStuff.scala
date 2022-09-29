package main.scala.math

import main.scala.generic.TupleOP.{tupleMerge, unpack}

import scala.annotation.tailrec

import main.scala.math.Rational


object RandomStuff {


  def partialSummation[V](seq : LazyList[V])(implicit num: Numeric[V]) : LazyList[V] = {
    import num._
    lazy val res : LazyList[V]= seq.zip(zero #:: res).map(a => a._1 + a._2)
    res
  }

  def fromPartialSummation[V](seq : LazyList[V])(implicit num: Numeric[V]) : LazyList[V] = {
    import num._
    seq.zip(zero #:: seq).map(a => a._1 - a._2)
  }

  def runningAverage[V](seq : LazyList[V])(implicit frac: Fractional[V]) : LazyList[V] = {
    applyByIndex(divideByInt[V])(partialSummation(seq))
  }

  def fromRunningAverage[V](seq : LazyList[V])(implicit frac: Fractional[V]) : LazyList[V] = {
    fromPartialSummation(applyByIndex(multiplyByInt[V])(seq))
  }

  def convergify[V](seq : LazyList[V])(implicit frac: Fractional[V]) : LazyList[V] = {
    fromPartialSummation(runningAverage(partialSummation(seq)))
  }

  def unconvergify[V](seq : LazyList[V])(implicit frac: Fractional[V]) : LazyList[V] = {
    fromPartialSummation(fromRunningAverage(partialSummation(seq)))
  }

  def applyByIndex[V](fun : (Int,V) => V) : LazyList[V] => LazyList[V] = {
    seq : LazyList[V] => LazyList.from(1).zip(seq).map(unpack(fun))
  }

  def double_iterate[V](fst : V, snd : V) : ((V,V) => V) => LazyList[V] = fun => {
    lazy val res : LazyList[V] = fst #:: snd #:: (res).zip(res.drop(1)).map(unpack(fun));res
  }

  def iterate[V](start : Int, fst : V) : ((Int,V) => V) => LazyList[V] = fun => {
    lazy val res : LazyList[V] = fst #:: LazyList.from(start).zip(res).map(unpack(fun))
    res
  }

  def double_iterate[V](start : Int, fst : V, snd : V) : ((Int,V ,V) => V) => LazyList[V] = fun => {
    lazy val res : LazyList[V] = fst #:: snd #:: LazyList.from(start).zip(res).zip(res.drop(1)
        ).map(unpack(tupleMerge[Int,V,V])).map(unpack(fun))
    res
  }

  def divideByInt[V](n : Int, e :V)(implicit frac: Fractional[V]) : V = {import frac._; e / fromInt(n)}
  def multiplyByInt[V](n : Int, e :V)(implicit frac: Fractional[V]) : V = {import frac._; e * fromInt(n)}

  def main(argv: Array[String]): Unit = {
    val n = 18

    val f: LazyList[BigInt] = LazyList.from(1).map(x => x * x)
    val fibo = double_iterate(BigInt(0),BigInt(1))(_+_)
    val fact = iterate(1, BigInt(1))(_*_)



    def closerTo = (n : Int) => (q1 : Rational,q2 : Rational) => {
      val (min, max) : (Rational, Rational) = if (q1 * q1< new Rational(n))
        (q1, new Rational(n) / q1) else (new Rational(n) / q1 , q1);
        min < q2 && q2 < max;
    };


    val closestList : LazyList[Rational] = LazyList.iterate(new Rational(1))(prevBest => {
      val closerToTwo = closerTo(2);
      val bar = closerToTwo(prevBest, _);
      val (min, max) : (Rational, Rational) = if (prevBest * prevBest < new Rational(2))
        (prevBest, new Rational(2) / prevBest) else (new Rational(2) / prevBest , prevBest)
      @tailrec
      def FindNext(n : Int) : Rational = {
        val best = ((n / max.toDouble).ceil.toInt to (n / min.toDouble).floor.toInt).map(
          k => new Rational(n,k)).fold(new Rational(1))((a,b) => if (closerToTwo(b,a)) a else b)
        if (bar(best)) best else FindNext(n + 1)
      }
      FindNext(prevBest.numer + 1)
    })

    closestList.take(n).foreach(println)
    partialSummation(LazyList.from(1)).take(n).foreach(println)
    fromPartialSummation(partialSummation(LazyList.from(1))).take(n).foreach(println)
    fibo.take(n).foreach(println)
    //convergify(LazyList.from(1).map(1.0 / _)).take(100).foreach(println)
  }
}
