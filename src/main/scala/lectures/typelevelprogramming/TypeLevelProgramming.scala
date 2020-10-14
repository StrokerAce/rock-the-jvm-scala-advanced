package lectures.typelevelprogramming

object TypeLevelProgramming extends App {

  import scala.reflect.runtime.universe._

  def show[T](value: T)(implicit tag: TypeTag[T]) =
    tag.toString.replace("lectures.typelevelprogramming.TypeLevelProgramming.", "")

  // type-level programming
  // Peano arithmetic
  trait Nat

  class _0 extends Nat

  class Succ[N <: Nat] extends Nat

  type _1 = Succ[_0]
  type _2 = Succ[_1] // Succ[Succ[_0]] etc.
  type _3 = Succ[_2]
  type _4 = Succ[_3]
  type _5 = Succ[_4]

  // _2 < _4 as type relationship?
  trait <[A <: Nat, B <: Nat]

  object < {
    implicit def ltBasic[B <: Nat]: <[_0, Succ[B]] = new <[_0, Succ[B]] {}

    implicit def inductive[A <: Nat, B <: Nat](implicit lt: <[A, B]): <[Succ[A], Succ[B]] = new <[Succ[A], Succ[B]] {}

    // Return whatever implicit compiler can find.
    def apply[A <: Nat, B <: Nat](implicit lt: <[A, B]) = lt
  }

  // Works because compiler finds ltbasic
  val comparision: _0 < _1 = <[_0, _1]

  val comparison2: _1 < _3 = <[_1, _3]

  /*
  1. <.apply[_1, _3] -> requires implicit <[_1, _3]
  2. inductive[_1, _3] -> requires implicit <[_0, _2]
  3. ltBasic[_1]] -> produces implicit <[_0, Succ[_1]] == <[_0, _2]
   */

  // badComparison: _3 < _2  = <[_3, _2] - will not compile

  trait <=[A <: Nat, B <: Nat]

  object <= {
    implicit def lteBasic[B <: Nat]: <=[_0, B] = new <=[_0, B] {}

    implicit def inductive[A <: Nat, B <: Nat](implicit lte: <=[A, B]): <=[Succ[A], Succ[B]] = new <=[Succ[A], Succ[B]] {}

    // Return whatever implicit compiler can find.
    def apply[A <: Nat, B <: Nat](implicit lte: <=[A, B]) = lte
  }

  val lteTest: _1 <= _1 = <=[_1, _1]

  // val invalidLte: _2 <= _1 = <=[_2, _1] // Will not compile.

  //  // ADD NUMBERS as types
  //  trait +[A <: Nat, B <: Nat, S <: Nat]
  //
  //  object + {
  //
  //    // 0 + 0 = 0
  //    implicit val zero: +[_0, _0, _0] = new +[_0, _0, _0] {}
  //
  //    // For every A <: Nat such that A > 0 we have 0 + A= A and A + 0 = A
  //    implicit def basicRight[A <: Nat](implicit lt: _0 < A): +[_0, A, A] = new +[_0, A, A]{}
  //    implicit def basicLeft[A <: Nat](implicit lt: _0 < A): +[A, _0, A] = new +[A, _0, A]{}
  //
  //    // If A + B = S then Succ[A] + Succ[B] = Succ[Succ[S]]
  //    implicit def inductive[A <: Nat, B <: Nat, S<: Nat](implicit plus: +[A, B, S]): +[Succ[A], Succ[B], Succ[Succ[S]]] =
  //      new +[Succ[A], Succ[B], Succ[Succ[S]]]{}
  //
  //
  //    def apply[A <: Nat, B <: Nat, S <: Nat](implicit plus: +[A, B, S]): +[A, B, S] = plus
  //  }
  //
  //  val zero: +[_0, _0, _0] = +[_0, _0, _0]
  //  val two: +[_0, _2, _2] = +[_0, _2, _2]
  //
  //  val four: +[_1, _3, _4] = +[_1, _3, _4]
  //  /*
  //  1. I need an implicit +[1, 3, 4] = +[Succ[0], Succ[2], Succ[Succ[2]]]
  //  2. Can run inductive nut I need an implicit +[0, 0, 2]
  //  3. Can run basicRight and construct +[0, 2, 2]
  //   */

  // Now let's make compiler figure out solution
  // ADD NUMBERS as types
  trait +[A <: Nat, B <: Nat] {
    type Result <: Nat
  }

  object + {

    type Plus[A <: Nat, B <: Nat, S <: Nat] = +[A, B] {type Result = S}

    // 0 + 0 = 0
    implicit val zero: Plus[_0, _0, _0] = new +[_0, _0] {
      type Result = _0
    }

    // For every A <: Nat such that A > 0 we have 0 + A= A and A + 0 = A
    implicit def basicRight[A <: Nat](implicit lt: _0 < A): Plus[_0, A, A] = new +[_0, A] {
      type Result = A
    }

    implicit def basicLeft[A <: Nat](implicit lt: _0 < A): Plus[A, _0, A] = new +[A, _0] {
      type Result = A
    }

    // If A + B = S then Succ[A] + Succ[B] = Succ[Succ[S]]
    implicit def inductive[A <: Nat, B <: Nat, S <: Nat](implicit plus: Plus[A, B, S]): Plus[Succ[A], Succ[B], Succ[Succ[S]]] =
      new +[Succ[A], Succ[B]] {
        type Result = Succ[Succ[S]]
      }

    def apply[A <: Nat, B <: Nat](implicit plus: +[A, B]): Plus[A, B, plus.Result] = plus
  }

  val zero: +[_0, _0] = +
  [_0
  , _0
  ]
  val two: +[_0, _2] = +
  [_0
  , _2
  ]

  val four: +[_1, _3] = +
  [_1
  , _3
  ]

  /*
  1. I need an implicit +[1, 3, 4] = +[Succ[0], Succ[2], Succ[Succ[2]]]
  2. Can run inductive nut I need an implicit +[0, 0, 2]
  3. Can run basicRight and construct +[0, 2, 2]
   */

  trait HList

  class HNil extends HList

  class ::[H <: Nat, T <: HList] extends HList

  /*
  1. Split List in half
  2. Sort the half
  3. Merge in sorted order
   */

  trait Split[HL <: HList, L <: HList, R <: HList]

  object Split {
    implicit val basic: Split[HNil, HNil, HNil] = new Split[HNil, HNil, HNil] {}

    implicit def basic2[N <: Nat]: Split[N :: HNil, N :: HNil, HNil] = new Split[N :: HNil, N :: HNil, HNil] {}

    implicit def inductive[N1 <: Nat, N2 <: Nat, T <: HList, L <: HList, R <: HList](implicit split: Split[T, L, R]): Split[N1 :: N2 :: T, N1 :: L, N2 :: R] =
      new Split[N1 :: N2 :: T, N1 :: L, N2 :: R] {}

    def apply[HL <: HList, L <: HList, R <: HList](implicit split: Split[HL, L, R]) = split
  }

  val validSplit: Split[_1 :: _2 :: _3 :: HNil, _1 :: _3 :: HNil, _2 :: HNil] = Split.apply

  /*
  1. Require implicit Split[_1 :: _2 :: _3 :: HNil, _1 :: _3 :: HNil, _2 :: HNil]
  2. inductive[_1, _2, _3 :: HNil, _3 :: HNil, HNil]
  3  Require implict Split[_3 :: HNil, _3 :: HNil, HNil]
  4. basic2[_3] => Split[_3 :: HNil, _3 :: HNil, HNil]
   */

  trait Merge[LA <: HList, LB <: HList, L <: HList]

  object Merge {
    implicit def basicLeft[L <: HList]: Merge[HNil, L, L] = new Merge[HNil, L, L] {}

    implicit def basicRight[L <: HList]: Merge[L, HNil, L] = new Merge[L, HNil, L] {}

    /*
    L1 = N1 :: T1
    L2 = N2 :: T2
    if N1 < N2 => N1 :: {...}
    if N2 <= N1 => N2 :: {...}
     */
    implicit def inductiveLTE[N1 <: Nat, T1 <: HList, N2 <: Nat, T2 <: HList, IR <: HList]
      (implicit merge: Merge[T1, N2 :: T2, IR], lte: <=[N1, N2]): Merge[N1 :: T1, N2 :: T2, N1 :: IR] =
      new Merge[N1 :: T1, N2 :: T2, N1 :: IR] {}

    implicit def inductiveGT[N1 <: Nat, T1 <: HList, N2 <: Nat, T2 <: HList, IR <: HList]
    (implicit merge: Merge[N1 :: T1, T2, IR], lt: <=[N2, N1]): Merge[N1 :: T1, N2 :: T2, N2 :: IR] =
      new Merge[N1 :: T1, N2 :: T2, N2 :: IR] {}

    def apply[LA <: HList, LB <: HList, L <: HList](implicit merge: Merge[LA, LB, L]): Merge[LA, LB, L] = merge
  }

  val validMerge: Merge[_1 :: _3 :: HNil, _2 :: _4 :: HNil, _1 :: _2 :: _3 :: _4 :: HNil] = Merge.apply
  /*
  1. Require implicit Merge[_1 :: _3 :: HNil, _2 :: _4 :: HNil, _1 :: _2 :: _3 :: _4 :: HNil]
  2. Run inductiveLTE, requires implicit Merge[_3 :: HNil, _2 :: _4 :: HNil, _2 :: _3 :: _4 :: HNil]
  3. Run inductiveGT, requires implicit Merge[_3 :: HNil, _4 :: HNil, _3 :: _4 :: HNil]
  4. Run inductiveLTE, requires implicit Merge[HNil, _4 :: HNil, _4 :: HNil]
  5. Run basicLeft[_4 :: HNil, HNil]
   */

  trait Sort[L <: HList] {type Result <: HList}

  object Sort {

    type SortOp[L <: HList, O <: HList]  = Sort[L] {type Result = O}

    implicit val basicNil: SortOp[HNil, HNil] = new Sort[HNil] {type Result = HNil}

    implicit def basicOne[N <: Nat]: SortOp[N :: HNil, N :: HNil] = new Sort[N :: HNil] {type Result = N :: HNil}

    implicit def inductive[I <: HList, L <: HList, R <: HList, SL <: HList, SR <: HList, O <: HList]
    (implicit split: Split[I, L, R], sortLeft: SortOp[L, SL], sortRight: SortOp[R, SR], merge: Merge[SL, SR, O]) : SortOp[I, O] = new Sort[I] {type Result = O}

    def apply[L <: HList](implicit sort: Sort[L]): SortOp[L, sort.Result] = sort
  }

  val validSort: Sort[_4 :: _3 :: _5 :: _1 :: _2 :: HNil] = Sort.apply

  println(show(Sort[_4 :: _3 :: _5 :: _1 :: _2 :: HNil]))

  println(show(+.apply[_1, _3]))
  println(show(four)) // Different result because type attached after it is computed
}
