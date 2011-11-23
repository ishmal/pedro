

/**
 * Lenses are required to satisfy the following two laws and to be side-effect free.
 *
 * <p>
 * All instances must satisfy 2 laws:
 * <ol>
 * <li><strong>identity</strong><br/><code>forall a. lens.set(a,lens(a)) = a</core></li>
 * <li><strong>retention</strong><br/><code>forall a b. lens(lens.set(a,b)) = b</core></li>
 * </ol>
 * </p>
 */

case class Lens[A,B](get: A => B, set: (A,B) => A) extends Immutable 
{
  /** Simply call get() */
  def apply(whole:A): B = get(whole)

  /** Modify the value viewed through the lens */
  def mod(a:A, f: B => B) : A = set(a, f(get(a)))

  /** Lenses can be composed */
  def compose[C](that: Lens[C,A]) = Lens[C,B](
    c => get(that.get(c)),
    (c, b) => that.mod(c, set(_, b))
  )
  def andThen[C](that: Lens[B,C]) = that compose this

  /** You can apply an isomorphism to the value viewed through the lens to obtain a new lens. */
  def xmap[C](f: B => C)(g: C => B) = Lens[A,C](
    a => f(get(a)),
    (a,c) => set(a,g(c))
  )

  /** Two lenses that view a value of the same type can be joined */
  def |||[C](that: Lens[C,B]) = Lens[Either[A,C],B](
    { case Left(a) => get(a)
      case Right(b) => that.get(b)
    },
    { case (Left(a),  b) => Left (set(a,b))
      case (Right(c), b) => Right(that.set(c,b))
    }
  )

  /** Two disjoint lenses can be paired */
  def ***[C,D](that: Lens[C,D]) = Lens[(A,C),(B,D)](
    ac => (get(ac._1), that.get(ac._2)),
    (ac,bd) => (set(ac._1,bd._1),that.set(ac._2,bd._2))
  )

}
