import java.util.regex.Pattern
import scala.util.Random

object RegexDfaNfaCheck {

  private val RE1: Pattern = Pattern.compile(
    """^
        (?:                             
            (?:aa*bb*ab|c*)*            
            (?:ba(?:                    
                    b                   
                    |(?:c|ac)ac         
                    |cc(?:bb|aba)       
                )*                      
            )*                          
        )
    $""",
    Pattern.COMMENTS
  )

  private val RE4: Pattern = Pattern.compile(
    """^
        (?:                             
            (?:a+b+ab|c)*               
            (?:ba(?:                    
                    b                   
                    |a?cac              
                    |cc(?:bb|aba)       
                )*                      
            )*                          
        )
    $""",
    Pattern.COMMENTS
  )

  def regex1Accepts(word: String): Boolean =
    RE1.matcher(word).matches()

  def regex4Accepts(word: String): Boolean =
    RE4.matcher(word).matches()

  
  val DFA_START: String = "q0"
  val DFA_FINAL: Set[String] = Set("q0", "q4", "q7")

  val DFA_TRANS: Map[String, Map[Char, String]] = Map(
    "q0"  -> Map('a' -> "q1",  'b' -> "q2",  'c' -> "q0"),
    "q1"  -> Map('a' -> "q1",  'b' -> "q3",  'c' -> "q14"),
    "q2"  -> Map('a' -> "q4",  'b' -> "q14", 'c' -> "q14"),
    "q3"  -> Map('a' -> "q5",  'b' -> "q3",  'c' -> "q14"),
    "q4"  -> Map('a' -> "q6",  'b' -> "q7",  'c' -> "q8"),
    "q5"  -> Map('a' -> "q14", 'b' -> "q0",  'c' -> "q14"),
    "q6"  -> Map('a' -> "q14", 'b' -> "q14", 'c' -> "q9"),
    "q7"  -> Map('a' -> "q4",  'b' -> "q7",  'c' -> "q8"),
    "q8"  -> Map('a' -> "q10", 'b' -> "q14", 'c' -> "q11"),
    "q9"  -> Map('a' -> "q10", 'b' -> "q14", 'c' -> "q14"),
    "q10" -> Map('a' -> "q14", 'b' -> "q14", 'c' -> "q4"),
    "q11" -> Map('a' -> "q12", 'b' -> "q13", 'c' -> "q14"),
    "q12" -> Map('a' -> "q14", 'b' -> "q2",  'c' -> "q14"),
    "q13" -> Map('a' -> "q14", 'b' -> "q4",  'c' -> "q14"),
    "q14" -> Map('a' -> "q14", 'b' -> "q14", 'c' -> "q14")
  )

  def dfaAccepts(word: String): Boolean = {
    var state = DFA_START
    var i = 0
    var ok = true

    while (i < word.length && ok) {
      val ch = word.charAt(i)
      DFA_TRANS.get(state) match {
        case Some(trans) =>
          trans.get(ch) match {
            case Some(next) => state = next
            case None       => ok = false
          }
        case None =>
          ok = false
      }
      i += 1
    }

    ok && DFA_FINAL.contains(state)
  }

  
  val DFA2_START: String = "h1"
  val DFA2_FINAL: Set[String] = Set("h1", "h2", "h4")

  val DFA2_TRANS: Map[String, Map[Char, String]] = Map(
    
    "h1" -> Map(
      'a' -> "h4", 
      'b' -> "h1",
      'c' -> "h2"
    ),
    "h2" -> Map(
      'a' -> "h3", 
      'b' -> "h1",
      'c' -> "h2"
    ),
    "h3" -> Map(
      'a' -> "h3",
      'b' -> "h1",
      'c' -> "h2"
    ),
    "h4" -> Map(
      'a' -> "h3", 
      'b' -> "h1",
      'c' -> "h2"
    )
  )

  def dfa2Accepts(word: String): Boolean = {
    var state = DFA2_START
    var i = 0
    var ok = true

    while (i < word.length && ok) {
      val ch = word.charAt(i)
      DFA2_TRANS.get(state) match {
        case Some(trans) =>
          trans.get(ch) match {
            case Some(next) => state = next
            case None       => ok = false
          }
        case None =>
          ok = false
      }
      i += 1
    }

    ok && DFA2_FINAL.contains(state)
  }

  val NFA_START: String = "f0"
  val NFA_FINAL: Set[String] = Set("g0")

  val NFA_TRANS: Map[String, Map[Char, Set[String]]] = Map(
    "f0" -> Map(
      'c' -> Set("f0"),
      'a' -> Set("f1")
    ),
    "f1" -> Map(
      'a' -> Set("f1"),
      'b' -> Set("f2")
    ),
    "f2" -> Map(
      'b' -> Set("f2"),
      'a' -> Set("f3")
    ),
    "f3" -> Map(
      'b' -> Set("f0")
    ),

    "g0" -> Map(
      'b' -> Set("g1")
    ),
    "g1" -> Map(
      'a' -> Set("u0")
    ),

    "u0" -> Map(
      'b' -> Set("u0"),
      'c' -> Set("u1", "u6"),
      'a' -> Set("u3")
    ),
    "u1" -> Map(
      'a' -> Set("u2")
    ),
    "u2" -> Map(
      'c' -> Set("u0")
    ),
    "u3" -> Map(
      'c' -> Set("u1")
    ),
    "u6" -> Map(
      'c' -> Set("u7")
    ),
    "u7" -> Map(
      'b' -> Set("u8"),
      'a' -> Set("u10")
    ),
    "u8" -> Map(
      'b' -> Set("u0")
    ),
    "u10" -> Map(
      'b' -> Set("g1")
    )
  )

  val NFA_EPS: Map[String, Set[String]] = Map(
    "f0" -> Set("g0"),
    "u0" -> Set("g0")
  )

  def epsilonClosure(states: Set[String]): Set[String] = {
    import scala.collection.mutable

    val stack   = mutable.Stack[String]()
    val closure = mutable.Set[String]() ++ states

    stack.pushAll(states)

    while (stack.nonEmpty) {
      val s = stack.pop()
      val epsTargets = NFA_EPS.getOrElse(s, Set.empty[String])
      for (t <- epsTargets) {
        if (!closure.contains(t)) {
          closure += t
          stack.push(t)
        }
      }
    }

    closure.toSet
  }

  def nfaAccepts(word: String): Boolean = {
    var current: Set[String] = epsilonClosure(Set(NFA_START))
    var i = 0
    var dead = false

    while (i < word.length && !dead) {
      val ch = word.charAt(i)

      val nextStates: Set[String] =
        current.flatMap { s =>
          NFA_TRANS
            .get(s)
            .flatMap(_.get(ch))
            .getOrElse(Set.empty[String])
        }

      if (nextStates.isEmpty) {
        dead = true
      } else {
        current = epsilonClosure(nextStates)
      }

      i += 1
    }

    !dead && current.exists(NFA_FINAL.contains)
  }

  
  val ALPHABET: IndexedSeq[Char] = Vector('a', 'b', 'c')

  def randomWord(maxLen: Int, rng: Random): String = {
    val length = rng.nextInt(maxLen + 1) // допускаем пустое слово
    val sb = new StringBuilder(length)
    var i = 0
    while (i < length) {
      val ch = ALPHABET(rng.nextInt(ALPHABET.length))
      sb.append(ch)
      i += 1
    }
    sb.toString()
  }

  def main(args: Array[String]): Unit = {
    val numWords = 50000
    val maxLen   = 30
    val seed     = 0

    val rng = new Random(seed)

    var allAccept  = 0
    var allReject  = 0
    var mismatch   = 0

    var i = 0
    while (i < numWords) {
      val w  = randomWord(maxLen, rng)

      val r1 = regex1Accepts(w)
      val r4 = regex4Accepts(w)
      val d  = dfaAccepts(w)
      val d2 = dfa2Accepts(w) & d  
      val n  = nfaAccepts(w)

      // Проверка: слово либо принадлежит всем, либо ничему
      if (r1 == r4 && r4 == d && d == n && n == d2) {
        if (r1) allAccept += 1
        else    allReject += 1
      } else {
        mismatch += 1
        println(
          s"Несовпадение для слова '$w': regex1=$r1, regex4=$r4, DFA1=$d, DFA2=$d2, NFA=$n"
        )
      }

      i += 1
    }

    println(s"  Words:      $numWords")
    println(s"  Accepted:   $allAccept")
    println(s"  Declined:   $allReject")
    println(s"  Errors:     $mismatch")
  }
}
