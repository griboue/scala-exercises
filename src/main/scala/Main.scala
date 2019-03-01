object Main extends App {
  

// ------------ EX 1.1 -----------------
// NIl mean empty list
// x::y je prends élement x et y représente le reste de la liste

def last(list: List[Int]): Option[Int] = list match 
{
  // condition pour sortir de la récursivité si on est au dernier élement

  case head::Nil => Some(head) // Some() permet de renvoyer Null si l'option

  // si on est pas au dernier element on réutilise la fonction last sur le reste de la liste
	case head::tail => last(tail)

  // car par défaut si liste vide
	case _ => None
}
//print(last(List(1, 1, 2, 3, 5, 8)))


// ------------ EX 1.2 -----------------
def nth(list: List[Int], n: Int): Option[Int] = list match 
{
  case h :: t => if (n == 0) Some(h) else nth(t, n-1)
	case Nil => None
}
//print(nth(List(1, 1, 2, 3, 5, 8), 2))




// ------------ EX 1.3 -----------------
def reverse(list: List[Int]): List[Int] = list match 
{
  case h::t => reverse(t) :+ h
  case Nil => Nil
}
//print(reverse(List(1, 1, 2, 3, 5, 8)))


// ------------ EX 1.4 -----------------
def compress(list: List[Int]): List[Int] = {
  list.foldLeft(List[Int]())((accu, el) => 
    if (accu.lastOption.contains(el)) accu
    else accu:+el
  )
}
//print(compress(List(1, 1, 1, 1, 2, 3, 4, 5, 4, 5, 5, 5, 5, 1)))



// ------------ EX 2.1 -----------------
def penultimate(list: List[Int]): Option[Int] = list match {
  case Nil => None
  case h1::h2::Nil => Some(h1)
  case h::Nil => None
  case h::t => penultimate(t)
}
//print(penultimate(List(1, 1, 2, 3, 5, 8)))



// ------------ EX 2.2 -----------------
def lengthIncorrect(list: List[Int]): Int = list match {
  case Nil => 0
  case h::t => 1 + lengthIncorrect(t)
  // this function is dangerous (risk of stack overflow)
}


def lengthCorrect(list: List[Int], res: Int): Int = list match {
  case Nil => res
  case h::t => lengthCorrect(t, res+1)
}
//print(lengthCorrect(List(1, 1, 2, 3, 5, 8), 0))


// ------------ EX 2.3 -----------------
// ??? to make ...


// ------------ EX 2.4 -----------------
def duplication(list: List[Int]): List[Int] = 
{
  list.flatMap(x => List(x,x)) // this is the returned list
}


}