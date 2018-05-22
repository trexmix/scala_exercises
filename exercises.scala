/******************************************************************************
* Find the last element of a list                                             *
******************************************************************************/

def last[T](l: List[T]): T =
{
    l.reverse.head
}


// Doesn't use standard library - just recurse until we have a one element list
// and return that
def last2[T](l: List[T]): T =
{
    l match {
        case Nil => throw new NoSuchElementException
        case x :: Nil => x
        case x :: xs => last(xs)
    }
}

/******************************************************************************
* Find the last but one element of a list.                                    *
******************************************************************************/

// Similar to above, just checks for when we have a two element list
def penultimate[T](l: List[T]): T =
{
    l match {
        case Nil => throw new NoSuchElementException
        case x :: y :: Nil => x
        case x :: y :: xs => penultimate(xs)
    }
}

/******************************************************************************
* Find the Kth element of a list.                                             *
******************************************************************************/

// My original solution- doesn't throw error if the index is too big
def nth[T](n: Int, l: List[T]): T = {
    if (n == 0) l.head else nth(n - 1, l.tail)
}

/******************************************************************************
* Find the number of elements of a list.                                      *
******************************************************************************/

def length[T](l: List[T]): Int = {
    l match {
        case Nil => 0
       	// If the list has anything in it add one to the length of it's tail
        case _ :: xs => 1 + length(xs)
    }
}

/******************************************************************************
* Reverse a list.                                                             *
******************************************************************************/

def reverse[T](l: List[T]): List[T] = {
    l match {
        case x :: Nil => List(x)
        case x :: xs => reverse(xs) ::: List(x)
    }
}

/******************************************************************************
* Find out whether a list is a palindrome.                                    *
******************************************************************************/

def isPalindrome[T](l: List[T]): Boolean = l == l.reverse

/******************************************************************************
* Flatten a nested list structure.                                            *
******************************************************************************/

// Not my solution- I looked up answer as I was struggling with syntax and I
// feel like rewriting wouldn't be productive/would just be derivative
def flatten(ls: List[Any]): List[Any] = ls flatMap {
    case ms: List[_] => flatten(ms)
    case e => List(e)
}

/******************************************************************************
* Eliminate consecutive duplicates of list elements.                          *
******************************************************************************/

def compress(ls: List[Any]): List[Any] = 
  ls match {
      case x :: y :: xs => if (x == y) compress(x :: xs) else x :: compress(y :: xs)
      case _ => ls
  }

/******************************************************************************
* Pack consecutive duplicates of list elements into sublists.                 *
******************************************************************************/

def pack[T](ls: List[T]): List[List[T]] = {
  // Empty list, just need correct return type
  if (ls.isEmpty) List(List())
  else {
  	// Split list into two parts, first part is all elements matching the head
    val (matched, rest) = ls span { _ == ls.head}
    // If no elements don't match head, just make a list out of all of the 
    // (we know) matching elements
    if (rest == Nil) List(matched)
    // Else, put the matching elements in a list, and repeat the process and
	// add it to the end
    else matched :: pack(rest)
  }
}

/******************************************************************************
* Run-length encoding of a list.                                              *
******************************************************************************/

def encode[T](ls: List[T]): List[(Int, T)] = pack(ls).map(es => (es.length, es.head))
