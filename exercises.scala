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