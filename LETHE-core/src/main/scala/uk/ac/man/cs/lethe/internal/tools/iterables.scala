package uk.ac.man.cs.lethe.internal.tools


object Iterables { 
  // attach/1 takes as argument a set of sets {{a1,a2},{b},{c1,c2}}, and returns all combinations of 
  // taking one element each {{a1,b,c1},{a1,b,c2},{a2,b,c1},{a2,b,c2}}.
  def attach[A](setSet: Set[Set[A]]): Set[Set[A]] = { 
    if(setSet.isEmpty)
      Set()
    else if(setSet.size==1)
      setSet.head.map(Set(_))
    else
      setSet.head.flatMap(head => attach(setSet.tail).map(tail => tail++Set(head)))
  }

}
