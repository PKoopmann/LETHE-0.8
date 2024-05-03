package uk.ac.man.cs.lethe.internal.tools.statistics



class IncrementalValuesCount { 

  private var numberOfValues: Double = 0
  
  private var currentMean: Double = 0

  private var currentVarianceSum: Double = 0


  private var values = List[Double]()


  def addValue(v: Double) = { 
    numberOfValues += 1

    var delta = v-currentMean

    currentMean = currentMean + (delta/numberOfValues)

    currentVarianceSum = currentVarianceSum + delta*(v-currentMean)

    values = v::values
  }



  def mean = 
    currentMean


  def variance = 
    if(numberOfValues<2)
      0.0
    else
      currentVarianceSum/(numberOfValues-1)


  def sort() = 
    values = values.sortBy(a => -a)


  def median = 
    if(numberOfValues%2 == 1)
      values.takeRight((numberOfValues/2+1).toInt).head
    else { 
      val first = values.takeRight((numberOfValues/2).toInt).head
      val second = values.takeRight((numberOfValues/2+1).toInt).head
      (first+second)/2
    }

  def quantil(value: Double) = {
    val position = (value*numberOfValues).toInt
    values(position)
  }


  override def toString = { 
    sort()
    "mean: "+mean+ "  variance: "+variance + "  median: "+median + "  90th percentile: "+quantil(.9)
  }
}
