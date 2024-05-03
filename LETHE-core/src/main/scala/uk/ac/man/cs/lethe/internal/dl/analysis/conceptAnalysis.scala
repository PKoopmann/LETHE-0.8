package uk.ac.man.cs.lethe.internal.dl.analysis


import java.io.File
import java.io.PrintStream
import java.util.Date
import scala.collection.SortedMap
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.Stack
import scala.concurrent._
import scala.concurrent.duration._
import scala.io.Source
import uk.ac.man.cs.lethe.internal.application.benchmarks.CustomExecutionContext
import uk.ac.man.cs.lethe.internal.dl.datatypes._
//import uk.ac.man.cs.lethe.internal.dl.filters.OWLOntologyFilters
import uk.ac.man.cs.lethe.internal.dl.forgetting.ConceptAndRoleForgetter
import uk.ac.man.cs.lethe.internal.dl.forgetting.DirectALCForgetter
import uk.ac.man.cs.lethe.internal.dl.forgetting.RoleForgetter
//import uk.ac.man.cs.lethe.internal.dl.owlapi.OWLApiConverter
//import uk.ac.man.cs.lethe.internal.dl.owlapi.OWLApiInterface
//import uk.ac.man.cs.lethe.internal.dl.owlapi.OWLParser
import uk.ac.man.cs.lethe.internal.tools.MockProgressBar


object DeepSymbolAnalyser { 

  implicit val exec = new CustomExecutionContext()

//  def main(args: Array[String]) = {
//    val file = new File(args(0))
//
//    val owlOntology = OWLApiInterface.getOWLOntology(file)
//
//    // restrict expressivity, if required
//    if(args.size>2)
//      args(2) match {
//	case "ALC" => OWLOntologyFilters.restrictToALC(owlOntology)
//	case "ALCH" => OWLOntologyFilters.restrictToALCH(owlOntology)
//	case "ALCHI" => OWLOntologyFilters.restrictToALCHI(owlOntology)
//	case "SHQ" => OWLOntologyFilters.restrictToSHQ(owlOntology)
//	case "SHIQ" => OWLOntologyFilters.restrictToSHIQ(owlOntology)
//      }
//
//    val ontology = OWLApiConverter.convert(owlOntology)
//
//    // only check TBox and RBox
//    ontology.abox = new ABox(Set())
//
//    if(args(1)=="count")
//      countSymbols(ontology)
//    else if(args(1)=="forget")
//      forgetEach(ontology)
//
//  }

  def forgetEach(ontology: Ontology, 
		 out: PrintStream = System.out, 
		 forgetConcepts: Boolean = true,
		 forgetRoles: Boolean = true) {
 
    val forgetter = ConceptAndRoleForgetter

    val inputSize = ontology.size
    val inputAxioms = ontology.statements.size

    val analysis = analyseSymbols(ontology)


    analysis.values.toSeq.sortWith((a,b) => a.value<b.value).foreach{ an =>
      if( an match { 
	case _: ConceptSymbolAnalysis => forgetConcepts
	case _: RoleSymbolAnalysis => forgetRoles
      } ) { 							    
	val symbol = an.name
	out.println
	out.println(an)
	val startTime = (new Date()).getTime
	val futureResult = future[Ontology]{ forgetter.forget(ontology, Set(symbol)) }
	try{
	  //DirectALCForgetter.useTimeOut=true
	  //DirectALCForgetter.timeOut=10000
	  //val result = forgetter.forget(ontology, Set(symbol))
	  val result = Await.result(futureResult, 100 seconds)
	  val duration = (new Date()).getTime - startTime
	  out.print(" duration: "+duration)
	  out.println(" diffSize: "+(result.size-inputSize)+" diffAxioms: "+(result.statements.size-inputAxioms))
	  out.println()
	} catch { 
	  case _: TimeoutException =>
	    out.println("Timeout.")
	    exec.lastThread.getOrElse(throw new RuntimeException("Not started")).stop()
	}
      }
    }

  }

  def countSymbols(ontology: Ontology) { 

    val analysis = analyseSymbols(ontology).values.toSet

    analysis.toSeq.sortWith((a,b) => a.underUniversalRestrictions<b.underUniversalRestrictions).foreach{ println }
    println()
    println("Concepts: "+analysis.filter(_.isInstanceOf[ConceptSymbolAnalysis]).size)
    println("Only positive: "+analysis.filter(_.isInstanceOf[ConceptSymbolAnalysis]).filter(_.asInstanceOf[ConceptSymbolAnalysis].onlyPositive).size)
    println("Only negative: "+analysis.filter(_.isInstanceOf[ConceptSymbolAnalysis]).filter(_.asInstanceOf[ConceptSymbolAnalysis].onlyNegative).size)
    println()
    println("Roles: "+analysis.filter(_.isInstanceOf[RoleSymbolAnalysis]).size)
    println("Only universally restricted: "
	    +analysis.filter(_.isInstanceOf[RoleSymbolAnalysis]).filter(_.asInstanceOf[RoleSymbolAnalysis].onlyUniversallyRestricted).size)
    println("Only existentially restricted: "
	    +analysis.filter(_.isInstanceOf[RoleSymbolAnalysis]).filter(_.asInstanceOf[RoleSymbolAnalysis].onlyExistentiallyRestricted).size)

    graph(analysis)
  }

  def graph(analysis: Set[SymbolAnalysis]) = { 
    val sorted = analysis.toSeq.sortWith((a,b) => a.underUniversalRestrictions<b.underUniversalRestrictions)

    var map: Map[Long, Long] = Map()

    var count = 0
    sorted.foreach{ num => count+=1; map=map.updated(num.underUniversalRestrictions, count) }

    val dataFile = new File("underUniversal.dat")
    val output = new PrintStream(dataFile)

    map.toSeq.sorted.foreach{ pair => output.println(pair._1.toString+" "+pair._2.toString) }

    output.close()
  }


  def analyseSymbols(ontology: Ontology): Map[Symbol, SymbolAnalysis] = { 

    val map = new HashMap[Symbol, SymbolAnalysis]()

    val nnf = DLHelpers.nnf(ontology.tbox)

    def getAnalysis(symbol: Symbol) = { 
      map.get(symbol) match { 
	case Some(analysis) => ;
	case None => symbol match { 
	  case b: BaseConcept => map.put(b, new ConceptSymbolAnalysis(b))
	  case r: BaseRole => map.put(r, new RoleSymbolAnalysis(r))
	}
      }
	map(symbol)
      }
   

    def analyseConcept(concept: Concept): Unit = concept match { 
      case BottomConcept => ;
      case TopConcept => ;
      case b: BaseConcept => 
	Some(getAnalysis(b).asInstanceOf[ConceptSymbolAnalysis]).foreach{d => d.occurrences+= 1; d.onlyNegative=false} 
      case ConceptComplement(b: BaseConcept) => 
	Some(getAnalysis(b).asInstanceOf[ConceptSymbolAnalysis]).foreach{d => d.occurrences+= 1; d.onlyPositive=false} 
      case ConceptConjunction(cs) => cs.foreach(analyseConcept)
      case ConceptDisjunction(ds) => ds.foreach(analyseConcept)
      case ExistentialRoleRestriction(r: BaseRole, c) => { 
	getAnalysis(r).occurrences+=1
	getAnalysis(r).asInstanceOf[RoleSymbolAnalysis].existentiallyRestricted += 1
	c.atomicConcepts.map(BaseConcept).foreach{ getAnalysis(_).underExistentialRestrictions+=1 }
	c.roleSymbols.map(BaseRole).foreach{ getAnalysis(_).underExistentialRestrictions+=1 }
	analyseConcept(c)
      }
      case UniversalRoleRestriction(r: BaseRole, c) => { 
	getAnalysis(r).occurrences+=1
	getAnalysis(r).asInstanceOf[RoleSymbolAnalysis].universallyRestricted += 1
	c.atomicConcepts.map(BaseConcept).foreach{ getAnalysis(_).underUniversalRestrictions+=1 }
	c.roleSymbols.map(BaseRole).foreach{ getAnalysis(_).underUniversalRestrictions+=1 }
	analyseConcept(c)
      }
      case _ => assert(false, "Unexpected concept: "+concept)
    }

    nnf.axioms.foreach{ _ match { 
      case Subsumption(TopConcept, concept) => analyseConcept(concept)
      case other => assert(false, "Unexpected axiom: "+other)
    }}

    map.toMap
    //map.values.toSet
  }

}



class SymbolAnalysis(val name: String) { 

  var occurrences: Int=0

  var underExistentialRestrictions: Int=0
  var underUniversalRestrictions: Int=0

  def value = Math.sqrt(occurrences)+underUniversalRestrictions

  def nameString = 
    "Name: " + name

  override def toString = nameString + 
    "\n Occurrences: " + occurrences + 
    "\n Under Existential Restrictions: "+underExistentialRestrictions+
    "\n Under Universal Restrictions: "+underUniversalRestrictions+
    "\n Value: "+value
//  var underNoRestriction: Int=0

  // var directExistentialRestrictions: Int=0
  // var directUniversalRestrictions: Int=0

  // assert(directExistentialRestrictions<=underExistentialRestrictions)
  // assert(directUniversalRestrictions<=underUniversalRestrictions)

}

class ConceptSymbolAnalysis(concept: BaseConcept) extends SymbolAnalysis(concept.name) { 
  var onlyPositive: Boolean=true
  var onlyNegative: Boolean=true
  
  override def nameString = super.nameString+ " (concept)"

  override def toString = 
    super.toString+
    (if(onlyPositive) "\n Only Positive" else "")+
    (if(onlyNegative) "\n Only Negative" else "")+
    "Concept"
}

class RoleSymbolAnalysis(role: BaseRole) extends SymbolAnalysis(role.name) { 
  var existentiallyRestricted: Int = 0
  var universallyRestricted: Int = 0

  override def nameString = super.nameString + " (role)"

  override def toString = 
    super.toString+
    "\n Existentially Restricted: "+existentiallyRestricted +
    "\n Universally Restricted: "+universallyRestricted

  def onlyUniversallyRestricted = existentiallyRestricted == 0
  def onlyExistentiallyRestricted = universallyRestricted == 0
}


object ResultsVisualizer { 
  
  class Average { 
    var value: Double = 0.0
    var count: Int = 0

    var m2: Double = 0.0 // for variance

    def add(newValue: Long) = { 
      count += 1
      value += (newValue-value)/count
      m2 += (newValue-value)*(newValue-value)
    }

    def variance = if(count<=1) 0 else m2/(count-1)
  }

  abstract class Binner { 
    def b(value: Int): Double
  }

  case class EqualSizeBinner(size: Int) extends Binner { 
    override def b(value: Int) = value - (value%size)
  }

  class LogBinner(scale: Double = 1) extends Binner { 
    override def b(value: Int) = Math.exp(Math.log((value+1)).toInt*scale)/scale
  }

  case class Struct(var value: Double, 
		    var occurrences: Int, 
		    var univ: Int, 
		    var duration: Long,
		  var timeout: Boolean = false,
		  var univRestricted: Int = 0,
		  var exisRestricted: Int = 0) { 
    def this() = this(0.0, 0, 0, 0L) 
  }

  class Structs{ 
    val occurrences = new HashMap[Double,Average]() 
    val univ = new HashMap[Double, Average]()
    val occTimeout = new HashMap[Double, Average]()
    val univTimeout = new HashMap[Double, Average]()
    val univRDur = new HashMap[Double, Average]()
    val univRTimeout = new HashMap[Double, Average]()

    var structs = List[Struct]()

    val b: Binner = new LogBinner(.1)

    def add(struct: Struct) = {

      structs = struct::structs

      if(!occurrences.contains(b.b(struct.occurrences))){ 
	occurrences.put(b.b(struct.occurrences), new Average())
	occTimeout.put(b.b(struct.occurrences), new Average())
      }

      if(!univ.contains(b.b(struct.univ))){ 
	univ.put(b.b(struct.univ), new Average())
	univTimeout.put(b.b(struct.univ), new Average())
      }


      if(!univRDur.contains(b.b(struct.univRestricted))){ 
	univRDur.put(b.b(struct.univRestricted), new Average())
	univRTimeout.put(b.b(struct.univRestricted), new Average())
      }

      if(struct.timeout){ 
	occTimeout(b.b(struct.occurrences)).add(1)
	univTimeout(b.b(struct.univ)).add(1)
	univRTimeout(b.b(struct.univRestricted)).add(1)
	// occurrences(b.b(struct.occurrences)).add(struct.duration)
	// univ(b.b(struct.univ)).add(struct.duration)
      } else { 
	occurrences(b.b(struct.occurrences)).add(struct.duration)
	univ(b.b(struct.univ)).add(struct.duration)
	univRDur(b.b(struct.univRestricted)).add(struct.duration)
	occTimeout(b.b(struct.occurrences)).add(0)
	univTimeout(b.b(struct.univ)).add(0)
	univRTimeout(b.b(struct.univRestricted)).add(0)
      }
    }
  }

  def main(args: Array[String]) = { 
    val structs = parse(new File(args(0)), SymbolType.ROLE)

    Seq((structs.occurrences, ".occDur"),
        (structs.univ, ".univDur"),
        (structs.occTimeout, ".occTimeout"),
	(structs.univTimeout, ".univTimeout"),
	(structs.univRDur, ".univRDur"),
	(structs.univRTimeout, ".univRTimeout")).foreach{ pair =>
	  val (map, ending) = pair
	  val sorted = map.toSeq.sortBy(_._1)
	  val dataFile = new File(args(1)+ending)
	  val output = new PrintStream(dataFile)
	  sorted.foreach{ tuple => if((tuple._2.count)>0)
			 output.println(tuple._1+" "+tuple._2.value+" "+tuple._2.variance+" "+tuple._2.count) }
	  output.close()
    }

    

    // val dataFile1 = new File(args(1)+".occTime")
    // val output1 = new PrintStream(dataFile1)
    // val dataFile2 = new File(args(1)+".univTime")
    // val output2 = new PrintStream(dataFile2)
    // val dataFile3 = new File(args(1)+".valTime")
    // val output3 = new PrintStream(dataFile3)
    // val dataFile4 = new File(args(1)+".univExTime")
    // structs.occurrences.foreach{ tuple => output1.println(tuple._1+" "+tuple._2.value)}
    // structs.univ.foreach{ tuple => output2.println(tuple._1+" "+tuple._2.value)}

    // structs.structs.foreach{ struct => 
    //   output1.println(struct.occurrences+" "+struct.duration)
    //   output2.println(struct.univ+" "+struct.duration)
    //   output3.println(struct.value+" "+struct.duration)
    // }

    // List(output1,output2).foreach{ _.close() }    
  }

  object SymbolType extends Enumeration { 
    type SymbolType = Value
    val CONCEPT = Value("concept")
    val ROLE = Value("role")
  }

  def parse(file: File, symbolType: SymbolType.SymbolType) = { 

    val result = new Structs()

    val rName = ("""Name: (.*) \("""+symbolType.toString+"""\)""").r
    val rOcc = """ Occurrences: (\d+)""".r
    val rVal = """ Value: (\d+(?:\.\d+)?)""".r
    val rUniv = """ Under Universal Restrictions: (\d+)""".r
    val rUnivR = """ Universally Restricted: (\d+)""".r
    val rExisR = """ Existentially Restricted: (\d+)""".r
    val rDur = """ duration: (\d+) .*""".r
    val rIgnore = """ Only (Negative|Positive)""".r
    val rTimeout = "Timeout."

    println(rName)

    var curr = new Struct()

    var ignore = true

    Source.fromFile(file).getLines.foreach{ line =>
      line match { 
	case rName(_) => { 
	  ignore = false
	  curr = new Struct()
	}
	case rOcc(occ) => { 
	  curr.occurrences=occ.toInt
	}
	case rVal(value) => curr.value=value.toDouble
	case rUniv(value) => curr.univ=value.toInt
	case rUnivR(value) => curr.univRestricted=value.toInt
	case rExisR(value) => curr.exisRestricted=value.toInt
	case rIgnore(_) => ignore = true
	case rDur(dur) => { 
	  assert(curr.occurrences>0)
	  curr.duration=dur.toLong
	  curr.timeout=false
	  if(!ignore)
	    result.add(curr)
	  ignore = true
	}
	case line if line==rTimeout => {  
	  curr.duration=10000 
	  curr.timeout=true 
	  if(!ignore)
	    result.add(curr) 
	  ignore = true
	}
	case _ => ;
      }
    }

    result
  }

}
