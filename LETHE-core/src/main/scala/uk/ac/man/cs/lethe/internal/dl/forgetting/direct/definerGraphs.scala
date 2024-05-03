
package uk.ac.man.cs.lethe.internal.dl.forgetting.direct

import scala.collection.mutable.{ HashMap, MultiMap, HashSet }
import scala.collection.mutable

import uk.ac.man.cs.lethe.internal.dl.datatypes._

import com.typesafe.scalalogging.Logger

object CycleRemover { 

  //implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(this)

  val logger = Logger(CycleRemover.getClass)

  def removeCycles(clauses: Iterable[ConceptClause], nonBaseSymbols: Set[String])
  : Set[ConceptClause] = { 
    removeCycles(DefinerGraphBuilder.buildDefinerGraph(clauses), nonBaseSymbols)
  }

  def removeCycles(definerGraph: DefinerGraph, nonBaseSymbols: Set[String])
  : Set[ConceptClause] = { 

    logger.debug("Checking for redundant clauses...")

    var top = getTopDefiners(definerGraph)
    logger.debug("Definers which equal top:" + top.mkString(", "))
    top.foreach{ definerGraph.setToTop(_)}

    var bottom = getBottomDefiners(definerGraph)
    logger.debug("Definers which equal bottom:" + bottom.mkString(", "))
    bottom.foreach{ definerGraph.setToBottom(_)}

    // var unreachable = getUnreachableDefiners(definerGraph)
    // logger.debug("Unreachable definers: " + top.mkString(", "))
    // unreachable.foreach{ definerGraph.setToTop(_)}
    // <-- dangerous when forgetting incrementally

    var alphaCyclic = getAlphaCyclicDefiners(definerGraph)
    logger.debug("Alpha-cyclic definers: " + top.mkString(", "))
    alphaCyclic.foreach{ definerGraph.setToTop(_)}

    var cyclic = getCyclicDefiners(definerGraph)
    logger.debug("Cyclic definers left: " + cyclic.mkString(", "))

    definerGraph.clauses.toSet[ConceptClause]
  }

  def getTopDefiners(definerGraph: DefinerGraph): Set[BaseConcept] = { 
    definerGraph.nodes.filter{ node => 
      !definerGraph.definerSets.contains(node) || definerGraph.definerSets(node).isEmpty }
  }

  def getBottomDefiners(definerGraph: DefinerGraph): Set[BaseConcept] = { 
    (definerGraph.nodes-EPSILON).filter(definerGraph.definerSets(_).exists(_.literals.size==1))
  }

  def getUnreachableDefiners(definerGraph: DefinerGraph): Set[BaseConcept] = { 
    val processed = new HashSet[BaseConcept]()

    def walk(node: BaseConcept): Unit = 
      if(processed(node))
	return
      else { 
	processed.add(node)
	definerGraph.successors(node).foreach(walk)	  
      }
  
    walk(EPSILON)

    definerGraph.nodes -- processed
  }


  def getAlphaCyclicDefiners(definerGraph: DefinerGraph): Set[BaseConcept] = { 
    var knownAlphaCyclic: Set[BaseConcept] = Set()
    definerGraph.nodes.foreach{ node =>
      if(!knownAlphaCyclic(node)){ 
	knownAlphaCyclic ++= collectAlphaCyclic(definerGraph, node, knownAlphaCyclic)
      }
	
    }

    return knownAlphaCyclic
  }

  def collectAlphaCyclic(definerGraph: DefinerGraph, 
			 definer: BaseConcept, 
			 knownAlphaCyclic: Set[BaseConcept]): Set[BaseConcept] = { 

    var _knownAlphaCyclic = knownAlphaCyclic

    // def filtered(clauses: Set[Clause]) = 
    //   clauses.filter(!_.literals.exists(_ match { 
    // 	case ConceptLiteral(_, BaseConcept(name)) => 
    // 	case ConceptLiteral(true, d: BaseConcept) => definerGraph.nodes(d)
    //   }

    // every clause contains a universal restriction
    def eachUniversal(clauses: Set[ConceptClause]): Boolean = 
      clauses.forall(_.literals.exists(_.concept.isInstanceOf[UniversalRoleRestriction]))

    def inner(node: BaseConcept, processed: Set[BaseConcept]): Boolean = { 
      if(_knownAlphaCyclic(node)){ 
	logger.trace("known alpha cyclic: "+node.toString)
	true
      }

      if(processed.contains(node)) { 
	logger.trace("new alpha cyclic: "+node.toString)
	_knownAlphaCyclic += node
	true
      }
      else if(!eachUniversal(definerGraph.definerSets(node))){ 
	logger.trace("stopping at "+node.toString + ", "+ definerGraph.definerSets(node).toString)
	false
      }
      else
	definerGraph.definerSets(node).forall(universalRestricted(_).exists(inner(_, processed+node)))
    }
    if(inner(definer, Set())){ 
      logger.trace("New alpha cyclic to be added: " + _knownAlphaCyclic.toString)
      _knownAlphaCyclic
    }
    else
      Set()
  }

  def findNewSubsumptions(definerGraph: DefinerGraph, old: SubsumptionChecker)
  : Map[BaseConcept, Set[BaseConcept]] = { 
    logger.debug("Checking for new subsumptions")

    var known = Map[BaseConcept, Set[BaseConcept]]() 
    var not = Set[(BaseConcept, BaseConcept)]()

    val nodes = (definerGraph.nodes - EPSILON).toSeq.sortWith{ 
      (a, b) => reachableDefiners(a, definerGraph).size < reachableDefiners(b, definerGraph).size
    }

    nodes.foreach{ node1 =>
//      known = known.updated(node1, Set()) 
      nodes.foreach{ node2 =>
	if(!old.subsumes(node1, node2)){ 
	  var (_known, _not) = pathSubsumptions(node1, node2, definerGraph, old, known, not)
	  known=_known
	  not=_not
	  logger.trace("known: " + known.mkString("; "))
	  logger.trace("not: " + not.mkString("; "))
	}
			       }
			     }

    known.filterNot((pair: (BaseConcept, Set[BaseConcept])) => pair._2.isEmpty)
  }

  def add[A](map: Map[A, Set[A]], pair: (A, A)): Map[A, Set[A]] = { 
    val (key, value) = pair
    var result = map

    if(!map.contains(key))
      result = result.updated(key, Set())
    result.updated(key, result(key)+value)
  }

  def pathSubsumptions(definer1: BaseConcept, 
		       definer2: BaseConcept, 
		       definerGraph: DefinerGraph,
		       subsumptionChecker: SubsumptionChecker,
		       known: Map[BaseConcept, Set[BaseConcept]],
		       not: Set[(BaseConcept, BaseConcept)])
  : (Map[BaseConcept, Set[BaseConcept]], Set[(BaseConcept, BaseConcept)]) = { 
    logger.trace("Checking path subsumption between " + definer1.toString + " and " + definer2.toString)

    var newKnown = known
    var newNot = not

    def inner(definer1: BaseConcept, 
	      definer2: BaseConcept,
	      processed: Set[Tuple2[BaseConcept, BaseConcept]]): Boolean = {
//      logger.trace(processed.mkString(" - "))
      if(processed((definer1, definer2))){
	newKnown=add(newKnown,(definer1, definer2))
	true
      }
      else if(newKnown.contains(definer1) && newKnown(definer1)(definer2)){
	true
      }
      else if(subsumptionChecker.subsumes(definer1, definer2))
	true
      else if(newNot(definer1, definer2)){ 
	false
      }
      else { 
	// A n B n C subsumes A n (B u C) 
	val result = definerGraph.definerSets(definer2).forall{ 
	  clause2 => 
	    definerGraph.definerSets(definer1).exists{ 
	      clause1 =>
		subsumes(definer1, 
			 definer2, 
			 clause1, 
			 clause2, 
			 processed+((definer1, definer2)))
	    }
	}
	if(result==true){ 
	  newKnown=add(newKnown, (definer1, definer2))
	} else
	  newNot += ((definer1, definer2))
	  
	result
      }
    }
       
      
    def subsumes(definer1: BaseConcept,
		 definer2: BaseConcept,
		 clause1: ConceptClause, 
		 clause2: ConceptClause,
		 processed: Set[Tuple2[BaseConcept, BaseConcept]]): Boolean = { 
      // A u B subsumes A u B u C
      clause1.literals.forall{ literal1 => literal1.concept==definer1 ||
	  clause2.literals.exists{ 
	    literal2 => literal2.concept!=definer2 && (
	      literal1.concept==definer1 && literal2.concept==definer2 
	      || subsumptionChecker.subsumes(literal1, literal2)
	      || ((literal1.concept, literal2.concept) match { 
		case (UniversalRoleRestriction(r1, d1: BaseConcept), 
		      UniversalRoleRestriction(r2, d2: BaseConcept)) if r1==r2 =>
		  inner(d1, d2, processed)
		case (ExistentialRoleRestriction(r1, d1: BaseConcept), 
		      ExistentialRoleRestriction(r2, d2: BaseConcept)) if r1==r2 =>
		  inner(d1, d2, processed)
		case _ => false
	      })
	    )
	  }
			    }
    }
	      
    inner(definer1, definer2, Set())
    return (newKnown, newNot)
  }

  /**
   * return all literals in clause that are universal restrictions
   */
  def universalRestricted(clause: ConceptClause): Set[BaseConcept] = 
    clause.literals.collect{ 
      case ConceptLiteral(true, UniversalRoleRestriction(_, d: BaseConcept)) => d 
			}

  def universalSuccessors(definerGraph: DefinerGraph, node: BaseConcept): Set[BaseConcept] = 
    definerGraph.definerSets(node).flatMap(_.literals.collect{ 
      case ConceptLiteral(true, UniversalRoleRestriction(r, d: BaseConcept)) => definerGraph.successors(d)
    }).flatten


  def existentialSuccessors(definerGraph: DefinerGraph, node: BaseConcept): Set[BaseConcept] = 
    definerGraph.definerSets(node).flatMap(_.literals.collect{ 
      case ConceptLiteral(true, ExistentialRoleRestriction(r, d: BaseConcept)) => definerGraph.successors(d)
    }).flatten


  def getCyclicDefiners(definerGraph: DefinerGraph): Set[BaseConcept] = {  
    var knownCyclic: Set[BaseConcept] = Set()

    def isCyclic(node: BaseConcept, processed: Set[BaseConcept]): Boolean = { 
      if(knownCyclic(node))
	true
      else if(processed.contains(node)) { 
	knownCyclic += node
	true
      }
      else { 
	definerGraph.successors(node).view.exists(node2 => isCyclic(node2, processed+node))
      }
    }

    definerGraph.nodes.foreach{ node =>
      if(isCyclic(node, Set()))
	knownCyclic += node
			     }

    knownCyclic
  }


  def getPaths(definerGraph: DefinerGraph, start: BaseConcept): Iterable[Seq[BaseConcept]] = { 
    
    def inner(node: BaseConcept, processed: Set[BaseConcept]): Iterable[Seq[BaseConcept]] = 
      if(processed.contains(node))
	Set(List(node))
      else { 
	definerGraph.successors(node).view.flatMap(node2 => inner(node2, processed+node).view.map(
	  (seq: Seq[BaseConcept]) => List(node)++seq
					))
      }
   
    inner(start, Set())
  }

  def isCyclic(path: Seq[BaseConcept]) = 
    path.head==path.last
  
  def reachableDefiners(definer: BaseConcept, definerGraph: DefinerGraph): Set[BaseConcept] = { 
    var processed: Set[BaseConcept] = Set()

    def inner(definer: BaseConcept): Unit = { 
      if(processed(definer))
	return
      processed += definer
      definerGraph.successors(definer).foreach(inner)
    }

    inner(definer)
    processed
  }
}

object DefinerGraphBuilder { 
  def buildDefinerGraph(clauses: Iterable[ConceptClause]): DefinerGraph = { 
    val definerGraph = new DefinerGraph()

    definerGraph.nodes = getDefiners(clauses)+EPSILON
    definerGraph.definerSets = getDefinerSets(definerGraph.nodes, clauses)
    definerGraph.edges = getEdges(definerGraph)

    definerGraph
  }

  private def getEdges(definerGraph: DefinerGraph): Map[BaseConcept, Map[Role, Set[BaseConcept]]] = { 
    val mapping = new HashMap[BaseConcept, MultiMap[Role, BaseConcept]]() 

    def addBinding(definer1: BaseConcept, role: Role, definer2: BaseConcept) = { 
      if(!mapping.contains(definer1))
	mapping.put(definer1, new HashMap[Role, mutable.Set[BaseConcept]]() with MultiMap[Role, BaseConcept])
      mapping(definer1).addBinding(role, definer2)
    }

    definerGraph.definerSets.foreach { 
      pair => { 
	val (definer, set) = pair
	set.foreach{ clause =>
	  clause.literals.collect{ _.concept match { 
	    case UniversalRoleRestriction(role, definer2: BaseConcept) => addBinding(definer, role, definer2)
	    case ExistentialRoleRestriction(role, definer2: BaseConcept) => addBinding(definer, role, definer2)
	  }}
	}
      }	 
    }
    
    
    mapping.toMap.map{ 
      pair => (pair._1, pair._2.toMap.map { 
	pair2 => (pair2._1, pair2._2.toSet)
      })
    }
  }

  private def getDefinerSets(definers: Set[BaseConcept], clauses: Iterable[ConceptClause])
  : Map[BaseConcept, Set[ConceptClause]] = { 
    val mapping = new HashMap[BaseConcept, mutable.Set[ConceptClause]]() 
      with MultiMap[BaseConcept, ConceptClause]

    clauses.foreach { clause => { 
	val definerOpt = clause.literals.collectFirst{ 
	  case ConceptLiteral(false, a: BaseConcept) if definers(a) => a
	}
	definerOpt match { 
	  case None => mapping.addBinding(EPSILON, clause)
	  case Some(b) => mapping.addBinding(b, clause)
	}
      }
    }
  
    return mapping.toMap.map{ pair => (pair._1, pair._2.toSet)}
  }

  private def getDefiners(clauses: Iterable[ConceptClause]) = 
    clauses.flatMap(clause => clause.literals.collect { 
      _.concept match { 
	case ExistentialRoleRestriction(_, definer) => definer.asInstanceOf[BaseConcept]
	case UniversalRoleRestriction(_, definer) => definer.asInstanceOf[BaseConcept]
	case b: BaseConcept if b.name.startsWith("_D") => b // less nice than the previous, but needed if
							    // we have unreachable definers
      } 
    }).toSet[BaseConcept]
}

object EPSILON extends BaseConcept("_EPSILON")

object DefinerGraph 

class DefinerGraph { 
//  implicit val (logger, formatter, appender) =
//  ZeroLoggerFactory.newLogger(DefinerGraph)
  val logger = Logger[DefinerGraph]

  var nodes: Set[BaseConcept] = _
  var definerSets: Map[BaseConcept, Set[ConceptClause]] = _
  var edges: Map[BaseConcept, Map[Role, Set[BaseConcept]]] = _

  def apply(node: BaseConcept) = edges.apply(node)

  def definers = nodes

  def successors(node: BaseConcept): Set[BaseConcept] = 
    if(!edges.contains(node))
      Set()
    else
      edges(node).values.flatten.toSet[BaseConcept]

  def clauses: Iterable[ConceptClause] = definerSets.flatMap(_._2)

  def setToTop(definer: BaseConcept) = { 
    def removeDefinerTop(clause: ConceptClause, definer: BaseConcept): Option[ConceptClause] = { 
      if(clause.literals.exists{ _ match { 
	case ConceptLiteral(true, UniversalRoleRestriction(r, d)) => d==definer
	case _ => false
      }})
	None
      else
	Some(clause)
    }


    // Nodes
    //    nodes -= definer

    // Definer sets
    definerSets = definerSets.updated(definer, Set())

    // Edges
    edges -= definer
    //    edges = edges.mapValues(_.mapValues(_-definer))

    // Literals
    definerSets = definerSets.mapValues(_.flatMap(removeDefinerTop(_, definer)))
  }


  def setToBottom(definer: BaseConcept) = { 
    def removeDefinerBottom(clause: ConceptClause, definer: BaseConcept): ConceptClause = { 
      val remove = clause.literals.filter{ _ match { 
	case ConceptLiteral(true, ExistentialRoleRestriction(r, d)) => d==definer
	case _ => false
      }}

      clause.without(remove)
    }


    // Nodes
    //    nodes -= definer

    // Definer sets
    definerSets = definerSets.updated(definer, Set())

    // Edges
    edges -= definer
    //    edges = edges.mapValues(_.mapValues(_-definer))

    // Literals
    definerSets = definerSets.mapValues(_.map(removeDefinerBottom(_, definer)))
  }

  // return non-base symbols to which paths from the current definer exist
  def reachableNonBaseSymbols(definer: BaseConcept, nonBaseSymbols: Set[String]): Set[String] = { 
    var result: Set[String] = Set()

    var processed: Set[BaseConcept] = Set()

    def walk(node: BaseConcept): Unit = { 
      if(processed.contains(node))
	return
      if(result==nonBaseSymbols)
	return

      if(definerSets.contains(node))
	result ++= definerSets(node).flatMap(_.signature).intersect(nonBaseSymbols)

      processed += node
      successors(node).foreach{ walk(_) }
    }

    walk(definer)

    return result
  }

  def reachableNonBaseSymbols(nonBaseSymbols: Set[String]): Map[BaseConcept, Set[String]] = { 
    Map()++nodes.map{ node => (node, reachableNonBaseSymbols(node, nonBaseSymbols))}
  }

  def updateReachableNonBaseSymbols(nonBaseSymbols: Set[String], old: Map[BaseConcept, Set[String]]) = { 
    val result = Map()++nodes.map{ node => 
      if(!old.contains(node))
	(node, reachableNonBaseSymbols(node, nonBaseSymbols))
      else
	(node, reachableNonBaseSymbols(node, old(node)))
    }

    logger.debug("reachable non-base symbols: \n" + result.mkString("\n"))

    result
  }
}
