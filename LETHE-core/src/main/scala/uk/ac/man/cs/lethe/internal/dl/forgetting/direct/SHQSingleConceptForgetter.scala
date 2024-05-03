package uk.ac.man.cs.lethe.internal.dl.forgetting.direct

import scala.collection.mutable.{ Set => mutSet, HashSet, HashMap, MultiMap, TreeSet, Queue }

//import com.dongxiguo.fastring.Fastring.Implicits._
import com.typesafe.scalalogging.Logger

import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.forgetting.Forgetter

object SHQSingleConceptForgetter 

class SHQSingleConceptForgetter(roleHierarchy: RoleHierarchy,
				transitiveRoles: Set[BaseRole]) 
extends ClauseSetListener { 

  //implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(SHQSingleConceptForgetter)
  //import formatter._

  val logger = Logger[SHQSingleConceptForgetter]

  var definerFactory: DefinerFactory = _
  val subsumptionChecker = 
    new SimpleSubsumptionChecker() with SHQSubsumptionChecker with RoleSubsumptionChecker with DefinerSubsumptionChecker


  var result: TreeSet[ConceptClause] = _
  var posClauses: TreeSet[ConceptClause] = _
  var negClauses: TreeSet[ConceptClause] = _
  var allClauses: TreeSet[ConceptClause] = _
  

  var definingClauses = new HashMap[BaseConcept, mutSet[ConceptClause]]() 
			with MultiMap[BaseConcept, ConceptClause]

  var definerUsages = new HashMap[BaseConcept, mutSet[(ConceptLiteral, ConceptClause)]]()
		      with MultiMap[BaseConcept, (ConceptLiteral, ConceptClause)]

  var combined = new HashMap[BaseConcept, mutSet[BaseConcept]]() 
		with MultiMap[BaseConcept, BaseConcept]

  var resolutionRule: ResolutionRule = _
  var numberRestrictionRule: NumberRestrictionRule = _
  var transitivityRule: TransitivityRule = _

  var forgetConcept: BaseConcept = _

  var ordering: ConceptLiteralOrdering = _

  def forget(clauses: Set[ConceptClause], forgetConcept: BaseConcept) = { 
    init(clauses, forgetConcept)

    derive()

    result.toSet[ConceptClause]
  }

  def derive() = { 

    while(!negClauses.isEmpty){ 
      val nextClause = negClauses.head
      logger.debug(s"\nResolving on ${nextClause}")
      negClauses.remove(nextClause)
      var candidates = posClauses
      var negDef = getNegDefiners(nextClause)
      if(negDef.size>0){ 
	if(combined.contains(negDef.head))
	   candidates --= combined(negDef.head).flatMap(definingClauses.getOrElse(_, Set[ConceptClause]())) // these would be ignored anyway
      }
      var conc = resolutionRule.getDerivations(nextClause, posClauses).flatMap(_.conclusions)
      conc.toSeq.sortBy(_.literals.size).foreach(proceed)
      logger.debug(s"${allClauses.size}")
    }
  }

  def getNegDefiners(clause: ConceptClause): Set[BaseConcept] = { 
    clause.literals.collect{ 
      case ConceptLiteral(false, c: BaseConcept) if isDefiner(c) => c 
    }
  }
//  val largestNegDef = ConceptLiteral(false, BaseConcept("_D"))

  def proceed(_clause: ConceptClause): Unit = { 

    logger.trace(s"proceed with ${_clause}")

    val negDefiners = getNegDefiners(_clause)
						     

    if(negDefiners.size<2){
      // checking for redundancy already done later?
      if(!redundant(_clause))
        // Check: condensing clause might be redundant here (done later anyway)
	addClause(subsumptionChecker.condenseClause(_clause))
      else
	logger.trace(s"${_clause} is redundant")
    } else { 
      assert(negDefiners.size==2, negDefiners) // since we only use binary rules

      val d1::d2::Nil = negDefiners.toList//.map(_.concept.asInstanceOf[BaseConcept])

      definerFactory.representative(d1, d2) match { 
	case Some(d3) => 
	  // var clause2 = clause.without(ConceptLiteral(false,d1))
	  // clause2 = clause2.without(ConceptLiteral(false, d2))
	  // clause2 = clause2._with(ConceptLiteral(false, d3))
	  // addClause(clause2)
	  logger.trace(s"(ignoring ${_clause}, not needed)")
	  //assert(false, "not sure yet whether this case should be possible")
	case None => {
	  if(redundant(_clause)){ 
	    logger.trace(s"${_clause} is redundant")
	    return
	  }

	  logger.debug(s"derived clause: ${_clause}")
	  logger.debug("Trigger!")
	  // new definer plus new definitions (simply reuse resolution rule)
	  val (d3, intermediateDefinitions) = definerFactory.combineDefiners(d1, d2)
	  combined.addBinding(d1, d2) // TODO: should be addBinding(d1,d3)
	  combined.addBinding(d2, d3)
	  logger.debug(s"Introducing new definer ${d3} representing ${d1} and ${d2}")
	  removeRedundants(d1)
	  removeRedundants(d2)

	  if(_clause.literals.size>2){ 
	    addClause(_clause.without(ConceptLiteral(false, d1)).without(ConceptLiteral(false,d2))._with(ConceptLiteral(false,d3)))
	    var resolutionCandidates = definingClauses.getOrElse(d1, Set())++definingClauses.getOrElse(d2, Set())
	    var resolvents = HashSet[ConceptClause]()
	    intermediateDefinitions.flatMap(resolutionRule.getDerivations(_, resolutionCandidates)).foreach{ d=>
	      logger.trace(s"  -  Derivation:   ${d}")
	      d.conclusions.foreach{ resolvents.add } // updates definingClauses 
	    }
	    resolvents.toSeq.sortBy(_.literals.size).foreach(addClause)
	  } else { 
	    // otherwise we can safely assume that the clause is of the from [-D1, -D2], and can be replaced by the single [-D12]
	    addClause(new ConceptClause(Set(ConceptLiteral(false, d3)), ordering))
	  }

	  // now trigger new rule applications that use new definer
	  (definerUsages.get(d1), definerUsages.get(d2)) match { 
	    case (Some(set1), Some(set2)) => { 

//	  var set1 = definerUsages(d1)
//	  var set2 = definerUsages(d2)

	      // update redundancy information
//	      set1.foreach(cl => if(!allClauses.contains(cl._2)) set1.remove(cl))
//	      set2.foreach(cl => if(!allClauses.contains(cl._2)) set2.remove(cl))
	      
	      logger.debug(s"\nApply number restriction rules...")
	      
	      // def reduce(clause: ConceptClause) = { 
	      //   val der = MinEliminationRule.getDerivations(clause, definingClauses(d3))
	      //   if(der.isEmpty)
	      //     clause
	      //   else { 
	      //     logger.trace(s"  Min-Elimination/Resolution: ${der.head}")
	      //     der.head.conclusions.head
	      //   }
	      // }
	      var nexts = HashSet[ConceptClause]()
	      set1.foreach{ pair1 =>
		val (literal1, clause1) = pair1
	        set2.foreach{ pair2 => 
		  val (literal2, clause2) = pair2
		  numberRestrictionRule.derive(clause1, literal1,
					       clause2, literal2).foreach(nexts.add)
//		  numberRestrictionRule.derive(clause2, literal2,
//					       clause1, literal1).foreach(nexts.add)
		}
	      }
	      nexts.toSeq.sortBy(_.literals.size).foreach(proceed)

	      if(nexts.isEmpty){ 
		// dann brauchen wir auch die Resolventen nicht!
		val remove = definingClauses(d3)
		definingClauses.remove(d3)
		allClauses --= remove
		result --= remove
		posClauses --= remove
		negClauses --= remove
	      }

	      var varSet1 = Set()++set1
	      var varSet2 = Set()++set2
	      while(nexts.size>0){ 
		nexts = HashSet[ConceptClause]()
		val used = varSet1++varSet2

		removeRedundants(d1)
		removeRedundants(d2)
		varSet1 = definerUsages(d1).toSet
		varSet2 = definerUsages(d2).toSet

		val newUsages1 = varSet1--used
		val newUsages2 = varSet2--used


		newUsages1.foreach{ pair1 =>
		  val (literal1, clause1) = pair1
		  varSet2.foreach{ pair2 => 
		    val (literal2, clause2) = pair2
		    numberRestrictionRule.derive(clause1, literal1,
						 clause2, literal2).foreach(nexts.add)
//		    numberRestrictionRule.derive(clause2, literal2,
//						 clause1, literal1).foreach(nexts.add)
		  }
	        }
		newUsages2.foreach{ pair1 =>
		  val (literal1, clause1) = pair1
		  varSet1.foreach{ pair2 => 
		    val (literal2, clause2) = pair2
		    numberRestrictionRule.derive(clause1, literal1,
						 clause2, literal2).foreach(nexts.add)
//		    numberRestrictionRule.derive(clause2, literal2,
//						 clause1, literal1).foreach(nexts.add)
		  }
		}
		nexts.toSeq.sortBy(_.literals.size).foreach(proceed)
	      }
	    }
	    case _ => ;
	  }
	}
      }
    }
  }


  def init(clauses: Set[ConceptClause], forgetConcept: BaseConcept) = { 


    this.forgetConcept = forgetConcept

    ordering = new ConceptLiteralOrdering(Seq(forgetConcept.name))
    val clauseOrdering = new ConceptClauseOrdering(ordering)

    definerFactory = new DefinerFactory(ALCFormulaPreparations, ordering, roleHierarchy)

    subsumptionChecker.setDefinerFactory(definerFactory)
    subsumptionChecker.setRoleHierarchy(roleHierarchy)


    
    // Rules
    ///////////

    resolutionRule = new ResolutionRule(ordering)
    resolutionRule.ignoreInvalid = false
    numberRestrictionRule = new NumberRestrictionRule(roleHierarchy, definerFactory)
    transitivityRule = new TransitivityRule(transitiveRoles, roleHierarchy)

    // Clauses
    ////////////

    result = new TreeSet[ConceptClause]()(clauseOrdering)
    posClauses = new TreeSet[ConceptClause]()(clauseOrdering)
    negClauses = new TreeSet[ConceptClause]()(clauseOrdering)
    allClauses = new TreeSet[ConceptClause]()(clauseOrdering)

    definingClauses = new HashMap[BaseConcept, mutSet[ConceptClause]]() 
		      with MultiMap[BaseConcept, ConceptClause]

    definerUsages = new HashMap[BaseConcept, mutSet[(ConceptLiteral, ConceptClause)]]()
		    with MultiMap[BaseConcept, (ConceptLiteral, ConceptClause)]

    
    clauses.foreach{ cl => addClause(cl) }

    // Transitivity
    ////////////////////
    addTransitivityClauses(clauses)
    //println("Warnung: transitivity aus!")
  }


  def addTransitivityClauses(clauses: Iterable[ConceptClause]) = { 

    var transSubRoles = new HashMap[BaseRole, mutSet[BaseRole]]() with MultiMap[BaseRole, BaseRole]

    roleHierarchy.transitiveRoles.foreach{ role =>
      roleHierarchy.getSuperRoles(role).foreach{ r => 
	transSubRoles.addBinding(r.asInstanceOf[BaseRole], role.asInstanceOf[BaseRole])
      }
    }

    clauses.foreach{ clause =>
      if(clause.literals.size>1)
      clause.literals.foreach{ l => l.concept match { 
	case MaxNumberRestriction(0,
				  r: BaseRole,
				  ConceptComplement(d: BaseConcept)) if transSubRoles.contains(r) =>{ 
	  transSubRoles(r).foreach { role2 =>
            val newDefiner = definerFactory.newDefiner()

	    val clause2 = new ConceptClause(Set(ConceptLiteral(false, newDefiner),
	       				      ConceptLiteral(true,
							     MaxNumberRestriction(0,role2,ConceptComplement(newDefiner)))), ordering)
	    updateTables(clause2)

	    val resolutionCandidates = definingClauses.getOrElse(d, Set())
	    val clause1 = new ConceptClause(Set(ConceptLiteral(false, newDefiner), 
	    					ConceptLiteral(true, d)), ordering)
	    resolutionRule.getDerivations(clause1, 
	    				  resolutionCandidates).flatMap(_.conclusions).foreach(addClause)
	    // <-- have to be added: possible resolution candidates for later
	    // <--- only update: don't want to introduce cycle if not neccessary
	    val clause3 = clause.without(l)._with(ConceptLiteral(true,
								 MaxNumberRestriction(0,
										      role2,
										      ConceptComplement(newDefiner))))
				   
	    addClause(clause3) // <--- has to be added: otherwise no connection to new definer
				    
				    
	  }
	}
	case _ => ;
      }
	
      }
    }
  }

  def addClause(_clause: ConceptClause): Unit = { 

    var clause = _clause

    if(redundant(clause)) { 
      logger.debug(s"New clause ${clause} is subsumed")
      return
    }


    val condensed = subsumptionChecker.condenseClause(clause)
    logger.debug(s"${if(condensed!=clause) "reduction possible: "+clause}")
    clause=condensed

    logger.debug(s"Adding ${clause})")

    updateTables(clause)
  
    // add new clause
    clause.literals.head match { 
      case ConceptLiteral(true, b: BaseConcept) if b==forgetConcept => posClauses.add(clause)
      case ConceptLiteral(false, b: BaseConcept) if b==forgetConcept => negClauses.add(clause)
      case _ => logger.debug(s"new result clause!"); result.add(clause)
    }
  }

  def updateTables(clause: ConceptClause) = { 
    // update tables
    clause.literals.foreach{ literal => 
      literal match { 
      case ConceptLiteral(false, d: BaseConcept) if isDefiner(d) => definingClauses.addBinding(d, clause)
      case ConceptLiteral(true, MaxNumberRestriction(_,_,ConceptComplement(d: BaseConcept))) =>
	definerUsages.addBinding(d, (literal, clause))
      case ConceptLiteral(true, MinNumberRestriction(_,_,d: BaseConcept)) =>
	definerUsages.addBinding(d, (literal, clause))
      case _ => 
    }}


    reduceClauseSets(clause)
    allClauses.add(clause)
  }


  def redundant(clause: ConceptClause): Boolean = 
    allClauses.exists(subsumptionChecker.subsumes(_, clause))

  /**
   * Situation: redundancy elimination is used on allClauses, posClauses, negClauses and result,
   * but not on the maps for the definers, since iterating over them would take too long.
   * Instead, we follow a lazy strategy, and only apply redundancy elimination when necessary,
   * by building the intersection with allClauses.
   */
  def removeRedundants(d: BaseConcept) = { 
    
    if(definingClauses.contains(d)){ 
      var old = definingClauses(d).size
      definingClauses.put(d, HashSet[ConceptClause]()++definingClauses(d).filter(allClauses))
      // if(old!=definingClauses(d).size)
      // 	println("removed "+(old-definingClauses(d).size))
    }
    if(definerUsages.contains(d)){ 
      var old = definerUsages(d).size
      definerUsages.put(d, HashSet[(ConceptLiteral, ConceptClause)]()++definerUsages(d).filter(a => allClauses(a._2)))
      // if(old!=definerUsages(d).size)
      // 	println("removed "+(old-definerUsages(d).size))

    }
  }
    

  def reduceClauseSets(clause: ConceptClause) = {
    val filtered = allClauses.filter(subsumptionChecker.subsumes(clause,_))
    logger.trace(s"subsumed: ${filtered}")
    allClauses --= filtered
    posClauses --= filtered
    negClauses --= filtered
    result --= filtered
    
    if(clause.literals.size==1) { 
      val l = clause.literals.head
      l match { 
	case ConceptLiteral(false, b: BaseConcept) if isDefiner(b) && definerUsages.contains(b) => { 
	  val usages = definerUsages(b)
	  usages.foreach{ pair =>
	    val (literal2, clause2) = pair
	    val derived = MinEliminationRule.derive(clause2, literal2, clause, l)
	    if(!derived.isEmpty) { 
	      logger.trace(s"Minimised from ${clause2} and ${clause}")
	      result -= clause2
	      posClauses -= clause2
	      negClauses -= clause2
	      allClauses -= clause2
	      if(!allClauses.contains(derived.head))
		addClause(derived.head) // avoids endless loop?
	      assert(derived.size==1)
	    }
	  }
	  result --= definingClauses.getOrElse(b, Set[ConceptClause]())
	  definingClauses.remove(b)
	  definerUsages.remove(b)
	}
	case _ => ;
      }
    }
  }

  def notifyClauseAddition(clauses: Set[ConceptClause]) = { 
    logger.debug(s"Ignore new clause notification: ${clauses}")
    // will come from definer factory
    //clauses.foreach(newClauses.add)
  }

  def isDefiner(concept: BaseConcept): Boolean = 
    concept.name.startsWith("_D")
}

