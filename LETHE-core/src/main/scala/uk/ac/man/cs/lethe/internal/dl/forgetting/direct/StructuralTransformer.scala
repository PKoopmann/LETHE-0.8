package uk.ac.man.cs.lethe.internal.dl.forgetting.direct


import uk.ac.man.cs.lethe.internal.dl.datatypes._ 


/**
 * Apply structural transformations to axioms in order to keep conjunctions small.
 *
 * @param nonBaseSymbols: these symbols will be kept outside of the transformations 
 */
class StructuralTransformer(nonBaseSymbols: Set[String]) { 
  
  var mapping: Map[String, Concept] = Map()


  def transform(ont: Ontology): Ontology = new Ontology(transform(ont.tbox), transform(ont.abox))

  def transform(tbox: TBox): TBox = TBox(tbox.axioms.map(transform))

  def transform(abox: ABox): ABox = ABox(abox.assertions.map(transform))

  def transform(axiom: Axiom): Axiom = axiom match { 
    case Subsumption(c1, c2) => Subsumption(transform(c1), transform(c2))
    case ConceptEquivalence(c1, c2) => ConceptEquivalence(transform(c1), transform(c2))
  }
  
  def transform(assertion: Assertion): Assertion = assertion match { 
    case ConceptAssertion(c, i) => ConceptAssertion(transform(c), i)
    case r: RoleAssertion => r
    case DisjunctiveConceptAssertion(as) 
      => DisjunctiveConceptAssertion(as.map(ca => transform(ca).asInstanceOf[ConceptAssertion]))
  }

  def transform(concept: Concept): Concept = concept match { 
    case TopConcept => TopConcept
    case BottomConcept => BottomConcept
    case b: BaseConcept => b
    case ConceptComplement(c) => ConceptComplement(transform(c))
    case c: ConceptConjunction => transform(c)
    case d: ConceptDisjunction => transform(d)
    case ExistentialRoleRestriction(r,c) => 
      if(c.signature.forall(!nonBaseSymbols(_))) {       
	if(r.signature.forall(!nonBaseSymbols(_))) { 
	  val label = newLabel()
	  mapping = mapping.updated(label, ExistentialRoleRestriction(r,c))
	  BaseConcept(label)
	} else { 
	  val label = newLabel()
	  mapping = mapping.updated(label, c)
	  ExistentialRoleRestriction(r, BaseConcept(label))
	}
      } else
	ExistentialRoleRestriction(r, transform(c))

    case UniversalRoleRestriction(r, c) =>       
      if(c.signature.forall(!nonBaseSymbols(_))) {       
	if(r.signature.forall(!nonBaseSymbols(_))) { 
	  val label = newLabel()
	  mapping = mapping.updated(label, UniversalRoleRestriction(r,c))
	  BaseConcept(label)
	} else { 
	  val label = newLabel()
	  mapping = mapping.updated(label, c)
	  UniversalRoleRestriction(r, BaseConcept(label))
	}
      } else
	UniversalRoleRestriction(r, transform(c))
    case MinNumberRestriction(n,r,c) => 
      if(c.signature.forall(!nonBaseSymbols(_))) {       
	if(r.signature.forall(!nonBaseSymbols(_))) { 
	  val label = newLabel()
	  mapping = mapping.updated(label, MinNumberRestriction(n, r, c))
	  BaseConcept(label)
	} else { 
	  val label = newLabel()
	  mapping = mapping.updated(label, c)
	  MinNumberRestriction(n, r, BaseConcept(label))
	}
      } else
	MinNumberRestriction(n, r, transform(c))
    case MaxNumberRestriction(n,r,c) => 
      if(c.signature.forall(!nonBaseSymbols(_))) {       
	if(r.signature.forall(!nonBaseSymbols(_))) { 
	  val label = newLabel()
	  mapping = mapping.updated(label, MaxNumberRestriction(n, r, c))
	  BaseConcept(label)
	} else { 
	  val label = newLabel()
	  mapping = mapping.updated(label, c)
	  MaxNumberRestriction(n, r, BaseConcept(label))
	}
      } else
	MaxNumberRestriction(n, r, transform(c))
  }

  def transform(conjunction: ConceptConjunction): Concept = { 
    val b_NB = conjunction.conjuncts.groupBy{ conj =>
      if(conj.signature.exists(nonBaseSymbols))
	"NB"
      else
	"B"
    }

    if(!b_NB.contains("B"))
      ConceptConjunction(conjunction.conjuncts.map(transform))
    else { 
      val label = newLabel()
      mapping = mapping.updated(label, ConceptConjunction(b_NB("B")))
      if(!b_NB.contains("NB"))
	BaseConcept(label)
      else
	ConceptConjunction(b_NB("NB").map(transform)+BaseConcept(label))
    }
  }


  def transform(disjunction: ConceptDisjunction): Concept = { 
    val b_NB = disjunction.disjuncts.groupBy{ disj =>
      if(disj.signature.exists(nonBaseSymbols))
	"NB"
      else
	"B"
    }

    var result = if(!b_NB.contains("B"))
                   ConceptDisjunction(disjunction.disjuncts.map(transform))
		 else { 
		   val label = newLabel()
		   mapping = mapping.updated(label, ConceptDisjunction(b_NB("B")))
		   if(!b_NB.contains("NB"))
		     BaseConcept(label)
		   else
		     ConceptDisjunction(b_NB("NB").map(transform)+BaseConcept(label))
		 }

    result match { 
      case b: BaseConcept => b
      case ConceptDisjunction(ds) => { 
    	val grouped = ds.groupBy{ _ match { 
    	  case ExistentialRoleRestriction(r, _) => r
    	  case c => c
    	}}
    	val reducedDs = grouped.keySet.map{ _ match { 
    	  case r: Role => { 
    	    val fillers = grouped(r).map(_ match { 
    	      case ExistentialRoleRestriction(_, f) => f
    	    })
    	    ExistentialRoleRestriction(r, ConceptDisjunction(fillers))
    	  }
    	  case c: Concept => c
    	}}.toSet
    	ConceptDisjunction(reducedDs)
      }
    }
  }

  /**
   * group clauses when possible, and apply further structural transformations
   */
  def transform(clauses: Set[ConceptClause], ordering: Ordering[ConceptLiteral], interestingDefiners: Set[String]): Set[ConceptClause] = { 
    var transformed = Set[ConceptClause]()
    var left = clauses

    def definer(name: String) =  name.startsWith("_D")

    def excluded(literal: ConceptLiteral): Boolean = literal.concept match { 
      case BaseConcept(name) => definer(name) || nonBaseSymbols(name)
      case ExistentialRoleRestriction(_, dc: DerivationCandidate) => true
      case ExistentialRoleRestriction(r, BaseConcept(name)) => 
	r.signature.exists(nonBaseSymbols) || interestingDefiners(name)
      case UniversalRoleRestriction(r, dc: DerivationCandidate) => true
      case UniversalRoleRestriction(r, BaseConcept(name)) => 
	r.signature.exists(nonBaseSymbols) || interestingDefiners(name)
      case _ => ; true //existential or universal restriction
    }
    def exclude(clause: ConceptClause): Set[ConceptLiteral] = clause.literals.filter(excluded)

    while(!left.isEmpty){ 
      val next = left.head
      val head = exclude(next)
      val group = left.filter(exclude(_)==head)
      if(group.size==1){ 
	left-=next
	transformed+=next
      } else { 
	val rests = group.map(cl => ConceptDisjunction((cl.literals--exclude(cl)).toSet[Concept]))
	val concept = ConceptConjunction(rests.toSet[Concept])
	val label = newLabel()
	mapping = mapping.updated(label, concept)
        left --= group
        transformed += new ConceptClause(head+ConceptLiteral(true, BaseConcept(label)), ordering)
      }
    }

    transformed
  }


  var counter = 0

  def newLabel() = { 
    counter += 1
    "_X"+counter.toString
  }


  def transformBack(ont: Ontology): Ontology = { 
    new Ontology(transformBack(ont.tbox), transformBack(ont.abox))
  }

  def transformBack(tbox: TBox): TBox = TBox(tbox.axioms.map(transformBack))

  def transformBack(abox: ABox): ABox = ABox(abox.assertions.map(transformBack))

  def transformBack(dlStatement: DLStatement): DLStatement = dlStatement match { 
    case a: Axiom => transformBack(a)
    case a: Assertion => transformBack(a)
  }

  def transformBack(axiom: Axiom): Axiom = axiom match { 
    case Subsumption(c1, c2) => Subsumption(transformBack(c1), transformBack(c2))
    case ConceptEquivalence(c1, c2) => ConceptEquivalence(transformBack(c1), transformBack(c2))
  }
  
  def transformBack(assertion: Assertion): Assertion = assertion match { 
    case ConceptAssertion(c, i) => ConceptAssertion(transformBack(c), i)
    case r: RoleAssertion => r
  }

  def transformBack(concept: Concept): Concept = concept match { 
    case l: ConceptLiteral => transformBack(l.convertBack)
    case TopConcept => TopConcept
    case BottomConcept => BottomConcept
    case BaseConcept(n) if mapping.contains(n) => transformBack(mapping(n))
    case b: BaseConcept => b
    case ConceptComplement(c) => ConceptComplement(transformBack(c))
    case ConceptConjunction(cs) => ConceptConjunction(cs.map(transformBack))
    case ConceptDisjunction(ds) => ConceptDisjunction(ds.map(transformBack))
    case ExistentialRoleRestriction(r,c) => ExistentialRoleRestriction(r, transformBack(c))
    case UniversalRoleRestriction(r, c) => UniversalRoleRestriction(r, transformBack(c))
    case MinNumberRestriction(n,r,c) => MinNumberRestriction(n,r,transformBack(c))
    case MaxNumberRestriction(n,r,c) => MaxNumberRestriction(n,r,transformBack(c))
  }

  // def transformBack(conjunction: ConceptConjunction): Concept = { 
  //   if(conjunction.conjuncts.forall(_.isInstanceOf[ConceptDisjunction])){ 
  //     val diss = conjunction.conjuncts.map(_.asInstanceOf[ConceptDisjunction])
  //     var overlap = diss.toSeq(0).disjuncts
  //     diss.foreach { dis =>
  // 	overlap = overlap.filter(dis.disjuncts.contains)
  //     }
  //     if(overlap.isEmpty)
  // 	DLHelpers.conjunction(conjunction.conjuncts.map(transformBack))
  //     else ConceptDisjunction(overlap.map(transformBack) + ConceptConjunction(
  // 	diss.map(dis =>transformBack(ConceptDisjunction(dis.disjuncts--overlap)))))
  //   }
  //   else
  //     DLHelpers.conjunction(conjunction.conjuncts.map(transformBack))
  // }

  def transformBack(clause: ConceptClause, ordering: Ordering[ConceptLiteral]): Set[ConceptClause] = {
    
    def safe(concept: Concept) = concept match { 
      case ConceptDisjunction(ds) => ds.forall(_.isInstanceOf[ConceptLiteral])
      case _ => false
    }

    clause.literals.collectFirst(l => l.concept match { 
      case BaseConcept(name) if mapping.contains(name) => l
    }) match { 
      case None => Set(clause)
      case Some(literal) => mapping(literal.concept.asInstanceOf[BaseConcept].name) match { 
	case ConceptConjunction(cs) if cs.forall(safe) => 
	  cs.flatMap(_ match { 
	    case ConceptDisjunction(ds) => 
	      val literals = ds.map(_.asInstanceOf[ConceptLiteral])
	      transformBack(new ConceptClause(clause.literals-literal++literals, ordering), ordering)
	  })
	case _ => Set(clause) // don't transform back yet if no conjunction of literals
      }
    }
  }
}
