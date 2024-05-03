package uk.ac.man.cs.lethe.internal.dl.abduction

import uk.ac.man.cs.lethe.internal.dl.datatypes.extended.{ConjunctiveAssertion, ConjunctiveDLStatement, DisjunctiveAssertion, DisjunctiveDLStatement, NegatedRoleAssertion}
import uk.ac.man.cs.lethe.internal.dl.datatypes.{Assertion, BaseConcept, BaseRole, BottomConcept, CheapSimplifier, Concept, ConceptAssertion, ConceptComplement, ConceptConjunction, ConceptDisjunction, ConceptEquivalence, DLHelpers, DLStatement, ExistentialRoleRestriction, Individual, Ontology, OntologyBeautifier, RoleAssertion, Subsumption, TopConcept, TopRole, UniversalRoleRestriction}
import uk.ac.man.cs.lethe.internal.tools.Cancelable
import uk.ac.man.cs.lethe.internal.tools.formatting.SimpleDLFormatter

object KBNegator extends Cancelable {

  var individual: Individual = Individual("a") // individual name used for negated TBox axioms

  /**
   * This negation operator may result in a mixed disjunction of TBox axioms and assertions.
   * assertions with universal roles may be transformed to TBox axioms
   */
  def negate(ontology: Ontology): DLStatement =
    dnf(DisjunctiveDLStatement(ontology.statements.map(negate).map(dnf).toSet))

  def pullOutCIsAxiom(subsumption: Subsumption): DLStatement = {
    checkCanceled

    println("Pulling out CIsAxiom from "+SimpleDLFormatter.format(subsumption))
    subsumption match {
      case Subsumption(TopConcept, rhs) =>
        // println("Before pulling out: "+statement)
        //pullOutCIsConcept2(pullOutCIsConcept(rhs)) match { // <--- TODO document properly - we are now not generating complete solutions anymore!
        val c1 = pullOutCIsConcept(rhs)
        println("C1: "+SimpleDLFormatter.format(c1))
        val c2 = simplify(c1)
        println("C2: "+SimpleDLFormatter.format(c2))
       // System.exit(1)
        val c3 = cnf(c2)
        println("C3: "+SimpleDLFormatter.format(c3))
        c3 match {
          case UniversalRoleRestriction(TopRole,c) => pullOutCIsAxiom(Subsumption(TopConcept, c))
        /*  // NOW ASSUMING A DNF instead
          case ConceptDisjunction(ds) =>
            DisjunctiveDLStatement(ds.map(concept =>
              pullOutCIsAxiom(Subsumption(TopConcept,concept))))
          case ConceptConjunction(ds) =>
            val elements = ds.partition(_ match {
              case UniversalRoleRestriction(TopRole, _) => true
              case _ => false
            })
            ConjunctiveDLStatement(
              (elements._1.map(c =>
                Subsumption(TopConcept,c.asInstanceOf[UniversalRoleRestriction].filler))
              + Subsumption(TopConcept, ConceptConjunction(elements._2))).toSet[DLStatement]
            )*/
          case ConceptConjunction(cs) => // if cs.exists(universalTopRole) =>
            ConjunctiveDLStatement(
              cs.map(Subsumption(TopConcept,_)).map(pullOutCIsAxiom))
          //            cs.filter(universalTopRole).map(Subsumption(TopConcept,_)).map(pullOutCIsAxiom)++
          //              Set(pullOutCIsAxiom(Subsumption(TopConcept,ConceptConjunction(cs.filterNot(universalTopRole))))))
          case ConceptDisjunction(ds) if ds.exists(universalTopRole) =>
            DisjunctiveDLStatement(
              ds.filter(universalTopRole).map(Subsumption(TopConcept,_)).map(pullOutCIsAxiom) ++
                Set(pullOutCIsAxiom(Subsumption(TopConcept, ConceptDisjunction(ds.filterNot(universalTopRole))))))


          case other => OntologyBeautifier.nice(Subsumption(TopConcept,other))
        }
      case other => throw new AssertionError("Unexpected: "+other)
    }
  }

  def simplify(concept: Concept): Concept = concept match {
    case ConceptDisjunction(ds) =>
      val newDs = ds.map(simplify)
      val topStuff = newDs.groupBy(_ match {
        case ConceptConjunction(cs) => cs.find(_ match {
          case UniversalRoleRestriction(TopRole,c) => true
          case _ => false
        }) match {
          case Some(c) => c
          case _ => TopConcept
        }
        case _ => TopConcept
      })
//      if(topStuff.size==1){
//        return DLHelpers.disjunction(newDs)
 //     } else {
      val joined = topStuff.keys.map { key: Concept =>
          DLHelpers.conjunction(Set(key, DLHelpers.disjunction(topStuff(key).map(_ match {
            case ConceptConjunction(cs) => DLHelpers.conjunction(cs - key)
            case other => other
          }))))
      }
      DLHelpers.disjunction(joined)
    case ConceptConjunction(cs) => ConceptConjunction(cs.map(simplify))
    case other => other

  }

  def dnf(statement: DLStatement): DLStatement = {
    checkCanceled
    println("Transforming to DNF: "+SimpleDLFormatter.format(statement))
    statement match {
      case ConjunctiveDLStatement(cs) =>
        val cs2 = cs.map(dnf)
        if (cs2.exists(_.isInstanceOf[ConjunctiveDLStatement]))
          dnf(ConjunctiveDLStatement(cs2.flatMap(_ match {
            case ConjunctiveDLStatement(cs3) => cs3
            case other => Set(other)
          })))
        else if (cs2.exists(_.isInstanceOf[DisjunctiveDLStatement])) {
          val disjunction = cs2.find(_.isInstanceOf[DisjunctiveDLStatement]).get.asInstanceOf[DisjunctiveDLStatement]
          val other = cs2.-(disjunction)
          dnf(DisjunctiveDLStatement(disjunction.statements.map(c => ConjunctiveDLStatement(other ++ Set(c)))))
        } else
          ConjunctiveDLStatement(cs2)
      case DisjunctiveDLStatement(ds) =>
        var ds2 = ds.map(dnf)
        ds2 = ds2.filterNot { c =>
          c.isInstanceOf[ConjunctiveDLStatement] &&
            ds2.exists(d => d.isInstanceOf[ConjunctiveDLStatement] &&
              !d.equals(c) && d.asInstanceOf[ConjunctiveDLStatement].statements.forall(c.asInstanceOf[ConjunctiveDLStatement].statements))
        }
        if (ds2.exists(_.isInstanceOf[DisjunctiveDLStatement]))
          dnf(DisjunctiveDLStatement(ds2.flatMap(_ match {
            case DisjunctiveDLStatement(ds3) => ds3
            case other => Set(other)
          })))
        else
          DisjunctiveDLStatement(ds2)
      case other =>
        other
    }
  }

  var inc2 = ""

  def cnf(concept: Concept): Concept = {
    checkCanceled
    println(inc2+"Computing the CNF of "+SimpleDLFormatter.format(concept))
    inc2+=" "
    val result = concept match {
      case ConceptDisjunction(ds) =>
        val ds2 = ds.map(cnf)
        if(ds2.exists(_.isInstanceOf[ConceptDisjunction]))
          cnf(ConceptDisjunction(ds2.flatMap(_ match {
            case ConceptDisjunction(ds3) => ds3
            case other => Set(other)
          })))
        else if(ds2.exists(_.isInstanceOf[ConceptConjunction])) {
          val conjunction = ds2.find(_.isInstanceOf[ConceptConjunction]).get.asInstanceOf[ConceptConjunction]
          val other = ds2.-(conjunction)
          cnf(ConceptConjunction(conjunction.conjuncts.map(c => ConceptDisjunction(other ++ Set(c)))))
        } else
          ConceptDisjunction(ds2)
      case ConceptConjunction(cs) =>
        val cs2 = cs.map(cnf)
        if(cs2.exists(_.isInstanceOf[ConceptConjunction]))
          cnf(ConceptConjunction(cs2.flatMap(_ match {
            case ConceptConjunction(ds3) => ds3
            case other => Set(other)
          })))
        else
          ConceptConjunction(cs2)
      case other =>
        other
    }
    inc2 = inc2.substring(0,inc2.size-1)
    println(inc2+"CNF done")

    result
  }

  var inc=""

  def needsPulling(concept: Concept): Boolean = concept match {
    case UniversalRoleRestriction(TopRole,_) => true
    case _ => (concept.subConcepts).exists(x => !x._1.equals(concept) && needsPulling(x._1))
  }

  /**
   * Required form to create hypotheses of the simpler (but not complete) form:
   * 1. no top role anywhere
   * 2. value restriction with top role
   * 3. conjunction in which each conjunct satisifes the above 2 conditions
   * 4. disjunction, where disjuncts satisfy the above 3 conditions
   * @param concept
   * @return
   */
  def requiredForm(concept: Concept): Boolean = {

    def case12(concept: Concept) = concept match {
      case concept if !needsPulling(concept) => true
      case UniversalRoleRestriction(TopRole, concept) if !needsPulling(concept) => true
      case _ => false
    }
    def case3(concept: Concept) = concept match {
      case ConceptConjunction(cs) => cs.forall(case12)
      case _ => false
    }
    def case4(concept: Concept): Boolean = concept match {
      case ConceptDisjunction(ds) => ds.forall(x => case12(x) || case3(x))
      case _ => false
    }
    case12(concept) || case3(concept) || case4(concept)
  }

  def pullOutCIsConcept2(concept: Concept): Concept = {
    var currentConcept = concept
    var finished = false // TODO make this proper
    while(!finished && !requiredForm(currentConcept)){
      println("STEP - current concept: "+SimpleDLFormatter.format(currentConcept))
      currentConcept match {
        case ConceptDisjunction(ds) => {
          /// A u (forall TopRole.B n C)  ===> (forall TopRole.B u A) n (A u C)
          val badDisjunct = ds.find(_ match {
            case ConceptConjunction(cs) => cs.exists(needsPulling)
          })
          badDisjunct match {
            case Some(conjunction: ConceptConjunction) =>
              val rest = ds - conjunction
              currentConcept = DLHelpers.conjunction(conjunction.conjuncts.map(c => DLHelpers.disjunction(rest + c)))
            case _ => finished=true
          }
        }
        case ConceptConjunction(cs) => {
          // A n (forall TopRole.B u C) ===> (A n forall TopRole.B) u (A n C)
          val badConjunct = cs.find(_ match {
            case ConceptDisjunction(ds) => ds.exists(needsPulling)
          })
          badConjunct match {
            case Some(disjunction: ConceptDisjunction) =>
              val rest = cs - disjunction
              currentConcept = DLHelpers.disjunction(disjunction.disjuncts.map(c => DLHelpers.conjunction(rest + c)))
            case _ => finished=true
          }
        }
      }
    }
    println("Result of pull out CIs concept 2: "+SimpleDLFormatter.format(currentConcept))
    currentConcept
  }

  def pullOutCIsConcept(concept: Concept): Concept = {
    checkCanceled
    println(inc+"pullOutCIsConcept: "+SimpleDLFormatter.format(concept))
    inc+=" "
    val result = DLHelpers.simplify(concept) match {
      case TopConcept => TopConcept
      case BottomConcept => BottomConcept
      case b: BaseConcept => b
      case ConceptComplement(b: BaseConcept) => ConceptComplement(b)
      case ExistentialRoleRestriction(role, c) if universalTopRole(c) =>
        DLHelpers.conjunction(Set(ExistentialRoleRestriction(role, TopConcept), pullOutCIsConcept(c)))
      case ExistentialRoleRestriction(role, c) =>
      pullOutCIsConcept(c) match {
        case ConceptConjunction(cs)  if cs.exists (universalTopRole) =>
          DLHelpers.conjunction(Set(DLHelpers.conjunction(cs.filter(universalTopRole).map(pullOutCIsConcept)),
          ExistentialRoleRestriction (role, DLHelpers.conjunction (cs.filterNot (universalTopRole).map (pullOutCIsConcept)))))
        case ConceptDisjunction(ds) =>
          DLHelpers.disjunction(ds.map(ExistentialRoleRestriction(role, _)).map(pullOutCIsConcept))
        case other => ExistentialRoleRestriction(role, other)
      }
      case UniversalRoleRestriction(role, c) if universalTopRole(c) =>
        DLHelpers.conjunction(Set(UniversalRoleRestriction(role, BottomConcept), pullOutCIsConcept(c)))
      case UniversalRoleRestriction(role, c) =>
        pullOutCIsConcept(c) match {
          case ConceptConjunction(cs) if cs.exists(universalTopRole) =>
            DLHelpers.disjunction(Set(DLHelpers.disjunction(cs.filter(universalTopRole).map(pullOutCIsConcept)),
              UniversalRoleRestriction(role, DLHelpers.disjunction(cs.filterNot(universalTopRole).map(pullOutCIsConcept))))
            )
          case ConceptConjunction(cs) =>
        DLHelpers.conjunction(cs.map(UniversalRoleRestriction(role, _) ).map(pullOutCIsConcept))
          case other => UniversalRoleRestriction(role,other)
        }
     // case ExistentialRoleRestriction(role, ConceptConjunction(cs)) if cs.exists(universalTopRole) =>
     //   DLHelpers.conjunction(Set(DLHelpers.conjunction(cs.filter(universalTopRole).map(pullOutCIsConcept)),
     //     ExistentialRoleRestriction(role, DLHelpers.conjunction(cs.filterNot(universalTopRole).map(pullOutCIsConcept))))
     //   )
     // case UniversalRoleRestriction(role, ConceptConjunction(cs)) if cs.exists(universalTopRole) =>
     //   DLHelpers.disjunction(Set(DLHelpers.disjunction(cs.filter(universalTopRole).map(pullOutCIsConcept)),
     //     UniversalRoleRestriction(role, DLHelpers.disjunction(cs.filterNot(universalTopRole).map(pullOutCIsConcept))))
     //   )
     // case ExistentialRoleRestriction(role, ConceptDisjunction(ds)) =>
     //   DLHelpers.disjunction(ds.map(ExistentialRoleRestriction(role,_)).map(pullOutCIsConcept))
    //  case UniversalRoleRestriction(role, ConceptConjunction(cs)) =>
    //    DLHelpers.conjunction(cs.map(UniversalRoleRestriction(role, _)).map(pullOutCIsConcept))
   //   case other: ExistentialRoleRestriction => other // over concept name or negated concept name
   //   case other: UniversalRoleRestriction => other // over concept name or negated concept name
      case ConceptConjunction(cs) if cs.size==1 =>
        pullOutCIsConcept(cs.head)
      case ConceptConjunction(cs) =>
        DLHelpers.conjunction(cs.map(pullOutCIsConcept))
      case ConceptDisjunction(ds) =>
        DLHelpers.disjunction(ds.map(pullOutCIsConcept))
    }
    inc = inc.substring(0,inc.size-1)
    println(inc+"Pulling out gave: "+SimpleDLFormatter.format(result))
    result
  }

  def universalTopRole(concept: Concept) = concept match {
    case UniversalRoleRestriction(TopRole, _) => true
    case _ => false
  }

  def negate(statement: DLStatement): DLStatement = {
    checkCanceled
    println("negate "+SimpleDLFormatter.format(statement))
    statement match {
      case ConceptAssertion(ConceptDisjunction(cs), a: Individual) =>
        negate(DisjunctiveDLStatement(cs.map(ConceptAssertion(_,a))))
      case ConceptAssertion(ConceptConjunction(cs), a: Individual) =>
        negate(ConjunctiveDLStatement(cs.map(ConceptAssertion(_,a))))
      case ConceptAssertion(ExistentialRoleRestriction(TopRole, concept), _) =>
       // println("Pulling out...")
        println("NNF")
        val nnf = DLHelpers.nnf(Subsumption(concept, BottomConcept))
        println("pullout")
        val pulledOut = nnf.map(pullOutCIsAxiom)
        val result = ConjunctiveDLStatement(pulledOut)
        println("...pulling out done.")
        result
        //ConjunctiveDLStatement(nnf.toSet[DLStatement])
      case DisjunctiveAssertion(disjuncts) =>
        negate(DisjunctiveDLStatement(disjuncts.toSet[DLStatement]))
      case DisjunctiveDLStatement(disjuncts) =>
        ConjunctiveDLStatement(disjuncts.map(negate))
      case ConjunctiveAssertion(conjuncts) =>
        negate(ConjunctiveDLStatement(conjuncts.toSet[DLStatement]))
      case ConjunctiveDLStatement(conjuncts) =>
        DisjunctiveDLStatement(conjuncts.map(negate))
      case other =>
        negate2Assertion(other)
    }
  }

/*  def splitDisjunctions(concept: Concept) = {
    case ExistentialRoleRestriction(r, ConceptDisjunction(ds)) =>
      ConceptDisjunction(ds.map(ExistentialRoleRestriction(r,_)))
    case
  }*/

  /**
   * This negation operator always results in a disjunction of assertions.
   */
  def negate2Assertion(ontology: Ontology): Assertion =
    negate2Assertion(ontology.statements)

  def negate2Assertion(statements: Iterable[DLStatement]): Assertion =
    DisjunctiveAssertion(statements.map(negate2Assertion).toSet)

  def negate2Assertion(statement: DLStatement): Assertion = {
    checkCanceled
    statement match {
      case ConceptAssertion(c, a) =>
        ConceptAssertion(CheapSimplifier.simplify(ConceptComplement(c)), a)
      case RoleAssertion(r: BaseRole, a, b) => NegatedRoleAssertion(r, a, b)
      case NegatedRoleAssertion(r, a, b) => RoleAssertion(r, a, b)
      case Subsumption(c, d) =>
        ConceptAssertion(CheapSimplifier.simplify(ExistentialRoleRestriction(TopRole,
          ConceptConjunction(Set(c, ConceptComplement(d))))), individual)
      case DisjunctiveAssertion(disjuncts) =>
        ConjunctiveAssertion(disjuncts.map(negate2Assertion))
      case ConceptEquivalence(c1, c2) =>
        DisjunctiveAssertion(Set(negate2Assertion(Subsumption(c1,c2)), negate2Assertion(Subsumption(c2,c1))))
    }
  }

}
