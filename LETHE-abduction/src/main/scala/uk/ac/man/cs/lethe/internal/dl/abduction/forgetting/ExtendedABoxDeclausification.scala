package uk.ac.man.cs.lethe.internal.dl.abduction.forgetting

import uk.ac.man.cs.lethe.internal.dl.abduction.forgetting.ExtendedABoxClausification.logger
import uk.ac.man.cs.lethe.internal.dl.datatypes.extended.{ConceptVariable, DisjunctiveAssertion, ExtendedABoxClause, GreatestFixpoint, NegatedRoleAssertion}
import uk.ac.man.cs.lethe.internal.dl.datatypes.{Assertion, BaseConcept, BaseRole, Concept, ConceptAssertion, ConceptComplement, ConceptConjunction, ConceptDisjunction, DLStatement, ExistentialRoleRestriction, InverseRole, NominalSet, Subsumption, TopConcept, TopRole, UniversalRoleRestriction}
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.{ALCFormulaPreparations, ConceptClause, ConceptLiteral, SimpleDefinerEliminator}

import scala.collection.mutable

class ExtendedABoxDeclausification {

  var negDefiner2Fresh = Map[BaseConcept, BaseConcept]()
  var fresh2NegDefiner = Map[BaseConcept, BaseConcept]()

  def declausify(aboxClauses: Set[ExtendedABoxClause],
                 definitionClauses: Set[ConceptClause])
  : Set[DLStatement] = {

    var additionalSubsumptions =
      new mutable.HashMap[BaseConcept, mutable.Set[Concept]]()
        with mutable.MultiMap[BaseConcept, Concept]

    definitionClauses.foreach(cl =>
      cl.literals.foreach(lit => lit match {
        case ConceptLiteral(true, UniversalRoleRestriction(r, d: BaseConcept)) =>
          val negDefiner = def4neg(d)
          val innerDisjuncts = (cl.literals-lit).map(_ match {
            case ConceptLiteral(false, d: BaseConcept)
              if ALCFormulaPreparations.isDefiner(d) => def4neg(d)
            case other => other.convertBack
          })
          val inner = ConceptDisjunction(innerDisjuncts)
          val switchedUniv = UniversalRoleRestriction(InverseRole(r), inner)
          additionalSubsumptions.addBinding(negDefiner, switchedUniv)
        case ConceptLiteral(false, d: BaseConcept) if ALCFormulaPreparations.isDefiner(d) =>
          additionalSubsumptions.addBinding(d, cl.without(lit).convertBack)
        case other => ; //do nothing
      })
    )

    aboxClauses.foreach{cl =>
      cl.literals.keys.foreach{ind =>
        cl.literals(ind).literals.map{l => l match {
          case ConceptLiteral(false, d: BaseConcept) if ALCFormulaPreparations.isDefiner(d) => {
            val clauseWithout = cl.without(ind, l)

            additionalSubsumptions.addBinding(d,
              ConceptDisjunction(
                asDisjuncts(clauseWithout) +
                ConceptComplement(new NominalSet(ind)),
              ))
          }
          case ConceptLiteral(true, UniversalRoleRestriction(r, d: BaseConcept)) =>
            additionalSubsumptions.addBinding(def4neg(d),
              UniversalRoleRestriction(InverseRole(r), ConceptDisjunction(
                asDisjuncts(cl.without(ind,l)) +
                ConceptComplement(new NominalSet(ind))
              ))
            )
          case other => // do nothing
        } }
      }
    }

    var currentABoxClauses = aboxClauses.map(replaceNegativeDefiners)

    logger.trace("current ABox clauses: ")
    logger.trace(s"${currentABoxClauses.mkString("\n")}")


    logger.trace("Additional Subsumptions:")
    logger.trace(s"${additionalSubsumptions.mkString("\n")}")

    //import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.SimpleDefinerEliminator._

    val definerEliminator = SimpleDefinerEliminator

    //var definerSubs = definerEliminator.groupSubsumptions(definitionClauses.map(definerEliminator.toSubsumption))
    //definerSubs = definerSubs.filter(s => definerEliminator.isDefiner(s.subsumer))

    //definerSubs ++=
    var definerSubs =
      additionalSubsumptions.map(pair =>
        Subsumption(pair._1, ConceptConjunction(pair._2.toSet)))

    logger.trace("Grouped definer definitions: ")
    logger.trace(s"${definerSubs.mkString("\n")}")

    var result = currentABoxClauses.map(toDisjunctiveAssertion).toSet[DLStatement]

    logger.trace("current result:")
    logger.trace(s"${result.mkString("\n")}")

    var cyclicDefinitions = Set[Subsumption]()

    var cyclicDefiners = Set[Concept]()

    while(!definerSubs.isEmpty){
      val next = definerSubs.head
      definerSubs = definerSubs.tail
      next match {
        // cyclic definer
        case Subsumption(d,c) if c.atomicConcepts.contains(d.toString) && definerEliminator.isDefiner(d) => {
          assert(d.toString.startsWith("_D"))
          logger.trace("Cyclic definer: "+d.toString)

          val replacement =
            if(definerEliminator.tautologicFixpoint(d,c)) {
              TopConcept
            } else {
              val variable = ConceptVariable.fresh
              GreatestFixpoint(variable, definerEliminator.expandDefinition(Subsumption(d, variable), c))
            }
          result = result.map(definerEliminator.expandDefinition(Subsumption(d, replacement),_))
          definerSubs = definerSubs.map(definerEliminator.expandDefinition(next, _))

          logger.trace("current result:")
          logger.trace(s"${result.mkString("\n")}")
          logger.trace("current definer definitions:")
          logger.trace(s"${definerSubs.mkString("\n")}")
        }
        case Subsumption(d, c) if definerEliminator.isDefiner(d) => {
          logger.trace(s"expand: ${d}")
          result = result.map(definerEliminator.expandDefinition(next, _))
          definerSubs = definerSubs.map(definerEliminator.expandDefinition(next, _))

          logger.trace("current result:")
          logger.trace(s"${result.mkString("\n")}")
          logger.trace("current definer definitions:")
          logger.trace(s"${definerSubs.mkString("\n")}")
        }
        case _ => assert(false, "unexpected case: "+next)
      }

    }

    result
  }

  def toDisjunctiveAssertion(aboxClause: ExtendedABoxClause): Assertion = {
    val disjuncts = aboxClause.roleAssertions ++
      aboxClause.negatedRoleAssertions.map(ra =>
        NegatedRoleAssertion(ra.role.asInstanceOf[BaseRole], ra.individual1, ra.individual2)) ++
      aboxClause.literals.keySet.map(ind =>
        ConceptAssertion(aboxClause.literals(ind).convertBack, ind))

    if(disjuncts.size==1)
      return disjuncts.iterator.next()
    else
      DisjunctiveAssertion(disjuncts.toSet[Assertion])
  }

  def asDisjuncts(aboxClause: ExtendedABoxClause): Set[Concept] = {
    def replaced = replaceNegativeDefiners(aboxClause)
    aboxClause.roleAssertions.map{ ra =>
      ExistentialRoleRestriction(TopRole, ConceptConjunction(Set(
        new NominalSet(ra.individual1),
        ExistentialRoleRestriction(ra.role, new NominalSet(ra.individual2))
      )))
    } ++ aboxClause.negatedRoleAssertions.map{ nra =>
      ExistentialRoleRestriction(TopRole, ConceptConjunction(Set(
	new NominalSet(nra.individual1),
        UniversalRoleRestriction(nra.role, ConceptComplement(new NominalSet(nra.individual2)))
      )))
    } ++ aboxClause.literals.keys.map{ ind =>
      ExistentialRoleRestriction(TopRole, ConceptConjunction(Set(
        new NominalSet(ind),
        replaced.literals(ind).convertBack
      )))
    }
  }
  


  def replaceNegativeDefiners(acl: ExtendedABoxClause) = {
    val inds = acl.literals.mapValues(cc =>
      new ConceptClause(cc.literals.map(_ match {
        case ConceptLiteral(false, d: BaseConcept) if ALCFormulaPreparations.isDefiner(d) =>
          ConceptLiteral(true, def4neg(d))
        case other => other
      }), cc.ordering)
    )
    new ExtendedABoxClause(inds, acl.roleAssertions, acl.negatedRoleAssertions)
  }

  /**
   * return a definer to be used for the negation of the given definer
   * @param definer
   */
  def def4neg(definer: BaseConcept) =
    negDefiner2Fresh.getOrElse(definer, {
      val fresh = ALCFormulaPreparations.newConcept()//definerFactory.newDefiner()
      negDefiner2Fresh += (definer -> fresh)
      fresh2NegDefiner += (fresh -> definer)
      fresh
    })
}
