package uk.ac.man.cs.lethe.internal.dl.forgetting

import java.util.Date
import uk.ac.man.cs.lethe.internal.dl.{AbstractMappedReasoner, AbstractMappedReasonerFactory}
import uk.ac.man.cs.lethe.internal.tools.{CanceledException, Timeoutable}

import scala.collection.JavaConversions._
import scala.concurrent.TimeoutException

//import com.dongxiguo.fastring.Fastring.Implicits._
import com.typesafe.scalalogging.Logger


import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct._
//import uk.ac.man.cs.lethe.internal.dl.owlapi.OWLExporter
import uk.ac.man.cs.lethe.internal.forgetting.Forgetter
import uk.ac.man.cs.lethe.internal.tools.{ ProgressBar, ConsoleProgressBar }

object RoleForgetterS extends Forgetter[Ontology, String] {

  override def steps = RoleForgetter.steps

  override def forget(ontology: Ontology, _symbols: Set[String]) = {
    val symbols = _symbols //.filter(ontology.roleSymbols)
    RoleForgetter.forget(ontology, symbols.map(BaseRole))
  }


}

object RoleForgetter extends Forgetter[Ontology, BaseRole] with Timeoutable {
  //  implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(this)
  //  import formatter._

  val logger = Logger(RoleForgetter.getClass)

  var progressBar: ProgressBar = new ConsoleProgressBar()

  var counter = 0
  override def steps = counter

  var rbox = new RBox(Set())

  var roleHierarchy: RoleHierarchy = _ //new RoleHierarchyX()

  var reasoner: AbstractMappedReasoner = _ //new MappedReasoner(new Ontology())

  var rolePropagationRule: RolePropagationRule = _

  //  var inverseRolePropagationRule: InverseRolePropagationRule = _

  var definerFactory: DefinerFactory = _

  var structuralTransformer: StructuralTransformer = _

  var subsumptionChecker: SubsumptionChecker = _

  var applyExtCalculus = false

  var reasonerFactory: AbstractMappedReasonerFactory = _ // set by the ConceptAndRoleForgetter

  def clean() = {
    reasoner  = null
    rbox = null
    roleHierarchy = null
    definerFactory = null
    structuralTransformer = null
    definerFactory = null
    rolePropagationRule = null
    subsumptionChecker = null
    DirectALCForgetterRoles.clean()
  }

  override def forget(_ontology: Ontology, roles: Set[BaseRole]): Ontology = {

    startTiming

    //    println("Restricting to ALCH")
    val ontology = OntologyFilter.restrictToALCH(_ontology)

    logger.info(s"Forgetting: ${roles}")

    // create a copy so we don't modify the input
    var result = new Ontology(tbox=ontology.tbox, abox=ontology.abox, rbox=ontology.rbox)

    var orderedSymbols =
      SymbolOrderings.orderByNumOfOccurrences(roles.map(_.name), result).reverse

    var counter = 0

    progressBar.init(orderedSymbols.size, "Roles ")

    orderedSymbols.foreach{ symbol =>

      if(isCanceled)
        return result

      val role = BaseRole(symbol)
      progressBar.update(counter, "Forgetting "+symbol.split("#").last)
      logger.info(s"Remaining role symbols: ${roles.size-counter}")
      logger.info(s"Ontology size: ${result.size}")
      rbox = result.rbox
      logger.info("Building up role hierarchy...")
      roleHierarchy = new RoleHierarchy(result)

      // let forgetter know about current role hierarchy
      // (design can be improved...)
      DirectALCForgetterRoles.setRoleHierarchy(roleHierarchy)

      logger.info("Built up role hierarchy.")

      try {
        result = forget(result, role)
      } catch {
        case _: TimeoutException =>
          assert(timeoutOccurred)
          logger.info("Timeout occured!")
        case _: CanceledException =>
          logger.info("Execution canceled!")

        case unknownThrowable: Throwable => throw unknownThrowable
      }

      if(!isCanceled)
        result.rbox = forgetInRBox(rbox, Set(role))



      assert(isCanceled || !result.roleSymbols.contains(role.name))

      counter += 1
    }

    progressBar.finish()

    roleHierarchy = new RoleHierarchy(result) // in case loop was not executed (roles only in rbox)

    result.rbox = forgetInRBox(result.rbox, roles) // <-- needed? In case roles only in rbox?

    assert(isCanceled || !result.roleSymbols.map(BaseRole).exists(roles))

    result
  }

  def forget(ontology: Ontology, role: BaseRole): Ontology = {

    logger.info(s"Forgetting ${role}")
    logger.debug(s" in ${ontology}")
    //println(fast"Forgetting ${role}")

    applyExtCalculus = false

    ALCFormulaPreparations.initDefinitions()


    val (base, nonBase, both) = DLForgettingPreprocessing.sortStatements(ontology.statements, Set(role.name))


    logger.trace(s"super: ${roleHierarchy.directSuperRoles}")


    // possibly redundant generation of clauses, but maybe needed in order to collect clauses who were 
    // in the forbidden signature at the beginning (dangerous? what if definer names change?)
    logger.info("Generate clauses...")

    ALCFormulaPreparations.initDefinitions()
    var clauses =  ALCFormulaPreparations.clauses(nonBase++both, SimpleLiteralOrdering)

    logger.info("Generated clauses.")
    logger.debug(s"the clauses are: \n ${clauses.mkString("\n")}")

    var applied = false

    transferCancelInformation(DirectALCForgetterRoles)

    // apply rules if necessary
    if((roleHierarchy.getDirectSuperRoles(role)-role).isEmpty){
      // Case 1
      logger.debug(s"For ${role} we are in Case 1 -> also role resolution!")

      // existential role restrictions will be removed - apply role restriction resolution
      reasoner = reasonerFactory.newReasoner(clauses)
      DirectALCForgetterRoles.mappedReasoner=reasoner

      clauses = DirectALCForgetterRoles.forget(clauses,
        role.name)

      applied = true
    } else { // if((roleHierarchy.getDirectSubRoles(role)-role).isEmpty){
      // Case 2

      logger.debug(s"For ${role} we are in Case 2 -> just propagate, no role resolution!")

      // role propagation always has to be applied exhaustively, even if the current role is "in the middle"
      // consider: r <= s, s<= t
      // A <= Es.B n As.¬B
      // if we apply role propagation, we get A <= Es.(B n ¬B), and in the next step A <= Et.(B n ¬B)
      // if we do not, we only obtain A<=Et.B, A <= Ar.¬B and r <= t, from which it does not follow anymore
      // that A is unsatisfiable


      // universal role restrictions will be removed - apply role propagation exhaustively!
      clauses = DirectALCForgetterRoles.forget(clauses, role.name,
        justPropagate=true)


      applied = true
    }



    logger.debug("Clauses after inferences where made:")
    logger.debug(s"${clauses.mkString("\n")}")


    // General note on what follows:
    // the if-statements are not neccessary, since we assume that clauses in the signature are not
    // filtered out

    logger.debug(s"super roles of ${role}: ${roleHierarchy.getDirectSuperRoles(role)}")
    val superRoles =roleHierarchy.getDirectSuperRoles(role)-role
    logger.debug(s" without: ${superRoles}")

    // existential role restriction replacement (on old clauses)
    // happens only in the case where role restriction resolution has not been applied (Case 1)
    //  if(superRoles.size>0){
    // if(applied) // add clauses, if case 2 applied, because clauses will have been filtered out
    // 	clauses++=replaceExBySuperRole(inForbiddenSignature, role)

    // else
    // <-- this case should not be interesting, because we should not be deleting any clauses
    //     that are outside the signature

    clauses = replaceExBySuperRole(clauses, role, superRoles)

    //  }

    logger.debug("After replacing in existential roles:")
    logger.debug(s"${clauses.mkString("\n")}")

    logger.trace(s"sub: ${roleHierarchy.directSubRoles}")
    val subRoles =roleHierarchy.getDirectSubRoles(role)-role
    logger.trace(s"without: "+subRoles)


    // universal role restriction replacement (on all clauses)
    if(subRoles.isEmpty){
      clauses = filterOutUniversalRestrictions(clauses, role)
    } else {
      clauses = replaceUnBySubRole(clauses, role, subRoles)
    }



    logger.debug("After replacing in universal roles:")
    logger.debug(s"${clauses.mkString("\n")}")

    logger.info("Simplifying...")
    var result = DLHelpers.simplify(Ontology.buildFrom(
      SimpleDefinerEliminator.eliminateDefiners(clauses)))
    logger.info("Simplified")
    base.foreach(result.addStatement)

    assert(isCanceled || !result.roleSymbols.contains(role.name))

    result
  }

  def replaceExBySuperRole(clauses: Iterable[ConceptClause], role: BaseRole, replaceWith: Iterable[Role]): Set[ConceptClause] =
    if(!DirectALCForgetterRoles.trackRoleAxioms){
      logger.info(s"Replacing ${role} by super roles  ${replaceWith} in existential restrictions")
      clauses.map(cl => cl.literals.map{ _ match {
        case ConceptLiteral(true, ExistentialRoleRestriction(r, c)) if r.equals(role) =>
          replaceWith.map{ r2 =>
            ConceptLiteral(true, ExistentialRoleRestriction(r2, c))
          }
        case cl => Set(cl)
      }}).flatMap { set =>
        attach(set).map(new ConceptClause(_))
      }.toSet[ConceptClause]
    } else {
      clauses.flatMap { cl =>
        val combinations: Set[Iterable[List[ConceptLiteral]]] = cl.literals.map(
          _ match {
            case ConceptLiteral(true, ExistentialRoleRestriction(r, c)) if r.equals(role) =>
              replaceWith.flatMap { r2 =>
                roleHierarchy.justificationsFor(r, r2).map { just =>
                  List(ConceptLiteral(true, ExistentialRoleRestriction(r2, c))) ++
                    just.map(ax => ConceptLiteral(true, AxiomTracker.getConcept(ax)))
                }
              }
            case literal => Set(List(literal))
          }
        )
        attach(combinations).map(
          (join: Iterable[List[ConceptLiteral]]) => new ConceptClause(join.flatten))
      }.toSet[ConceptClause]
    }


  def replaceUnBySubRole(clauses: Iterable[ConceptClause], role: BaseRole, replaceWith: Iterable[Role]): Set[ConceptClause] =
    if(!DirectALCForgetterRoles.trackRoleAxioms){
      logger.info(s"Replacing ${role} by sub roles ${replaceWith} in universal restrictions")
      clauses.map(cl => cl.literals.map{ _ match {
        case ConceptLiteral(true, UniversalRoleRestriction(r, c)) if r.equals(role) =>
          replaceWith.map{ r2 =>
            ConceptLiteral(true, UniversalRoleRestriction(r2, c))
          }
        case cl => Set(cl)
      }}).flatMap { set =>
        attach(set).map(new ConceptClause(_))

      }.toSet[ConceptClause]
    } else {
      clauses.flatMap { cl =>
        val combinations: Set[Iterable[List[ConceptLiteral]]] = cl.literals.map(
          _ match {
            case ConceptLiteral(true, UniversalRoleRestriction(r, c)) if r.equals(role) =>
              replaceWith.flatMap { r2 =>
                roleHierarchy.justificationsFor(r2, r).map { just =>
                  List(ConceptLiteral(true, UniversalRoleRestriction(r2, c))) ++
                    just.map(ax => ConceptLiteral(true, AxiomTracker.getConcept(ax)))
                }
              }
            case literal => Set(List(literal))
          }
        )
        attach(combinations).map(
          (join: Iterable[List[ConceptLiteral]]) => new ConceptClause(join.flatten))
      }.toSet[ConceptClause]

    }

  // [[1,a],[2,b]] => [[1,2],[1,b],[a,2],[a,b]]
  def attach[A](setSet: Iterable[Iterable[A]]): Iterable[Iterable[A]] = {
    if(setSet.size<=1)
      setSet
    else
      setSet.head.flatMap(head => attach(setSet.tail).map(tail => tail++Set(head)))
  }


  def filterOutUniversalRestrictions(clauses: Iterable[ConceptClause], role: BaseRole) = {
    logger.info(s"removing clauses containing universal restrictions on ${role}")
    clauses.filterNot(cl => cl.literals.exists{ _.concept match {
      case UniversalRoleRestriction(r, _) => r.signature.contains(role.name)
      case _ => false
    }}).toSet[ConceptClause]
  }

  def forgetInRBox(rbox: RBox, _roles: Set[BaseRole]) = {

    var roles = _roles ++ _roles.map(DLHelpers.inverse)
    var axioms = rbox.axioms
    roles.foreach{role =>
      axioms = axioms.flatMap(_ match {
        case RoleSubsumption(r1: Role, r2: Role) => {
          val r1s =
            if(r1==role)
              roleHierarchy.getDirectSubRoles(r1)--roles-r2
            else
              Set(r1)-r2

          val r2s =
            if(r2==role)
              roleHierarchy.getDirectSuperRoles(r2)--roles-r1
            else
              Set(r2)-r1

          r1s.flatMap(r1x => r2s.map(r2x => RoleSubsumption(r1x, r2x))).toSet[RoleAxiom]
        }
        case TransitiveRoleAxiom(r) => {
          assert(!roles(r))
          Some(TransitiveRoleAxiom(r))
        }
        case ra => assert(false, "Not implemented: "+ra.toString); Set(ra)
      })}

    RBox(axioms)
  }
}
