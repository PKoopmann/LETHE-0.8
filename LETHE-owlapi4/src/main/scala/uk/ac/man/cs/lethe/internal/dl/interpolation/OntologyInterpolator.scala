package uk.ac.man.cs.lethe.internal.dl.interpolation

import com.typesafe.scalalogging.Logger

import scala.collection.JavaConversions._
import org.semanticweb.owlapi.model.{OWLClass, OWLEntity, OWLObjectProperty, OWLOntology}
import uk.ac.man.cs.lethe.internal.dl.{AbstractMappedReasonerFactory, HermitMappedReasonerFactory}
import uk.ac.man.cs.lethe.internal.dl.owlapi.OWLExporter
import uk.ac.man.cs.lethe.internal.dl.forgetting.ExistsEliminator
import uk.ac.man.cs.lethe.internal.tools.Cancelable
import uk.ac.man.cs.lethe.internal.dl.datatypes._
import uk.ac.man.cs.lethe.internal.dl.owlapi.{ModuleExtractor, OWLApiConverter}
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.FixpointApproximator
import uk.ac.man.cs.lethe.internal.dl.forgetting.direct.SymbolOrderings
import uk.ac.man.cs.lethe.internal.forgetting.{Forgetter, Interpolator}


class OntologyInterpolator(forgetter: Forgetter[Ontology, String],
                           delayedExistsElimination: Boolean = false,
                           val universalRoles: Boolean = false)
extends Interpolator[OWLOntology, OWLEntity] with Cancelable {

  var SUPPORT_INVERSE_ROLES = false

  //  implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(InterpolatorLogger)
//  import formatter._

  val logger = Logger[OntologyInterpolator]

  var dontForget = Set[String]()

  var includeMostFrequent = 0

  def uniformInterpolant(owlOntology: OWLOntology, signature: Set[OWLEntity]): OWLOntology = {

    val inner = uniformInterpolantInternalFormat(owlOntology, signature)

    logger.debug(s"the final interpolant is ${inner}")

    logger.debug("now converting to OWL data structure")


    val result = (new OWLExporter()).toOwlOntology(inner)


    logger.debug("done converting to OWL data structure.")

    result
  }

  /**
   * as above, but compute definer-less representation by approximating the
   * corresponding fixpoint expressions
   */
  def uniformInterpolant(owlOntology: OWLOntology,
			 signature: Set[OWLEntity],
			 approximateLevel: Int) = {
    var interpolant = uniformInterpolantInternalFormat(owlOntology, signature)

    interpolant = FixpointApproximator.approximate(interpolant, approximateLevel)

    (new OWLExporter()).toOwlOntology(interpolant)
  }

  /**
   * compute
   * @param owlOntology
   * @param signature
   * @return
   */
  def uniformInterpolantInternalFormat(owlOntology: OWLOntology, signature: Set[OWLEntity]): Ontology = {

    logger.debug(s"start interpolating")

    // OWLApiInterface.classify(owlOntology)

    val moduleAxioms = ModuleExtractor.getModule(owlOntology, signature.toSet[OWLEntity])

    val modulesSignature = moduleAxioms.flatMap(_.getSignature).filter(s => s.isInstanceOf[OWLClass]||s.isInstanceOf[OWLObjectProperty])


    // {
    //   //remove this block later!
    //   val exp = new OWLExporter()
    //   exp.save(exp.toOwlOntology(moduleAxioms), new File("module.owl"))
    // }

    val owlEntitiesToForget = modulesSignature--signature

    var ontology = new Ontology()
    OWLApiConverter.convert(moduleAxioms).foreach{ ontology.addStatement }

    if(!SUPPORT_INVERSE_ROLES)
      ontology = OntologyFilter.restrictToSHQ(ontology)


    if(universalRoles || delayedExistsElimination)
      ontology.addStatements(universalRoleStatements(ontology))

    // println("Module: ")
    // println(ontology)

    logger.info(s"Size of module: ${ontology.size}")
    logger.info(s"Module's axioms: ${ontology.statements.size}")



    var symbolsToForget = owlEntitiesToForget.map(OWLApiConverter.getName)--dontForget

    if(includeMostFrequent>0)
      symbolsToForget = SymbolOrderings.orderByNumOfOccurrences(symbolsToForget, ontology).drop(includeMostFrequent).toSet

    logger.info(s"Forgetting ${symbolsToForget.size} symbols.")

    println("\n INFO: "+symbolsToForget.size+" to be eliminated.")



    var interpolant = forgetter.forget(ontology, symbolsToForget)

    logger.debug(s"done forgetting. making nice now.")

    OntologyBeautifier.makeNice(interpolant)

    logger.debug(s"done making nice")

    if(delayedExistsElimination && !universalRoles) {
      logger.debug("Delayed Exists Elimination! Eliminating remaining occurrences of the universal role...")
      transferCancelInformation(ExistsEliminator)
      ExistsEliminator.setReasonerFactory(HermitMappedReasonerFactory)
      interpolant = ExistsEliminator.forget(interpolant, Set(TopRole))
    }

    logger.debug(s"cleaning up the rbox.")

    if(universalRoles || delayedExistsElimination)
      interpolant.rbox.axioms = interpolant.rbox.axioms.filterNot(_.roles.contains(TopRole))


    logger.debug(s"done with computing the uniform interpolant")

    interpolant
  }

  def universalRoleStatements(ontology: Ontology) = {
    var maxRoles = ontology.roles.filterNot(role =>
      ontology.rbox.axioms.exists(_ match {
        case RoleSubsumption(r,s) => r.equals(role)
        case _ => false
      })
    )

    maxRoles.map(RoleSubsumption(_, TopRole))
  }

  override def cancel: Unit = {
    super.cancel

    /*
      We use this solution rather than using transferCancelInformation to avoid memory leaks:
      otherwise, several instances of interpolators might remain in the lists of the forgetters
      cancelation dependencies
     */
    forgetter match {
      case cancelable: Cancelable =>
        cancelable.cancel
      case _ => ;
    }
  }



}
