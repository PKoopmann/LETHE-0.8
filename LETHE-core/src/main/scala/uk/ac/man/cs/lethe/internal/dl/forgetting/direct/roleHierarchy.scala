package uk.ac.man.cs.lethe.internal.dl.forgetting.direct

import scala.collection.mutable.{ HashMap, Set => MutSet, MultiMap, HashSet }

import uk.ac.man.cs.lethe.internal.dl.datatypes._

//import com.dongxiguo.fastring.Fastring.Implicits._

import com.typesafe.scalalogging.Logger

object RoleHierarchy {
  //  implicit val (logger, formatter, appender) =
  //  ZeroLoggerFactory.newLogger(RoleHierarchy)
  val logger = Logger[RoleHierarchy]
}

class RoleHierarchy(_rbox: RBox) extends SubsumptionChecker {

  /**
   * Return all axioms from the RBox that can be used to establish subRole is
   * a subrole of superRole.
   */
  def justificationsFor(subRole: Role, superRole: Role): Set[Set[RoleSubsumption]] = {

    def collect(subRole: Role, superRole: Role, visited: Set[Role]): Set[Set[RoleSubsumption]] = {
      if(subRole==superRole)
        Set(Set())
      else if(visited(subRole))
        Set() // cyclic path - discard
      else if(subRole==TopRole || getDirectSuperRoles(subRole).isEmpty)
        Set() // wrong path (not ending in superRole), discard
      else {
        val x: Set[Set[RoleSubsumption]] = getDirectSuperRoles(subRole)
          .flatMap { nextSubRole => // continue on next role
            collect(nextSubRole, superRole, visited + subRole) // for each successful path
              .map(_ + RoleSubsumption(subRole, nextSubRole)) // add the current step to it
          }
        x
      }
    }

    collect(subRole,superRole, Set())
    /*match{
      case None => Set()
      case Some(set) => set
    }*/
  }

  def this() = this(new RBox(Set()))
  def this(ontology: Ontology) = this(ontology.rbox)

  import RoleHierarchy._

  val rbox = _rbox

  private var superRoles = new HashMap[Role, MutSet[Role]]() with MultiMap[Role, Role]
  private var subRoles = new HashMap[Role, MutSet[Role]]() with MultiMap[Role, Role]
  var directSubRoles = new HashMap[Role, MutSet[Role]]() with MultiMap[Role, Role]
  var directSuperRoles = new HashMap[Role, MutSet[Role]]() with MultiMap[Role, Role]

  def getDirectSubRoles(role: Role): Set[Role] = directSubRoles.getOrElse(role, Set()).toSet
  //CHANGED 20/05/2020: temp fix for role forgetting issue with abduction (e.g. ont: empty, obs: exists r.C <= exists r.D). May need handling differently!
  def getDirectSuperRoles(role: Role): Set[Role] = 
      directSuperRoles.getOrElse(role, Set()).toSet

  def getSuperRoles(role: Role): Set[Role] = superRoles.getOrElse(role, Set()).toSet
  def getSubRoles(role: Role):Set[Role] = subRoles.getOrElse(role, Set()).toSet

  var knownRoles = new HashSet[Role]()

  var transitiveRoles = new HashSet[Role]()

  // Build up transitive role hierarchy
  {
    val directSubsumptions = new HashMap[Role, MutSet[Role]]() with MultiMap[Role, Role]

    var roles = HashSet[Role]()

    rbox.axioms.foreach{ _ match {
      case RoleSubsumption(r1, r2) => {
        directSubsumptions.addBinding(r1, r2)
        directSubsumptions.addBinding(DLHelpers.inverse(r1), DLHelpers.inverse(r2))
        directSuperRoles.addBinding(r1, r2)
        directSubRoles.addBinding(r2, r1)
        directSuperRoles.addBinding(DLHelpers.inverse(r1), DLHelpers.inverse(r2))
        directSubRoles.addBinding(DLHelpers.inverse(r2), DLHelpers.inverse(r1))
        roles += r1
        roles += r2
      }
      case TransitiveRoleAxiom(role) => transitiveRoles.add(role)
      case a => assert(false, "Not supported: "+a)
    } }

    def addRoles(r1: Role): Unit = {
      superRoles.addBinding(r1, r1)
      superRoles.addBinding(DLHelpers.inverse(r1), DLHelpers.inverse(r1))
      subRoles.addBinding(r1, r1)
      subRoles.addBinding(DLHelpers.inverse(r1), DLHelpers.inverse(r1))
      val successors = directSubsumptions.getOrElse(r1, Set()).toSet
      successors.foreach{ r2 =>
        if(directSubsumptions.contains(r1) && directSubsumptions(r1)(r2)){
          directSubsumptions.removeBinding(r1, r2)
          addRoles(r2)
          superRoles(r2).foreach{ r22 =>
            superRoles.addBinding(r1, r22)
            superRoles.addBinding(DLHelpers.inverse(r1), DLHelpers.inverse(r22))
            subRoles.addBinding(r22, r1)
            subRoles.addBinding(DLHelpers.inverse(r22), DLHelpers.inverse(r1))
          }
        }
      }
    }

    roles.foreach(addRoles)

    knownRoles = roles

    //    logger.trace(s"Role Hierarchy: \n${superRoles.mkString("\n")}")
  }

  // r2 <= r1
  def isSuperRole(r1: Role, r2: Role) = subsumedBy(r2, r1)
  def isSubRole(r1: Role, r2: Role) = subsumedBy(r1, r2)

  // checks whether r1 is subsumed by r2 (r1 <= r2)
  def subsumedBy(r1: Role, r2: Role) =
    r2==TopRole || r1==r2 || (superRoles.get(r1) match {
      case Some(subsumers) => subsumers(r2)
      case None => false
    })

  def equiv(r1: Role, r2: Role) = subsumedBy(r1, r2) && subsumedBy(r2, r1)

  def mostCommonSuperRoles(r1: Role, r2: Role): Set[Role] = {
    if(r1==r2)
      return Set(r1)

    if(!knows(r1) || !knows(r2))
      return Set()

    val commonSuperRoles = getSuperRoles(r1).filter(getSuperRoles(r2))
    commonSuperRoles.filterNot(superRole =>
      commonSuperRoles.exists(subRole =>
        subRole!=superRole && subsumedBy(subRole, superRole))).toSet
  }

  def mostCommonSubRoles(r1: Role, r2: Role): Set[Role] = {
    if(r1==r2)
      return Set(r1)

    if(!knows(r1) || !knows(r2))
      return Set() // not known means not in rbox - it does not mean not existent

    val commonSubRoles =
      subRoles.getOrElse(r1,Set()).filter(subRoles.getOrElse(r2, Set()))
    commonSubRoles.filterNot(subRole =>
      commonSubRoles.exists(superRole =>
        subRole!=superRole && subsumedBy(subRole, superRole))).toSet
  }

  def knows(role: Role) =
    knownRoles(role)
}

object EmptyRoleHierarchy extends RoleHierarchy {
  override def knows(role: Role) = true

  override def getSuperRoles(role: Role): Set[Role] = Set(TopRole)

  override def getDirectSuperRoles(role: Role): Set[Role] = Set(TopRole)
}

trait RoleSubsumptionChecker extends SubsumptionChecker {

  private var roleHierarchy: RoleHierarchy = EmptyRoleHierarchy

  private var ignoreRole: Role = _

  /**
   * role to be ignored when checking role hierarchy (to avoid making forgetting results immediately redundant)
   */
  def setIgnoreRole(role: Role) =
    this.ignoreRole = role

  def setRoleHierarchy(roleHierarchy: RoleHierarchy) =
    this.roleHierarchy=roleHierarchy

  // r1 <= r2  ------> r1 subsumes r2
  override def subsumes(role1: Role, role2: Role) = {
    assert(roleHierarchy!=null)
    logger.trace(s"ignoring role ${ignoreRole}")
    role1==role2 ||
      (!role1.equals(ignoreRole) &&
        !role2.equals(ignoreRole) &&
        roleHierarchy.subsumedBy(role1, role2))

    // <-- renaming of function might make sense here to avoid confusion
    // but: the name of this method refers to subsumption as typical in resolution, and the
    // name in roleHierarchy refers to subsumption as typical in ontology languages
  }
}
