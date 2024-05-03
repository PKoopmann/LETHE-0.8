package uk.ac.man.cs.lethe.internal.dl.abduction.filtering;

import scala.collection.JavaConverters;
import uk.ac.man.cs.lethe.internal.dl.datatypes.extended.DisjunctiveAssertion;
import uk.ac.man.cs.lethe.internal.dl.datatypes.extended.NegatedRoleAssertion;
import uk.ac.man.cs.lethe.internal.dl.datatypes.Assertion;
import uk.ac.man.cs.lethe.internal.dl.datatypes.BaseRole;
import uk.ac.man.cs.lethe.internal.dl.datatypes.BottomConcept$;
import uk.ac.man.cs.lethe.internal.dl.datatypes.Concept;
import uk.ac.man.cs.lethe.internal.dl.datatypes.ConceptAssertion;
import uk.ac.man.cs.lethe.internal.dl.datatypes.ConceptConjunction;
import uk.ac.man.cs.lethe.internal.dl.datatypes.ConceptDisjunction;
import uk.ac.man.cs.lethe.internal.dl.datatypes.DLHelpers;
import uk.ac.man.cs.lethe.internal.dl.datatypes.ExistentialRoleRestriction;
import uk.ac.man.cs.lethe.internal.dl.datatypes.Individual;
import uk.ac.man.cs.lethe.internal.dl.datatypes.Role;
import uk.ac.man.cs.lethe.internal.dl.datatypes.UniversalRoleRestriction;

import java.util.*;

/*
Takes a set of DisjunctiveAssertions  (forgetting solution), applies simplification rules relevant to KB abduction problem.
 */

public class AbductionSimplifier {

    public java.util.Set<Assertion> simplify(scala.collection.immutable.Set<Assertion> conjuncts) {//method to execute entire process



        java.util.Set<DisjunctiveAssertion> originalConjuncts = new HashSet<DisjunctiveAssertion>();
        java.util.Set<Assertion> simplificationResult = new HashSet<Assertion>();

        for(Assertion as:JavaConverters.setAsJavaSet(conjuncts)) {
            if(as instanceof DisjunctiveAssertion) {
                originalConjuncts.add((DisjunctiveAssertion) as);
            }
            else {
                originalConjuncts.add(new DisjunctiveAssertion(JavaConverters.asScalaSet(new HashSet<Assertion>(Arrays.asList(as))).toSet()));
            }
        }
        //java.util.Set<DisjunctiveAssertion> originalConjuncts = JavaConverters.setAsJavaSet(conjuncts);

        java.util.Set<DisjunctiveAssertion> originalConjunctsNNF = getNNF(originalConjuncts);

        //Take conjuncts in NNF, for each disjunct in the DisjunctiveAssertion, store flattened version in a set. Then, add DisjunctiveAssertion(flattenedSet) to a new set of disjunctive assertions
        java.util.Set<DisjunctiveAssertion> simplifiedConjuncts = new HashSet<DisjunctiveAssertion>();

        for(DisjunctiveAssertion as:originalConjunctsNNF) {
            java.util.Set<Assertion> simplifiedDisjuncts = new HashSet<Assertion>();
            java.util.Set<Assertion> nonConceptAssertions = new HashSet<Assertion>();
            for(Assertion disj:JavaConverters.setAsJavaSet(as.disjuncts())) {

                if(!(disj instanceof ConceptAssertion)) { //e.g. r(a, b)
                    nonConceptAssertions.add(disj);
                }
                else { //e.g. disjunctive assertion (C or D)(a) or B(b)
                    Individual ind = ((ConceptAssertion) disj).individual();
                    Concept conc = ((ConceptAssertion) disj).concept();
                    if(conc instanceof ConceptDisjunction) {
                        ConceptDisjunction subDisjunction = (ConceptDisjunction) conc;
                        for(Concept subDisj : JavaConverters.setAsJavaSet(subDisjunction.disjuncts())) {

                            java.util.Set<Concept> expandedConjunctions = expandNestedConjunctions(subDisj);
                            if(expandedConjunctions.size() > 1) {
                                ConceptConjunction flattenedDisj = new ConceptConjunction(JavaConverters.asScalaSet(expandedConjunctions).toSet());
                                simplifiedDisjuncts.add(new ConceptAssertion(flattenedDisj, ind));
                            }
                            else if(expandedConjunctions.size() == 1) {
                                Concept flattenedDisj = expandedConjunctions.iterator().next();
                                simplifiedDisjuncts.add(new ConceptAssertion(flattenedDisj, ind));
                            }
                        }
                    }
                    else if(conc instanceof ConceptConjunction) {
                        ConceptConjunction subConjunction = (ConceptConjunction) conc;

                        java.util.Set<Concept> expandedConjunctions = new HashSet<Concept>();
                        for(Concept subConj : JavaConverters.setAsJavaSet(subConjunction.conjuncts())) {
                            expandedConjunctions.addAll(expandNestedConjunctions(subConj));
                        }

                        if(expandedConjunctions.size() > 1 ) {
                            ConceptConjunction flattenedConjunct = new ConceptConjunction(JavaConverters.asScalaSet(expandedConjunctions).toSet());
                            simplifiedDisjuncts.add(new ConceptAssertion(flattenedConjunct, ind));
                        }
                        else if(expandedConjunctions.size() == 1){
                            Concept flattenedConjunct = expandedConjunctions.iterator().next();
                            simplifiedDisjuncts.add(new ConceptAssertion(flattenedConjunct, ind));
                        }

                    }
                    else {
                        Set<Concept> expandedConjunctions = expandNestedConjunctions(((ConceptAssertion) disj).concept());
                        if(expandedConjunctions.size() > 1) {
                            ConceptConjunction flattenedDisj = new ConceptConjunction(JavaConverters.asScalaSet(expandedConjunctions).toSet());
                            simplifiedDisjuncts.add(new ConceptAssertion(flattenedDisj, ((ConceptAssertion) disj).individual()));
                        }
                        else if(expandedConjunctions.size() == 1){
                            Concept flattenedDisj = expandedConjunctions.iterator().next();
                            simplifiedDisjuncts.add(new ConceptAssertion(flattenedDisj, ((ConceptAssertion) disj).individual()));
                        }
                    }
                }
            }
            simplifiedDisjuncts.addAll(nonConceptAssertions);
            simplifiedConjuncts.add(new DisjunctiveAssertion((JavaConverters.asScalaSet(simplifiedDisjuncts)).toSet()));
        }

        // pull out all tole-roles
        java.util.Set<DisjunctiveAssertion> conjunctsNoTopRole = new HashSet<DisjunctiveAssertion>();
        for(DisjunctiveAssertion as:simplifiedConjuncts) {
            java.util.Set<Assertion> disjunctsNoTopRole = new HashSet<Assertion>();
            java.util.Set<Assertion> nonConceptAssertions = new HashSet<Assertion>();

            for (Assertion disj : JavaConverters.setAsJavaSet(as.disjuncts())) {
                if(!(disj instanceof ConceptAssertion)) {
                    nonConceptAssertions.add(disj);
                }
                else {
                    Concept conc = ((ConceptAssertion) disj).concept();
                    Individual ind = ((ConceptAssertion) disj).individual();
                    //if(conc instanceof ConceptDisjunction) { //TODO:needed?

                    //}
                    if(conc instanceof ConceptConjunction) {
                        ConceptConjunction subConjunction = (ConceptConjunction) conc;

                        java.util.Set<Concept> expandedConjunctions = new HashSet<Concept>();
                        for(Concept subConj : JavaConverters.setAsJavaSet(subConjunction.conjuncts())) {
                            expandedConjunctions.addAll(pullOutTopRole(subConj));
                        }

                        if(expandedConjunctions.size() > 1 ) {
                            ConceptConjunction flattenedConjunct = new ConceptConjunction(JavaConverters.asScalaSet(expandedConjunctions).toSet());
                            disjunctsNoTopRole.add(new ConceptAssertion(flattenedConjunct, ind));
                        }
                        else if(expandedConjunctions.size() == 1){
                            Concept flattenedConjunct = expandedConjunctions.iterator().next();
                            disjunctsNoTopRole.add(new ConceptAssertion(flattenedConjunct, ind));
                        }
                    }
                    else {
                        java.util.Set<Concept> concTranslated = pullOutTopRole(conc);

                        if(concTranslated.size() > 1) {
                            disjunctsNoTopRole.add(new ConceptAssertion(new ConceptConjunction(JavaConverters.asScalaSet(concTranslated).toSet()), ((ConceptAssertion)disj).individual()));
                        }
                        else if(concTranslated.size() == 1){
                            Concept concNoTopRole = concTranslated.iterator().next(); //should never be empty - check
                            disjunctsNoTopRole.add(new ConceptAssertion(concNoTopRole, ((ConceptAssertion)disj).individual()));
                        }
                    }
                }
            }
            disjunctsNoTopRole.addAll(nonConceptAssertions);
            conjunctsNoTopRole.add(new DisjunctiveAssertion((JavaConverters.asScalaSet(disjunctsNoTopRole)).toSet()));
        }

        //apply distributivity (CNF?) to each (simplified) disjunctive assertion in the forgetting solution
        java.util.Set<DisjunctiveAssertion> distributedConjuncts = new HashSet<DisjunctiveAssertion>();
        for(DisjunctiveAssertion conjunct:conjunctsNoTopRole) {
            distributedConjuncts.addAll(applyDistributivity(conjunct));
        }

        simplificationResult.addAll(distributedConjuncts);
        return simplificationResult;
    }

    private java.util.Set<DisjunctiveAssertion> getNNF(java.util.Set<DisjunctiveAssertion> originalConjuncts) {
        //get NNF - e.g. not forAll r.(not C and D) --> exists r.(C and not D)
        java.util.Set<DisjunctiveAssertion> originalConjunctsNNF = new HashSet<DisjunctiveAssertion>();
        for(Assertion as:originalConjuncts) {
            if(as instanceof DisjunctiveAssertion) {
                java.util.Set<Assertion> disjuncts = new HashSet<Assertion>(JavaConverters.setAsJavaSet(((DisjunctiveAssertion) as).disjuncts()));
                java.util.Set<Assertion> disjunctsNNF = new HashSet<Assertion>();

                for (Assertion disj : disjuncts) {
                    if(disj instanceof NegatedRoleAssertion) {
                        disjunctsNNF.add(disj);
                    }
                    else {
                        disjunctsNNF.addAll(JavaConverters.setAsJavaSet(DLHelpers.nnf(disj)));
                    }
                }

                DisjunctiveAssertion disjAs = new DisjunctiveAssertion(JavaConverters.asScalaSet(disjunctsNNF).toSet());
                originalConjunctsNNF.add(disjAs);
            }
            else if(as instanceof Assertion){
                if(as instanceof NegatedRoleAssertion) {
                    originalConjunctsNNF.add(new DisjunctiveAssertion(JavaConverters.asScalaSet(new HashSet<Assertion>(Arrays.asList(as))).toSet()));
                }
                else {
                    originalConjunctsNNF.add(new DisjunctiveAssertion(DLHelpers.nnf(as)));
                }
            }
        }
        return originalConjunctsNNF;
    }

    /**
     * This pulls out conjunctions out of universal role restrictions:
     * Ar.(A and B) -> (Ar.A and Ar.B)
     */
    private java.util.Set<Concept> expandNestedConjunctions(Concept originalConc) { //expands nested conjunctions within a given (non-disjunctive) concept
        java.util.Set<Concept> flattenedConc = new HashSet<Concept>();

        //initialize unflattened conjuncts as original class expression, expand this as needed
        List<Concept> unflattenedConcepts = new ArrayList<Concept>();

        unflattenedConcepts.add(originalConc);

        ListIterator<Concept> iterator = unflattenedConcepts.listIterator();
        List<Concept> beenChecked = new ArrayList<Concept>(); //capture "simplest" conjuncts, i.e. don't re-check forAll r.B

        while(iterator.hasNext()) {
            Concept conc = iterator.next();

            if(beenChecked.contains(conc) || !(conc instanceof UniversalRoleRestriction)) { //if simplest, add to flattened conjuncts
                flattenedConc.add(conc);
                continue;
            }
            else {
                Role concRole = ((UniversalRoleRestriction) conc).role();
                Concept filler = ((UniversalRoleRestriction) conc).filler();

                if(filler instanceof ConceptConjunction) { //forAll r.(B and forAll r.(C and D)) --> forAll r.B and forAll r.(C and D)
                    for(Concept nestedConj:JavaConverters.setAsJavaSet(((ConceptConjunction) filler).conjuncts())) {
                        iterator.add(new UniversalRoleRestriction(((UniversalRoleRestriction) conc).role(), nestedConj));
                        iterator.previous();
                    }
                }
                else if(filler instanceof UniversalRoleRestriction) {
                    List<Role> roles = new ArrayList<Role>();
                    roles.add(((UniversalRoleRestriction) conc).role());

                    boolean furtherNesting = true;
                    Concept subFiller = filler;
                    while(furtherNesting == true) {

                        if(subFiller instanceof UniversalRoleRestriction) {
                            subFiller = ((UniversalRoleRestriction) subFiller).filler();
                            roles.add(((UniversalRoleRestriction) filler).role()); //add role to nesting list
                        }
                        else {
                            furtherNesting = false;
                        }
                        filler = subFiller;
                    }
                    //reverse role list, apply to each disjunct found in nesting
                    Collections.reverse(roles);

                    //Now, concept under (role1, role2, role3,...) of the form A, A or B etc
                    if(subFiller instanceof ConceptConjunction) {
                        for (Concept nestedConj : JavaConverters.setAsJavaSet(((ConceptConjunction) subFiller).conjuncts())) {
                            for (Role role : roles) {
                                nestedConj = new UniversalRoleRestriction(role, nestedConj);
                            }
                            iterator.add(nestedConj);
                            iterator.previous();
                            beenChecked.add(conc);
                        }
                    }
                    else {
                        Concept nestedConc = subFiller;
                        for(Role role:roles) {
                            nestedConc = new UniversalRoleRestriction(role, nestedConc);
                        }
                        iterator.add(nestedConc);
                        iterator.previous();
                        beenChecked.add(conc);
                    }
                }
                else { //i.e. filler in forall r.(filler) not instance of forall r. nor conjunction
                    flattenedConc.add(conc);
                }
            }
        }
        return flattenedConc;
    }


    /**
     * pull out all universal roles occurring under a role restriction of another role
     * @param originalConc
     * @return
     */
    private java.util.Set<Concept> pullOutTopRole(Concept originalConc) {

        Queue<Concept> unflattenedConcepts = new LinkedList<Concept>();
        unflattenedConcepts.add(originalConc);

        return pullOutTopRole(unflattenedConcepts);
    }

    private java.util.Set<Concept> pullOutTopRole(Queue<Concept> unflattenedConcepts) {

        // note to the comments: in the following, "flattened" means universal roles pulled out

        //ListIterator<Concept> iterator = unflattenedConcepts.listIterator();
        List<Concept> beenChecked = new ArrayList<Concept>();

        java.util.Set<Concept> flattenedConc = new HashSet<Concept>();

        while(!unflattenedConcepts.isEmpty()) {
            Concept conc = unflattenedConcepts.poll();

            boolean foundNestedTopRole = false;

         /*   if(beenChecked.contains(conc)) {
                flattenedConc.add(conc);
                continue;
            }
          */
            //Concept filler = ((ExistentialRoleRestriction) conc).filler();
            if(conc instanceof ConceptConjunction) {
                Collection<Concept> conjuncts = JavaConverters.asJavaCollection(((ConceptConjunction)conc).conjuncts());

                // conjuncts still to be processed
                for(Concept conjunct: conjuncts){
                    unflattenedConcepts.add(conjunct);
                }
            }
            else if(conc instanceof ConceptDisjunction) {
                // apply recursively on each disjunct, afterwards, flattening done
                Set<Concept> newDisjuncts = new HashSet<>();
                Collection<Concept> oldDisjuncts = JavaConverters.asJavaCollection((((ConceptDisjunction)conc).disjuncts()));
                for(Concept concept: oldDisjuncts){
                    newDisjuncts.add(new ConceptConjunction(
                            JavaConverters.asScalaSet(pullOutTopRole(concept)).toSet()));
                }

                flattenedConc.add(new ConceptDisjunction(
                        JavaConverters.asScalaSet(newDisjuncts).toSet()
                ));
            }
            else if(conc instanceof UniversalRoleRestriction) {
                Concept oldFiller = ((UniversalRoleRestriction) conc).filler();
                Role concRole = ((UniversalRoleRestriction) conc).role();

                Concept filler = DLHelpers.conjunction(
                        JavaConverters.asScalaSet(pullOutTopRole(oldFiller))
                );

                if(filler instanceof ConceptConjunction) { //e.g. forAll r.(C and exists topRole.(...))

                    Set<Concept> fillerConjuncts = JavaConverters.setAsJavaSet(((ConceptConjunction)filler).conjuncts());

                    // always split conjunction under role restriction
                    // it is not sound to pull out nested universal role restrictions:
                    // forall r.(C and exists topRole.D) is NOT equivalent to (forall r.C and exists topRole.D)

                    // adding to unflattened: we might still need to pull directly nested universal roles out
                    fillerConjuncts.forEach(c -> unflattenedConcepts.add(new UniversalRoleRestriction(concRole, c)));

                    /*
                    for(Concept conj:JavaConverters.setAsJavaSet(((ConceptConjunction) filler).conjuncts())) {
                        if(conj instanceof ExistentialRoleRestriction) {
                            Role conjRole = ((ExistentialRoleRestriction) conj).role();
                            if(conjRole instanceof BaseRole && ((BaseRole) conjRole).name().equals("TOP-ROLE")) {
                                fillerConjuncts.remove(conj);
                                iterator.add(new ExistentialRoleRestriction(conjRole, ((ExistentialRoleRestriction)conj).filler()));
                                iterator.previous();
                                foundNestedTopRole = true;
                            }
                        }
                    }
                    iterator.add(new UniversalRoleRestriction(concRole, new ConceptConjunction(JavaConverters.asScalaSet(fillerConjuncts).toSet())));
                    iterator.previous();
                     */
                }
                else if(filler instanceof ConceptDisjunction) {

                    // forall r.(C or exists topRole.D) -> (forall r.C or exists topRole.D)
                    // (sound because: if "forall r.C" is not satisfied, there must be an r-successor not in C, which
                    // would have the topRole-successor in D)
                    // important: if no disjuncts are left in the disjunction, this corresponds to BOTTOM! Therefore, we should have
                    // for instance
                    // forall r.(exists topRole.A or exists topRole.B) -> (forall r.BOTTOM or exists topRole.A or exists topRole.B)

                    java.util.Set<Concept> fillerDisjuncts= new HashSet<>(
                            JavaConverters.setAsJavaSet(((ConceptDisjunction)filler).disjuncts()));
                    java.util.Set<Concept> newDisjuncts = new HashSet<Concept>();

                    for(Concept disj:JavaConverters.setAsJavaSet(((ConceptDisjunction)filler).disjuncts())) {
                        if(disj instanceof ExistentialRoleRestriction) {
                            Role disjRole = ((ExistentialRoleRestriction) disj).role();
                            if(disjRole instanceof BaseRole && ((BaseRole) disjRole).name().equals("TOP-ROLE")) {
                                fillerDisjuncts.remove(disj);
                                foundNestedTopRole = true;
                                newDisjuncts.add(new ExistentialRoleRestriction(disjRole, ((ExistentialRoleRestriction)disj).filler()));
                            }
                        }
                    }
                    //if(foundNestedTopRole = true) {
                    if(fillerDisjuncts.size() > 1) {
                        Concept conc1 = new UniversalRoleRestriction(concRole, new ConceptDisjunction(JavaConverters.asScalaSet(fillerDisjuncts).toSet()));
                        newDisjuncts.add(conc1);
                    }
                    else if(fillerDisjuncts.size() == 1) {
                        Concept conc1 = new UniversalRoleRestriction(concRole, fillerDisjuncts.iterator().next());
                        newDisjuncts.add(conc1);
                    } else if(fillerDisjuncts.size() == 0) {
                        Concept conc1 = new UniversalRoleRestriction(concRole, BottomConcept$.MODULE$);
                        newDisjuncts.add(conc1);
                    }

                    // the result is finished flattening: any deeper nested universal roles were pulled out due recursive call,
                    // immediately nested universal roles were pulled out here.
                    flattenedConc.add(new ConceptDisjunction(JavaConverters.asScalaSet(newDisjuncts).toSet()));
                }
                else if(filler instanceof ExistentialRoleRestriction) {
                    Role fillerRole = ((ExistentialRoleRestriction)filler).role();
                    if(fillerRole instanceof BaseRole && ((BaseRole)fillerRole).name().equals("TOP-ROLE")) {
                        BottomConcept$ bot = BottomConcept$.MODULE$;
                        Concept conc1 = new UniversalRoleRestriction(concRole, bot);
                        Concept conc2 = filler;

                        // again, due to the recursive call, this has been fully flattened
                        flattenedConc.add(new ConceptDisjunction(JavaConverters.asScalaSet(new HashSet<Concept>(Arrays.asList(conc1, conc2))).toSet()));
                        foundNestedTopRole = true;
                    }
                } else {
                    // meaning: filler (after pulling out), is a universal restriction, a nominal set, or a concept names, or a
                    // negated concept name.
                    // there was nothing to pull out, so we can add the original concept.
                    flattenedConc.add(conc);
                }
            }
            else if(conc instanceof ExistentialRoleRestriction) {
                // exists r.(A and exists topRole.B) -> (exists r.A and exists topRole.B)

                Concept oldFiller = ((ExistentialRoleRestriction) conc).filler();
                Role concRole = ((ExistentialRoleRestriction) conc).role();

                Concept filler = DLHelpers.conjunction(
                        JavaConverters.asScalaSet(pullOutTopRole(oldFiller))); // safe recursion: one nesting level down

                if(filler instanceof ConceptConjunction) {
                    // exists r.(C and exists topRole D) -> (exists r.C and exists topRole. D)
                    Set<Concept> fillerConjuncts = new HashSet<Concept>(JavaConverters.setAsJavaSet(((ConceptConjunction)filler).conjuncts()));

                    for(Concept conj:JavaConverters.setAsJavaSet(((ConceptConjunction) filler).conjuncts())) {
                        if(conj instanceof ExistentialRoleRestriction) {
                            Role conjRole = ((ExistentialRoleRestriction) conj).role();
                            if(conjRole instanceof BaseRole && ((BaseRole) conjRole).name().equals("TOP-ROLE")) {
                                fillerConjuncts.remove(conj);

                                // this has been flattened, then,
                                flattenedConc.add(new ExistentialRoleRestriction(conjRole, ((ExistentialRoleRestriction)conj).filler()));
                                foundNestedTopRole = true;
                            }
                        }
                    }
                    // the rest is flattened due to the recursive call
                    flattenedConc.add(new ExistentialRoleRestriction(concRole, new ConceptConjunction(JavaConverters.asScalaSet(fillerConjuncts).toSet())));
                }
                else if(filler instanceof ConceptDisjunction) {
                    // always split disjunctions
                    // the commented code below is not sound: exists r.(A or exists topRole.B) is not equivalent to
                    // (exists r.A or exists topRole.B), since the latter can be true without an r-successor, while the first cannot
                    java.util.Set<Concept> disjuncts = new HashSet<>();
                    for(Concept c:JavaConverters.asJavaCollection(((ConceptDisjunction)filler).disjuncts())){
                        disjuncts.add(new ExistentialRoleRestriction(concRole, c));
                    }

                    // this is also added to unflattened, as we might have to pull immediate occurrences out
                    unflattenedConcepts.add(new ConceptDisjunction(
                            JavaConverters.asScalaSet(disjuncts).toSet()));
                    /*
                    java.util.Set<Concept> fillerDisjuncts = new HashSet<Concept>(JavaConverters.setAsJavaSet(((ConceptDisjunction)filler).disjuncts()));
                    java.util.Set<Concept> newDisjuncts = new HashSet<Concept>();
                    for(Concept disj:JavaConverters.setAsJavaSet(((ConceptDisjunction)filler).disjuncts())) {
                        if(disj instanceof ExistentialRoleRestriction) {
                            Role disjRole = ((ExistentialRoleRestriction) disj).role();
                            if(disjRole instanceof BaseRole && ((BaseRole) disjRole).name().equals("TOP-ROLE")) {
                                fillerDisjuncts.remove(disj);
                                foundNestedTopRole = true;
                                newDisjuncts.add(new ExistentialRoleRestriction(disjRole, ((ExistentialRoleRestriction)disj).filler()));
                            }
                        }
                    }
                    //if(foundNestedTopRole = true) {
                    if(fillerDisjuncts.size() > 1) {
                        Concept conc1 = new ExistentialRoleRestriction(concRole, new ConceptDisjunction(JavaConverters.asScalaSet(fillerDisjuncts).toSet()));
                        newDisjuncts.add(conc1);
                    }
                    else if(fillerDisjuncts.size() == 1) {
                        Concept conc1 = new ExistentialRoleRestriction(concRole, fillerDisjuncts.iterator().next());
                        newDisjuncts.add(conc1);
                    }
                    // if fillerDisjuncts.size()==0, we have exists r.BOTTOM=BOTTOM, which can be excluded from the conjunction

                    iterator.add(new ConceptDisjunction(JavaConverters.asScalaSet(newDisjuncts).toSet()));
                    iterator.previous();

                     */
                }
                else if(filler instanceof ExistentialRoleRestriction) {
                    Role fillerRole = ((ExistentialRoleRestriction)filler).role();
                    // exists r.exists topRole.A -> (exists r.TOP and exists topRole.A)
                    if(fillerRole instanceof BaseRole && ((BaseRole)fillerRole).name().equals("TOP-ROLE")) {
                        Concept top = uk.ac.man.cs.lethe.internal.dl.datatypes.TopConcept$.MODULE$;
                        Concept conc1 = new ExistentialRoleRestriction(concRole, top);
                        Concept conc2 = filler;

                        // this is exactly the flattening operation, which is why we add to flattenedConc
                        flattenedConc.add(conc1);
                        flattenedConc.add(conc2);
                        flattenedConc.add(new ConceptDisjunction(JavaConverters.asScalaSet(new HashSet<Concept>(Arrays.asList(conc1, conc2))).toSet()));
                        foundNestedTopRole = true;
                    }
                    else {
                        // nothing to be flattened, add original concept
                        flattenedConc.add(conc);
                    }
                } else {
                    // the remaining cases are universal restriction, nominal set, concept name and negated concept name
                    // as we called the flattening operation recursively, this means there was nothing to be flattened
                    flattenedConc.add(conc);
                }
            }  else {
                // the remaining cases are nominal set, concept name and negated concept name
                // as there cannot possibly be any universal roles nested in those, we can add to flattened
                flattenedConc.add(conc);
            }


            beenChecked.add(conc);
        }
        return flattenedConc;
    }

    private Set<DisjunctiveAssertion> applyDistributivity(DisjunctiveAssertion as) {
        java.util.Set<DisjunctiveAssertion> distributedAssertions = new HashSet<DisjunctiveAssertion>();

        List<DisjunctiveAssertion> undistributedAssertions = new ArrayList<DisjunctiveAssertion>(Arrays.asList(as));
        ListIterator<DisjunctiveAssertion> iterator = undistributedAssertions.listIterator();
        List<DisjunctiveAssertion> beenChecked = new ArrayList<DisjunctiveAssertion>();

        while(iterator.hasNext()) {
            DisjunctiveAssertion currentAs = iterator.next();
            boolean distributionFound = false;

            if(beenChecked.contains(currentAs)) {
                continue;
            }

            for(Assertion disjunct : JavaConverters.setAsJavaSet(currentAs.disjuncts())) {

                if(!(disjunct instanceof ConceptAssertion)) {
                    //continue;
                }
                else {
                    if (((ConceptAssertion) disjunct).concept() instanceof ConceptConjunction) {
                        distributionFound = true;

                        ConceptConjunction disjConc = (ConceptConjunction) ((ConceptAssertion) disjunct).concept();
                        Set<Concept> conjuncts = JavaConverters.setAsJavaSet(((ConceptConjunction) disjConc).conjuncts());

                        java.util.Set<Assertion> otherDisjuncts = new HashSet<Assertion>(JavaConverters.setAsJavaSet(currentAs.disjuncts()));
                        otherDisjuncts.remove(disjunct);

                        for (Concept conj : conjuncts) {
                            Assertion conjAs = new ConceptAssertion(conj, ((ConceptAssertion) disjunct).individual());
                            otherDisjuncts.add(conjAs);
                            iterator.add(new DisjunctiveAssertion(JavaConverters.asScalaSet(otherDisjuncts).toSet()));
                            otherDisjuncts.remove(conjAs);
                            iterator.previous();
                        }
                    }
                }
            }
            if(distributionFound == false) {
                distributedAssertions.add(currentAs);
            }
            beenChecked.add(currentAs);
        }
        return distributedAssertions;
    }
}
