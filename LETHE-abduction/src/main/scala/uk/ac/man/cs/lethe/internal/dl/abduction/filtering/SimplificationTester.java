//package uk.ac.man.cs.lethe.internal.dl.abduction.filtering;

//import org.semanticweb.owlapi.model.OWLOntologyCreationException;

/*
public class SimplificationTester {
    private static AbductionSimplifier simplifier = new AbductionSimplifier();


    public static void main(String[] args) throws OWLOntologyCreationException {
        //nnfTest();
        //nestedConjunctionTest();
        //nestedUniversalRoleTest();
        //nestedUniversalRoleTest2();
        //distributivityTest();
        complexDisjunctiveAssertionTest();
    }

    public static void nnfTest() {
        BaseConcept a = new BaseConcept("A");
        BaseConcept b = new BaseConcept("B");
        BaseRole r = new BaseRole("r");
        Individual ind = new Individual("a");


        ConceptComplement exp1 = new ConceptComplement(new ExistentialRoleRestriction(r, new ConceptDisjunction(JavaConverters.asScalaSet(new HashSet<Concept>(Arrays.asList(a, DLHelpers.neg(b)))).toSet())));
        Assertion ax1 = new ConceptAssertion(exp1, ind);
        DisjunctiveAssertion conjunct1 = new DisjunctiveAssertion(JavaConverters.asScalaSet(new HashSet<Assertion>(Arrays.asList(ax1))).toSet());

        BaseConcept c = new BaseConcept("C");
        BaseConcept d = new BaseConcept("D");
        Individual ind2 = new Individual("b");
        Individual ind3 = new Individual("c");
        Assertion ax2 = new ConceptAssertion(c, ind2);
        Assertion ax3 = new ConceptAssertion(d, ind3);

        DisjunctiveAssertion conjunct2 = new DisjunctiveAssertion(JavaConverters.asScalaSet(new HashSet<Assertion>(Arrays.asList(ax2,ax3))).toSet());

        scala.collection.immutable.Set<DisjunctiveAssertion> testForgettingSolution = JavaConverters.asScalaSet(new HashSet<Assertion>(Arrays.asList(conjunct1,conjunct2))).toSet();//, conjunct1))).toSet();
        java.util.Set<DisjunctiveAssertion> flattenedForgettingSolution = simplifier.simplify(testForgettingSolution);

        System.out.println("====================");
        System.out.println("~NNF~");
        System.out.println("Original: " + testForgettingSolution);
        System.out.println("Flattened: " + flattenedForgettingSolution);
    }

    public static void nestedUniversalRoleTest() {

        BaseConcept a = new BaseConcept("A");
        BaseConcept b = new BaseConcept("B");
        BaseConcept c = new BaseConcept("C");
        BaseConcept d = new BaseConcept("D");

        TopRole$ topRole = TopRole$.MODULE$;

        Individual freshInd = new Individual("a*");

        Concept nestedExis = new ExistentialRoleRestriction(topRole, new ConceptConjunction(JavaConverters.asScalaSet(new HashSet<Concept>(Arrays.asList(a, DLHelpers.neg(b)))).toSet()));
        Assertion ax1 = new ConceptAssertion(new ExistentialRoleRestriction(topRole, new ConceptDisjunction(JavaConverters.asScalaSet(new HashSet<Concept>(Arrays.asList(c, DLHelpers.neg(b), nestedExis))).toSet())), freshInd);
        DisjunctiveAssertion conjunct1 = new DisjunctiveAssertion(JavaConverters.asScalaSet(new HashSet<Assertion>(Arrays.asList(ax1))).toSet());

        scala.collection.immutable.Set<DisjunctiveAssertion> testForgettingSolution = JavaConverters.asScalaSet(new HashSet<Assertion>(Arrays.asList(conjunct1))).toSet();//, conjunct1))).toSet()
        java.util.Set<DisjunctiveAssertion> flattenedForgettingSolution = simplifier.simplify(testForgettingSolution);

        System.out.println("====================");
        System.out.println("~Nested universal roles~");
        System.out.println("Original: " + testForgettingSolution);
        System.out.println("Flattened: " + flattenedForgettingSolution);
    }

    public static void nestedUniversalRoleTest2() {
        BaseConcept a = new BaseConcept("A");
        BaseConcept b = new BaseConcept("B");
        BaseConcept c = new BaseConcept("C");
        BaseConcept d = new BaseConcept("D");
        BaseRole r = new BaseRole("r");
        BaseRole s = new BaseRole("s");

        TopRole$ topRole = TopRole$.MODULE$;

        Individual ind = new Individual("a");

        Concept nestedExis = new ExistentialRoleRestriction(topRole, new ConceptConjunction(JavaConverters.asScalaSet(new HashSet<Concept>(Arrays.asList(a, DLHelpers.neg(b)))).toSet()));
        Assertion ax1 = new ConceptAssertion(new UniversalRoleRestriction(r, new ConceptConjunction(JavaConverters.asScalaSet(new HashSet<Concept>(Arrays.asList(a, nestedExis))).toSet())), ind);
        DisjunctiveAssertion conjunct1 = new DisjunctiveAssertion(JavaConverters.asScalaSet(new HashSet<Assertion>(Arrays.asList(ax1))).toSet());

        scala.collection.immutable.Set<DisjunctiveAssertion> testForgettingSolution = JavaConverters.asScalaSet(new HashSet<Assertion>(Arrays.asList(conjunct1))).toSet();//, conjunct1))).toSet()
        java.util.Set<DisjunctiveAssertion> flattenedForgettingSolution = simplifier.simplify(testForgettingSolution);

        System.out.println("====================");
        System.out.println("~Nested universal roles case 2~");
        System.out.println("Original: " + testForgettingSolution);
        System.out.println("Flattened: " + flattenedForgettingSolution);
    }

    public static void nestedConjunctionTest() {
        BaseConcept a = new BaseConcept("A");
        BaseConcept c = new BaseConcept("C");
        BaseConcept d = new BaseConcept("D");
        BaseRole r = new BaseRole("r");
        Individual ind = new Individual("a");

        Concept subConc1 = new UniversalRoleRestriction(r, new ConceptConjunction(JavaConverters.asScalaSet(new HashSet<Concept>(Arrays.asList(c, d))).toSet()));
        Assertion ax1 = new ConceptAssertion(new ConceptConjunction(JavaConverters.asScalaSet(new HashSet<Concept>(Arrays.asList(a, subConc1))).toSet()), ind);
        //Assertion ax1 = new ConceptAssertion(new UniversalRoleRestriction(r, new ConceptConjunction(JavaConverters.asScalaSet(new HashSet<Concept>(Arrays.asList(c, d))).toSet())), ind);
        DisjunctiveAssertion conjunct1 = new DisjunctiveAssertion(JavaConverters.asScalaSet(new HashSet<Assertion>(Arrays.asList(ax1))).toSet());

        scala.collection.immutable.Set<DisjunctiveAssertion> testForgettingSolution = JavaConverters.asScalaSet(new HashSet<Assertion>(Arrays.asList(conjunct1))).toSet();//, conjunct1))).toSet()
        java.util.Set<DisjunctiveAssertion> flattenedForgettingSolution = simplifier.simplify(testForgettingSolution);

        System.out.println("====================");
        System.out.println("~Nested Conjunction~");
        System.out.println("Original: " + testForgettingSolution);
        System.out.println("Flattened: " + flattenedForgettingSolution);
    }


    public static void distributivityTest() {
        BaseConcept a = new BaseConcept("A");
        BaseConcept b = new BaseConcept("B");
        BaseConcept c = new BaseConcept("C");

        BaseRole r = new BaseRole("r");
        Individual ind1 = new Individual("a");
        Individual ind2 = new Individual("b");

        Assertion ax1 = new ConceptAssertion(new UniversalRoleRestriction(r, new ConceptConjunction(JavaConverters.asScalaSet(new HashSet<Concept>(Arrays.asList(b, c))).toSet())), ind1);
        Assertion ax2 = new ConceptAssertion(a, ind2);
        DisjunctiveAssertion conjunct1 = new DisjunctiveAssertion(JavaConverters.asScalaSet(new HashSet<Assertion>(Arrays.asList(ax1, ax2))).toSet());

        scala.collection.immutable.Set<DisjunctiveAssertion> testForgettingSolution = JavaConverters.asScalaSet(new HashSet<Assertion>(Arrays.asList(conjunct1))).toSet();//, conjunct1))).toSet()
        java.util.Set<DisjunctiveAssertion> flattenedForgettingSolution = simplifier.simplify(testForgettingSolution);

        System.out.println("====================");
        System.out.println("~Distributivity~");
        System.out.println("Original: " + testForgettingSolution);
        System.out.println("Flattened: " + flattenedForgettingSolution);
    }

    public static void complexDisjunctiveAssertionTest() {
        BaseConcept a = new BaseConcept("A");
        BaseConcept b = new BaseConcept("B");
        BaseConcept c = new BaseConcept("C");
        BaseConcept d = new BaseConcept("D");
        BaseConcept e = new BaseConcept("D");
        BaseConcept f = new BaseConcept("F");
        BaseConcept g = new BaseConcept("G");
        BaseConcept h = new BaseConcept("H");

        BaseRole r = new BaseRole("r");
        TopRole$ topRole = TopRole$.MODULE$;

        Individual freshInd = new Individual("a*");
        Individual ind2 = new Individual("b");
        Individual ind3 = new Individual("c");

        ExistentialRoleRestriction subNestedExis1 = new ExistentialRoleRestriction(topRole, new ConceptConjunction(JavaConverters.asScalaSet(new HashSet<Concept>(Arrays.asList(g, DLHelpers.neg(h)))).toSet()));
        ExistentialRoleRestriction nestedExis1 = new ExistentialRoleRestriction(topRole, new ConceptConjunction(JavaConverters.asScalaSet(new HashSet<Concept>(Arrays.asList(subNestedExis1, e, DLHelpers.neg(f)))).toSet()));

        ConceptComplement exp1 = new ConceptComplement(new ExistentialRoleRestriction(r, new ConceptDisjunction(JavaConverters.asScalaSet(new HashSet<Concept>(Arrays.asList(a, DLHelpers.neg(b)))).toSet())));

        Assertion ax1 = new ConceptAssertion(new ExistentialRoleRestriction(topRole, new ConceptConjunction(JavaConverters.asScalaSet(new HashSet<Concept>(Arrays.asList(c, DLHelpers.neg(d), nestedExis1))).toSet())), freshInd);
        Assertion ax2 = new ConceptAssertion(new ConceptDisjunction(JavaConverters.asScalaSet(new HashSet<Concept>(Arrays.asList(exp1, c))).toSet()), ind2);
        Assertion ax3 = new NegatedRoleAssertion(r, ind2, ind3);
       // Assertion ax3 = new ConceptAssertion(g, ind3);

        DisjunctiveAssertion conjunct1 = new DisjunctiveAssertion(JavaConverters.asScalaSet(new HashSet<Assertion>(Arrays.asList(ax1, ax3))).toSet());
        DisjunctiveAssertion conjunct2 = new DisjunctiveAssertion(JavaConverters.asScalaSet(new HashSet<Assertion>(Arrays.asList(ax2))).toSet());

        scala.collection.immutable.Set<DisjunctiveAssertion> testForgettingSolution = JavaConverters.asScalaSet(new HashSet<Assertion>(Arrays.asList(conjunct1))).toSet();//, conjunct1))).toSet();
        java.util.Set<DisjunctiveAssertion> flattenedForgettingSolution = simplifier.simplify(testForgettingSolution);

        System.out.println("====================");
        System.out.println("~Complex disjunctive assertion~");
        System.out.println("Original: " + testForgettingSolution);
        System.out.println("Flattened: " + flattenedForgettingSolution);
    }

}*/
