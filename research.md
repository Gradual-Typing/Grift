# Research Schedule for Schml

## PLDI 2016 Submission

As of 7/15/2015 there are 16 weeks until the submission deadline.
The following schedule is based on trying to accomplish the paper's
goals with 2 weeks to spare in order to polish the submitted prose.

The goals for the submission are as follow:
+ Thorough Analysis of Function Calls
   + Eta-Expansion Representation of Function Casts (ERFC)
   + Data Representation of Function Casts (DRFC) / Not Space Efficient (NSE)
   + DRFC / Space Efficient (SE)
   + Hybrid Representation of Function Casts (HRFC)
+ Thorough Analysis of Mutable References
   + Guarded Semantics for Mutable References (GSMR)
      + SE / NSE
      + Single References
      + Arrays
   + Monotonic Semantics for Mutable References (MSMR)
      + Single References
      + Arrays
+ Understanding of Where Future Research is Heading
   + The Role of Standard Optimizations
      + Type Inference
      + Hash Cons Types / Coercions
      + Inlining to Avoid Casts
      + Closure Optimizations
   + New Semantics that Enable More Efficient Compilation
   + Interplay With Standard Language Features
      + Parametric Polymorphism / Type Case
   
The following is a rough time line subject to extensive modification
as we gain experience and wisdom. Weeks are dated be the Monday
starting each week.

0.  (20/07/15) Preliminary Framework Setup
   + Tidy up current benchmarks according to Jeremy's Feedback. (impl / artifact)
      + Standardize Graphing Using gnuplot
      + Some Way of Measuring the Accuracy of the timer?
      + Loop overhead mesurements
         + C for loop
         + C recursive loop
         + Schml repeat loop
         + Schml recursive loop
      + ERC Function Call Measurement
         + OCaml/Haskell?
         + Gambit/Chicken?
         + C
      + Guarded Non-SE
         + ?
   + Write Outline of Paper (paper)
      + Draft the paper position, character, tone, etc.
   + Write a benchmarking framework that keeps track of results
      indexed by commits.
1.  (27/07/15) HRFC + GRMR Arrays 
   + I am at PLT Redex Summer School this week
   + Draft Implementation Details of HRFC (Paper)
2.  (20/07/15) HRFC + GRMR Arrays 
   + Implement HRFC (impl)
   + Debug GRMR Arrays
3.  (03/08/15) HRFC + GRMR / SE
   + Implement HRFC
4.  (10/08/15) HRFC + GRMR / SE
   + Debug HRFC
   + Review Implementation Documentation in the Paper
5.  (17/08/15) DRC
6.  (24/08/15) DRC
7.  (31/08/15) Space Efficiency
8.  (07/09/15) Space Efficiency
9.  (14/09/15) Space Efficiency
10. (21/09/15) Space Efficiency
11. (28/09/15) Space Efficiency
12. (05/10/15) Space Efficiency
13. (12/10/15) Space Efficiency
14. (19/10/15) Polish All Implementation Work.
15. (26/10/15) Polish Writing / Artifact
16. (02/11/15) Polish Writing / Artifact

