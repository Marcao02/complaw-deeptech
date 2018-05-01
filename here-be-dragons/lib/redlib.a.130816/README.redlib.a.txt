README for REDLIB.  

README file for REDLIB version 1

----------------------------------------------------------------

This package contains the materials for the library executable of 
REDLIB. 
It contains the following four files. 

1. README.redlib.a.txt
   That is this file. 
   
2. redlib.a 
   The executable for REDLIB. 
   
3. redlib.h
   The header file to be included for the 
   constants, types, macro definitions that can be used with 
   redlib.a. 
   
4. redlib.e 
   The external declaraions for variables and procedures that 
   can be used with redlib.a.  

5. redlib.pdf 
   This is a user manual for REDLIB. 
   This information is not up-to-date.  
   There can be some features and functions of the newest REDLIB 
   that are not written in the manual.  


****************************************
Change log: 

=================================
Notes on 12 Dec. 2009

In enhancing the capability of our symbolic simulator pathg, 
we have implemented the following new functions to REDLIB. 
 1. As you might have known, in pathg, at each step, it shows the 
    accumulated execution time along a path. 
    This is implemented with a special clock TIME 
    in REDLIB with large time-bounds. 
    This was kind of incompatible with the way the other clocks were 
    manipulated.  
    All the other clocks are used the same bound derived from the biggest 
    timing constants used in the model and in the spec. 
    Thus clock TIME needs some exceptional manipulations in various procedures. 
    In previous version of REDLIB, some of the procedures were not 
    updated to manipulate the exceptional cases with clock TIME. 
    Such bugs were exposed in the usage of pathg. 
    This new version has many such bugs removed.  
    However REDLIB now is huge now.  
    We do not have the confidence that all the procedures have been updated 
    to take care of the exceptional cases with clcok TIME.  
    If you found any anomalies that looks suspicious, we will appreciate 
    your notification with detailed error traces and models. 
    
 2. To make pathg a more powerful simulator, we have enhanced 
    its procedures for oracle testing.  
    Thus we have provided a new version of procedure check_oracle() 
    in file pathgame_strategy.c.  
    With the new version, users can check if the current state predicate 
    satisfies some properties.  
 
 3. To make pathg an even more powerful simulator, we have implemented 
    a backtrack command that can be issued in each step.  
    With this option, now the users can move forward, backtrack one step, 
    and reset to the initial state.  
    We believe this will be useful in exploring the state space and 
    constructing test plans.  
 
 4. Also, the counter-example capabilities of forward reachability analysis
    did not really work. 
    In this new version, we have fixed the bug. 
    


=================================
Notes on 22 Feb. 2010

There have been the following enhancements to REDLIB. 
 1. We have implemented stream data types for test data input 
    with pathg. 
    However, all such features are disabled in model-checking, 
    simulation-checking, and reachability analysis at the moment. 
 2. We have revised the procedures for some preprocessing techniques 
    of the models. 
    These include the simple procedures for over-approximation of 
    reachable modes and firable transitions of each process. 
    Especially, now we can analysis those mode changes incurred 
    by actions like `mode = v;' where v is a variable. 
    Such actions are essential in our model generated for 
    C/C++ programs with procedure calls. 
 3. We have also enhanced the procedures for analyzing the inactive 
    variables.  
 
 
=================================
Notes on 11 Dec. 2009

There were some features that may be against the intuition of 
users. 
 1. The reachability returned with red_reach_fwd() and red_reach_bck() 
    when no full reachability is optined and an counter example is constructed 
    does not include the states in the last iteration.  
 2. The counter example generation for forward analysis did not quite 
    work. 
We have changed the features to be more compatible with the users' 
intuition.  

=================================
Notes on 08 Dec. 2009

In the last update, we have changed the option interaction 
regarding abstraction, counter-example generation, and reachability 
construction. 
Unfortunately, we missed the change of some post condition procedure 
invocations.  
As a result, some post conditions are either calculated incorrectly or 
incur core dump. 
We have fixed the bug in this new version. 


=================================
Notes on 07 Dec. 2009

In response to some users' feedback, 
we have make the following changes. 

 1. We have moved some of the preliminary analysis for model-checking 
    from offline to runtime so that the preliminary analysis does not 
    become a burden in running the symbolic simulator pathg. 
 2. There was a conflict in the approximation options, the counter-example 
    options, and full reachability options.  
    The conflict is as follows. 
    2a. If an over-approximation was used, then no counter-example would
        be generated since REDLIB would think the reachability was 
        inconclusive and the counter-example could be false. 
        We now change the implementation so that counter-examples may still 
        be generated with option over-approximation.  
        We believe this is more intuitive for using the approximation options 
        and can help the users carrying out experiment about CEGAR techniques. 
    2b. Also in the before, if users specify option for full reachability, 
        then counter-example option is disabled.  
        The reasoning of REDLIB for this implementation decision was that 
        full reachability could be generated at a deep least fixpoint 
        iteration while the counter-example may be generated at a shallow 
        least fixpoint iteration. 
        To avoid the confusion, so REDLIB chose not generate counter examples
        when full reachability option was selected. 
        But we feel that this could be less than intuitive and may restrict 
        the innovation of the users in using REDLIB. 
        So now will generate a counter-example, maybe at a shallow least 
        fixpoint iteration, even if the full reachability is constructed 
        at a deep least fixpoint iteration.  

=================================
Notes on 26 Nov. 2009

In the execution of pathg, we use a diagram, say SYNC_ALL, that 
records the synchronization among all transitions.  
SYNC_ALL also records the triggering conditions of all the transitions.  
SYNC_ALL was constructed before the verification tasks, 
e.g. symbolic simulation or model-checking, of the model are started. 
It turns out that the large number of transitions and complex triggering 
conditions incurs huge memory consumption in the construction of SYNC_ALL.  
We have thus decided to bypass the construction of SYNC_ALL and 
analyse the synchronous transitions that can be triggered at a step online.  
We also found that SYNC_ALL is actually mainly used with pathg. 
So the change does not affect red and the other redlib applications.  


=================================
Notes on 12 Nov. 2009

We have changed a bug in red_reach_bck() that 
does not set the value of rr->reachability correctly.  


=================================
Notes on 7 Nov. 2009

We have modified REDLIB for the following purpose. 
 1. To integrate the modules of REDLIB parser so that 
    all parsing are through the same flow.  
 2. To maintain the original form of model input when 
    users want to reconstruct the model from REDLIB. 
    Before this version, REDLIB would remove all macro constants, 
    special constant symbols, and inline expressions when users 
    query for the model structure.  
    Now we add an option to red_input_model(), 
    red_input_rules(), red_end_declaration(), and 
    red_change_declaration().  

=================================
Notes on 5 Nov. 2009

We have fixed some bugs related to new experiment conducted with REDLIB. 

=================================
Notes on 31 Oct. 2009

We have added capability of file inclusion in 
the model file of RED models. 
The users can write C-like compile time commmand in the following. 

#include "file_name"

Then RED will create an intermediate representation with file_name 
inserted.  
The file inclusion capability of RED now allows for recursive 
file inclusion.  
However, duplicate file inclusion will be signaled and only done once. 
Also C-like comments are allowed freely to be used 
with the file inclusion command.  


=================================
Notes on 20 Oct. 2009

There was a lot of update notes for previous versions of REDLIB.   
But due to the unexpected revision of SourceFore, 
we lost all the previous update notes. 
So now we learn to be smart by relying on the service of SourceForge
as little as possible. 
So from now on we will have this README file in the downloadable package 
of REDLIB and we will have the update logs directly put down in this 
README file from now on. 

In this new version of REDLIB, we have experimented with some 
techniques for the efficient evaluation of word-level constraints.  
The techniques are not based on bounded model-checking or BMD.  
We have also fixed some bugs of REDLIB.  



=======================================================
August 7, 2009

There was some bugs in REDLIB regarding the 
support for pathg. 
The bug is about the degenerate case when there is no clock 
or when the system is untimed. 
Then the bug is that pathg still wants to measure time progress 
after each transition and access some clocks. 
Such clocks of course do not exist in the degenerate case. 

We have fixed the bugs.  


=================================
Notes on 081005: 

In this new version, we have substantially extend the interface of 
the RED modeling language to industry applications. 
There are three main changes. 

1. Attributes for graph drawing: 
   We have added new attributes to the RED modeling language 
   for the drawing of the graphs. 
   This includes the shape of the modes, the color of the modes, 
   the coordinates of the modes, the nail coordinates of the transitions, 
   and the labeling of the modes and transitions.  
   
2. Inline functions: 
   Now in RED, we can declare and use inline function calls. 
   This can be very convenient in writing complex control mechanism. 
   Users can declare two types of inline functions. 
   One is Boolean and the other is discrete. 
   The first can be used as predicates while the second as discrete functions 
   in invariances and triggers.  
   
3. Conditional expressions: 
   Now we also extend the modeling language with C-like conditional 
   expressions with the following format. 
   
      (condition) ? exp1 : exp2 
      
   condition is any state-predicate of RED.  
   exp1 and exp2 are two discrete expressions.  
   If condition is evalauted true, then the expression behaves like exp1. 
   Otherwise, it behaves like exp2.  
   
Another minor feature change is with quantification expressions. 
In previous version, when we write a quantified expression like 

    forall i, condition
    
    forall i:i>5, condition 
    
    exists i, condition
    
    exists i:i<7, condition
    
The range of i is always interpreted to be in [1, #PS] where 
#PS is the number of processes. 
This syntax design is restrictive and cannot support the complex 
constraints on any finite ranges. 
Thus in this version, we have relaxed the syntax to the following. 

    forall i:u..w, condition 
    
    exists i:u..w, condition 
    
Here u and w are two constant expressions that explicitly state the 
lower-bound and upper-bound of the quantified variable i.  


=================================
Notes on 080615: 

Again we found that we have crashed the bisimulation capabilities for untimed systems while we recently experimented with some new techniques for TCTL model-checking. 
We will thus include some untimed benchmarks in our 
regresstional testing file. 

===================================
Notes on 080612: 

While experimenting with some techniques to improve the 
model-checking performance of RED, we accidentally 
disabled the simulation/bisimulation checking capability 
of RED. 
Also, we found that the new procedure in REDLIB 
that allows for the input of new modes and rules 
does not take into consideration of the changes of 
  . the value ranges of the transition variable and 
  . the value ranges of the mode variable 
for each process.  
So we now fix the bugs. 


====================================
Notes 080602: 

We have implemented several speed-up techniques for the model-checking of timed inevitabilities of the following forms. 

  forall eventually {[c,d]} p

or 

  exists always {[c,d]} p


====================================
Notes on 080518: 

In response to Mondale Wang's request, we have 
reimplemented procedure red_diagram_discrete_model_count 
so that now it returns the number of all solutions to the 
user-declared discrete variables in the parameter 
diagram.  

=======================================
Notes on 080518: 

Due to the request of Mondale Wang, I have reimplemented 
procedure red_diagram_discrete_model_count() so that now 
it calculates the number of solutions to the user-declared
discrete variables. 


=======================================
Notes on 080508: 

In comparison with the previous releases, there are the following two changes. 

1. We have tested our simulation checking capabilities for 
   an untimed benchmark.  As a result, we found that some 
   procedures may access some data structures that are 
   allocated only for timed systems and run into 
   segmentation faults.  We have removed some bugs in this 
   aspect to make RED also applicable for untimed system 
   simulation checking. 

2. We have identified a bug in a zone normalization 
   procedure.  In general, the corresponding normal form 
   is not the default one.   Thus the bugs may not 
   affect the correctness of the function of our previous 
   releases.  


===========================================
Notes on 080506: 

In the previous release, to make a consistent presentation 
of counter examples for both LHA and TA, we have added a 
new parameter to a precondition calculation procedure.  
However, we did not check all the files that use this 
procedure.  As a result, the simulation checking and 
bisimulation checking capabilities were destroyed.  
In this new version, we have corrected the bug.  

===========================================
Notes on 080504:

This version supports the following verification tasks. 

1. for timed automata 

   a. safety/risk analysis, 
   b. TCTL model-checking, 
   c. simulation checking, and 
   d. bisimulation checking.  

2. for linear-hybrid automata 

   a. parametric risk analysis 

Note that we have not restored all the functionalities for address enforcers in the synchronizers





   
   
   