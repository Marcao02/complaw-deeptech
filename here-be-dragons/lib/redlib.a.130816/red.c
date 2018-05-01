#include <stdlib.h>
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <float.h>

#include "redlib.h"
#include "redlib.e"



extern FILE	*yyin;   
extern void	hsp(); 
  
int	flag_analysis_direction, 
#define FLAG_ANALYSIS_BACKWARD	0
#define	FLAG_ANALYSIS_FORWARD	1 

	flag_my_system_type, 
#define FLAG_MY_SYSTEM_UNTIMED	0
#define	FLAG_MY_SYSTEM_TIMED	1 
#define	FLAG_MY_SYSTEM_HYBRID	2 

	flag_normality, 
	flag_action_approx, 
	flag_reduction, 
        flag_time_progress_analysis, 
	flag_fairness_assumptions_eval, 
        flag_gfp_path,  
	flag_tconvexity_shared_partitions, 
        flag_time_progress_options, 
        flag_gfp_on_the_fly, 
	flag_lub_extrapolation,  
	flag_zeno,
	flag_approx, 
	flag_counter_example, 
	flag_full_reachability, 
	flag_simulation_reasoning, 
	flag_symmetry, 
	flag_print, 
	my_depth_enumerative_synchronization,  
	pc_command_line,  
	my_proc_count, 
	bound_reachability, 

#define	SAFETY_CHECK	1
#define	RISK_CHECK	2
#define GOAL_CHECK	3
#define DEADLOCK_CHECK	4
#define ZENO_CHECK	5
#define	TCTL_CHECK	6
#define BISIM_CHECK	7
#define SIM_CHECK	8

	task_type; 
	
char	*model_fname,  
	*spec_fname,
	*output_fname; 

int	my_status_initialize() { 
  int	i; 
  
  my_proc_count = -1; 
  flag_analysis_direction = 
    FLAG_ANALYSIS_BACKWARD; 
//    FLAG_ANALYSIS_FORWARD; 
    
  flag_my_system_type = RED_SYSTEM_UNTIMED; 

  flag_zeno = 
//      RED_PLAIN_NONZENO;
//      RED_APPROX_NONZENO;
      RED_ZENO_TRACES_OK; 
  flag_normality = RED_NORM_ZONE_MAGNITUDE_REDUCED; 
  flag_action_approx = RED_NO_ACTION_APPROX; 
  flag_reduction = RED_REDUCTION_INACTIVE; 
  flag_approx = RED_NOAPPROX; 
  flag_symmetry = RED_NO_SYMMETRY; 
  flag_print = RED_NO_PRINT; 
  flag_counter_example = RED_NO_COUNTER_EXAMPLE; 
  flag_full_reachability = RED_NO_FULL_REACHABILITY; 
  flag_time_progress_analysis = RED_TIME_PROGRESS_ANALYSIS_ADVANCED; 
  flag_tconvexity_shared_partitions = RED_TIME_TCONVEXITY_SHARED_PARTITIONS; 
//   flag_tconvexity_shared_partitions = RED_TIME_TCONVEXITY_NO_SHARED_PARTITIONS; 
//   flag_time_progress_options = RED_TIME_PROGRESS_FULL_FORMULATION; 
//   flag_time_progress_options = RED_TIME_PROGRESS_ON_THE_FLY_SHARED_SPLIT_CONVEXITIES; 
  flag_time_progress_options 
  = RED_TIME_PROGRESS_ADAPTIVE_SHARED_SPLIT_CONVEXITIES; 

  flag_fairness_assumptions_eval = RED_GFP_EASY_STRONG_FAIRNESS; 
  flag_gfp_path = RED_GFP_PATH_INVARIANCE; 

//  flag_gfp_on_the_fly = RED_GFP_ON_THE_FLY; 
  flag_gfp_on_the_fly = RED_GFP_COMBINATONAL; 

  flag_lub_extrapolation = 0; 
  flag_simulation_reasoning = RED_SIMULATION_NEG_EXISTENTIAL; 
  
  my_depth_enumerative_synchronization = RED_DEPTH_ENUMERATIVE_DEFAULT; 

  pc_command_line = -1; 

  model_fname = "STDIN"; 
  spec_fname = "STDIN"; 
  output_fname = "STDOUT"; 
  
  task_type = 
//      SIM_CHECK; 
      RISK_CHECK; 
} 
  /* my_status_initialize() */ 
  
  

int	my_process_command_line(argc, argv) 
	int	argc; 
	char	**argv; 
{ 
  int	i, j, k, file_count, value; 
  
  red_register_arg(argc, argv); 
  
  my_status_initialize(); 
  
  for (file_count = 0, i = 1; i < argc; i++) {
/*
    printf(
      "\nChecking command-line argument %1d:(%s), len=%1d, argv[%1d][0]=%1d.\n", 
      i, argv[i], strlen(argv[i]), i, argv[i][0] 
    ); 
*/
    for (j = 0; j < strlen(argv[i]); j++) { 
      if (argv[i][j] <= ' ') {
        argv[i][j] = '0'; 
        break; 
      } 
    } 
    if (strlen(argv[i]) <= 0) { 
      continue; 
    } 
    else if (argv[i][0] != '-') { 
      switch (file_count) {
      case 0: 
//        printf("\nBefore receiving model fname.\n");  
        model_fname = argv[i]; 
//        printf("\nAfter receiving model fname.\n");  
        break; 
      case 1: 
//        printf("\nBefore receiving spec fname.\n");  
        spec_fname = argv[i];
//        printf("\nAfter receiving spec fname.\n");  
        break; 
      case 2: 
        output_fname = argv[i]; 
        RED_OUT = fopen(argv[i], "w"); 
        break; 
      } 
      file_count++;
    }
    else {
      for (j = 1; j < strlen(argv[i]); j++) {
	switch (argv[i][j]) {
	case '?': 
	  
	  exit(0); 
	case 'A': 
	  flag_approx = RED_OAPPROX_GAME_MAGNITUDE; 
	  break;
	  
	case 'C':
	  flag_counter_example = RED_COUNTER_EXAMPLE; 
	  break;
	case 'D': 
	  for (k = j+1; argv[i][k] >= '0' && argv[i][k] <= '9'; k++) {
	    argv[i][k-1] = argv[i][k]; 
	  } 
          argv[i][k-1] = '\0'; 
          bound_reachability = atoi(&(argv[i][j])); 
          if (value <= 0 || value > 255) { 
            printf("Error: out-of-range bound %1d for progression estimation.\n", 
		   value
		   ); 
	    exit(0); 	
          } 
          j = k;  
	  break; 
	case 'F':
	  flag_analysis_direction = FLAG_ANALYSIS_FORWARD;
	  break;
	  
	case 'H':
	  flag_my_system_type = RED_SYSTEM_HYBRID;
	  break;
	  
	case 'P': // For the number of processes to override the one in the 
		  // the template. 
	  for (k = j+1; argv[i][k] >= '0' && argv[i][k] <= '9'; k++) {
	    argv[i][k-1] = argv[i][k];
	  }
          argv[i][k-1] = '\0';
          pc_command_line = atoi(&(argv[i][j])); 
          my_proc_count = pc_command_line; 
          j = k; 
	  break; 
	  
	case 'R':
	  switch (argv[i][++j]) { 
	  case 'f': 
	    flag_full_reachability = RED_FULL_REACHABILITY;
	    break; 
	  case '\t': 
	  case ' ': 
	    j--; 
	    break; 
	  }
	  break;

	case 'S':
	  switch (argv[i][++j]) { 
	  case 'z': 
	    flag_symmetry = RED_SYMMETRY_ZONE; 
	    break; 
	  case 'd': 
	    flag_symmetry = RED_SYMMETRY_DISCRETE; 
	    break; 
	  case 'p': 
	    flag_symmetry = RED_SYMMETRY_POINTER; 
	    break; 
	  case 's': 
	    flag_symmetry = RED_SYMMETRY_STATE;
	    break; 
	  default: 
	    printf("\nCommand-line error: unrecognized symmetry option 'S%c'\n", argv[i][j]);
	    exit(0);
	  }
	  break; 
	  
	case 'T':
	  switch (argv[i][++j]) { 
	  case 's': 
	    task_type = SAFETY_CHECK; 
	    break; 
	  case 'r': 
	    task_type = RISK_CHECK; 
	    break; 
	  case 'g': 
	    task_type = GOAL_CHECK; 
	    break; 
	  case 'd': 
	    task_type = DEADLOCK_CHECK; 
	    break; 
	  case 'z': 
	    task_type = ZENO_CHECK; 
	    break; 
	  case 'm': 
	    task_type = TCTL_CHECK; 
	    break; 
	  case 'b': 
	    task_type = BISIM_CHECK; 
	    break; 
	  case 'i': 
	    task_type = SIM_CHECK; 
	    break; 
	  default: 
	    printf("\nCommand-line error: unrecognized task option 'T%c'\n", argv[i][j]);
	    exit(0);
	  }
	  break; 

	case 'X': // for experiments 
	  switch (argv[i][++j]) { 
	  case 'E':
	    // In the following, we move every digit one position ahead. 
	    // As a result, 'E' is overridden.  
	    // The reason is that we need to clear a position for the 
	    // terminating null character.  
	    k = j+1; 
	    if (argv[i][k] == '*') {
	    // This means enumeration-all. 
	      my_depth_enumerative_synchronization = RED_DEPTH_ENUMERATIVE_ALL; 
	      j = k+1; 
	    }
	    else { 
	      for (k = j+1; argv[i][k] >= '0' && argv[i][k] <= '9'; k++) {
	        argv[i][k-1] = argv[i][k];
	      }
              argv[i][k-1] = '\0';
              my_depth_enumerative_synchronization = atoi(&(argv[i][j]));
              j = k;
	    }
	    break;
          case 'a': 
            flag_time_progress_analysis  
            = RED_TIME_PROGRESS_ANALYSIS_NONE; 
            break; 
          case 'b': 
            flag_time_progress_analysis  
            = RED_TIME_PROGRESS_ANALYSIS_TCTCTL; 
            break; 

          case 'c': 
            flag_time_progress_analysis  
            = RED_TIME_PROGRESS_ANALYSIS_ADVANCED; 
            break; 
  
          case 'g': 
            flag_gfp_on_the_fly = RED_GFP_ON_THE_FLY; 
            break; 
          case 'h': 
            flag_gfp_on_the_fly = RED_GFP_COMBINATONAL; 
            break; 
  
          case 'k': 
            flag_time_progress_options 
            = RED_TIME_PROGRESS_ASSUMED_CONVEXITY; 
            break; 
            
  	  case 'l': 
	    flag_time_progress_options 
	    = RED_TIME_PROGRESS_FULL_FORMULATION; 
	    break; 
          case 'm': 
            flag_time_progress_options 
            = RED_TIME_PROGRESS_SHARED_CONCAVITY; 
            break; 
          case 'n': 
            flag_time_progress_options 
            = RED_TIME_PROGRESS_ADAPTIVE_SHARED_CONCAVITY; 
            break; 
          case 'o': 
            flag_fairness_assumptions_eval = 0; 
            break; 
          case 'p': 
            flag_fairness_assumptions_eval 
            = RED_GFP_EASY_STRONG_FAIRNESS; 
            break; 
/*
            flag_fairness_assumptions_eval 
            = RED_FAIRNESS_ASSUMPTIONS_EVAL_OCC_VAR; 
            break;  
          case 'p': 
            flag_fairness_assumptions_eval 
            = RED_FAIRNESS_ASSUMPTIONS_EVAL_CONCAT; 
            break; 
          case 'q': 
            flag_fairness_assumptions_eval 
            = RED_FAIRNESS_ASSUMPTIONS_EVAL_CONJ; 
            break; 
*/
          case 'r': 
            flag_gfp_path = RED_GFP_PATH_INVARIANCE; 
            break;  
          case 's': 
            flag_gfp_path = RED_GFP_PATH_FXP; 
            break; 

          case 't': 
            flag_lub_extrapolation 
            = RED_GLUB_EXTRAPOLATION; 
            break; 
          case 'u': 
            flag_lub_extrapolation 
            = RED_LUB_EXTRAPOLATION; 
            break; 

          case 'x':
            flag_simulation_reasoning = RED_SIMULATION_UNIVERSAL;  
            break; 
          case 'y': 
            flag_simulation_reasoning = RED_SIMULATION_NEG_EXISTENTIAL;  
            break; 
	  default: 
	    printf("\nCommand-line error: unrecognized symmetry option 'S%c'\n", argv[i][j]);
	    exit(0);
	  }
	  break; 

	case 'Z': 
	  flag_zeno = RED_PLAIN_NONZENO; 
	  break; 

        default: 
	  printf("\nCommand-line error: unrecognized option '%c'\n", argv[i][j]);
	  exit(0); 
	} 
      }
    } 
  }
//  printf("\nProcessing all %1d command-line arguments.\n", argc); 
  
  red_set_sync_bulk_depth(my_depth_enumerative_synchronization); 
  
  return(file_count); 
}
  /* my_process_command_line() */ 
  


  
int	my_verifier(tt, s)
	int	tt; // task type
	char	*s; // string for the spec. 
{
  int					NEGATED_SPEC, assume, pi, xi, 
  					deadlock, wreach;
  struct reachable_return_type		*rr; 
  struct sim_check_return_type		*sr; 
  struct model_check_return_type	*mr; 
  redgram				result, ds; 

  /* goal processing */ 

  switch (tt) {
  case BISIM_CHECK: 
    sr = red_bisim_check(
      red_query_diagram_initial(), 
      red_query_diagram_global_invariance(), 
      RED_FULL_REACHABILITY, 
      RED_NO_REACHABILITY_DEPTH_BOUND, 
      flag_counter_example, 
      RED_TIME_PROGRESS, 
      // RED_NORM_ZONE_CLOSURE  
      flag_normality, 
      flag_action_approx, 
      flag_reduction, 
      flag_approx, 
      flag_symmetry, 
      flag_zeno, 
        flag_tconvexity_shared_partitions 
      | flag_time_progress_options  
      | flag_gfp_on_the_fly 
      | flag_fairness_assumptions_eval 
      | flag_gfp_path
      | flag_simulation_reasoning, 
      flag_print,  
      s 
    ); 
    red_print_sim_check_return(sr); 
    return (sr->iteration_count); 
/*
    fprintf(RED_OUT, "\nThe equivalence states:\n"); 
    red_print_graph(RT[wreach]); 
*/
    break; 
  case SIM_CHECK: 
    sr = red_sim_check(
      red_query_diagram_initial(), 
      red_query_diagram_global_invariance(), 
      RED_FULL_REACHABILITY, 
      RED_NO_REACHABILITY_DEPTH_BOUND, 
      flag_counter_example, 
      RED_TIME_PROGRESS, 
      // RED_NORM_ZONE_CLOSURE  
      flag_normality, 
      flag_action_approx, 
      flag_reduction, 
      flag_approx, 
      flag_symmetry, 
      flag_zeno, 
        flag_tconvexity_shared_partitions 
      | flag_time_progress_options  
      | flag_gfp_on_the_fly 
      | flag_fairness_assumptions_eval 
      | flag_gfp_path 
      | flag_simulation_reasoning, 
      flag_print, 
      s 
    ); 
    red_print_sim_check_return(sr); 
/*
    fprintf(RED_OUT, "\nThe equivalence states:\n"); 
    red_print_graph(RT[wreach]); 
*/
    break; 
  case DEADLOCK_CHECK: 
    switch (red_query_system_type()) { 
    case RED_SYSTEM_HYBRID: 
      switch (flag_analysis_direction) { 
      case FLAG_ANALYSIS_FORWARD: 
        rr = red_reach_fwd( 
          red_query_diagram_initial(), 
          red_query_diagram_enhanced_global_invariance(), 
          red_query_diagram_deadlock(), // This is to be destroyed. 
          RED_TASK_DEADLOCK, 
          RED_PARAMETRIC_ANALYSIS, 
          RED_ALL_ROLES, 
          RED_FULL_REACHABILITY, 
          RED_NO_REACHABILITY_DEPTH_BOUND, 
          flag_counter_example, 
          RED_TIME_PROGRESS, 
          RED_NORM_HYBRID_2REDUNDANCY_ELIMINATION_DOWNWARD, 
          flag_action_approx, 
          flag_reduction, 
          RED_NOAPPROX, 
          RED_NO_SYMMETRY, 
            flag_tconvexity_shared_partitions 
          | flag_time_progress_options
          | flag_lub_extrapolation, 
          RED_NO_PRINT 
        );
        break; 
      case FLAG_ANALYSIS_BACKWARD: 
        rr = red_reach_bck( 
          red_query_diagram_initial(), 
          red_query_diagram_enhanced_global_invariance(), 
          red_query_diagram_deadlock(), // This is to be destroyed. 
          RED_TASK_DEADLOCK, 
          RED_PARAMETRIC_ANALYSIS, 
          RED_ALL_ROLES, 
          RED_FULL_REACHABILITY, 
          RED_NO_REACHABILITY_DEPTH_BOUND, 
          flag_counter_example, 
          RED_TIME_PROGRESS, 
          RED_NORM_HYBRID_2REDUNDANCY_ELIMINATION_DOWNWARD, 
          flag_action_approx, 
          flag_reduction, 
          RED_NOAPPROX, 
          RED_NO_SYMMETRY, 
            flag_tconvexity_shared_partitions 
          | flag_time_progress_options
          | flag_lub_extrapolation, 
          RED_NO_PRINT 
        );
        break; 
      default: 
        fprintf(RED_OUT, "\nIllegal analysis direction.\n"); 
        exit(0); 
      }
      break; 
    default: 
      switch (flag_analysis_direction) { 
      case FLAG_ANALYSIS_FORWARD: 
        rr = red_reach_fwd( 
          red_query_diagram_initial(), 
          red_query_diagram_enhanced_global_invariance(), 
          red_query_diagram_deadlock(), // This is to be destroyed. 
          RED_TASK_DEADLOCK, 
          RED_NO_PARAMETRIC_ANALYSIS, 
          RED_ALL_ROLES, 
          flag_full_reachability, // RED_NO_FULL_REACHABILITY, 
          RED_NO_REACHABILITY_DEPTH_BOUND, 
          flag_counter_example, 
          RED_TIME_PROGRESS, 
          RED_NORM_ZONE_MAGNITUDE_REDUCED, 
          flag_action_approx, 
          flag_reduction, 
          flag_approx, // RED_NOAPPROX, 
          RED_NO_SYMMETRY, 
            flag_tconvexity_shared_partitions 
          | flag_time_progress_options
          | flag_lub_extrapolation, 
          RED_NO_PRINT 
        );
        break; 
      case FLAG_ANALYSIS_BACKWARD: 
        rr = red_reach_bck( 
          red_query_diagram_initial(), 
          red_query_diagram_enhanced_global_invariance(), 
          red_query_diagram_deadlock(), // This is to be destroyed. 
          RED_TASK_DEADLOCK, 
          RED_NO_PARAMETRIC_ANALYSIS, 
          RED_ALL_ROLES, 
          flag_full_reachability, // RED_NO_FULL_REACHABILITY, 
          RED_NO_REACHABILITY_DEPTH_BOUND, 
          flag_counter_example, 
          RED_TIME_PROGRESS, 
          RED_NORM_ZONE_MAGNITUDE_REDUCED, 
          flag_action_approx, 
          flag_reduction, 
          flag_approx, // RED_NOAPPROX, 
          RED_NO_SYMMETRY, 
            flag_tconvexity_shared_partitions 
          | flag_time_progress_options
          | flag_lub_extrapolation, 
          RED_NO_PRINT 
        );
        break; 
      default: 
        fprintf(RED_OUT, "\nIllegal analysis direction.\n"); 
        exit(0); 
      }
    }
    red_print_reachable_return(rr); 
    return (rr->iteration_count); 
    break; 
  case ZENO_CHECK: 
    switch (red_query_system_type()) { 
    case RED_SYSTEM_HYBRID: 
      switch (flag_analysis_direction) { 
      case FLAG_ANALYSIS_FORWARD: 
        rr = red_reach_fwd( 
          red_query_diagram_initial(), 
          red_query_diagram_enhanced_global_invariance(), 
          red_query_diagram_zeno(), // This is to be destroyed. 
          RED_TASK_ZENO, 
          RED_PARAMETRIC_ANALYSIS, 
          RED_ALL_ROLES, 
          RED_FULL_REACHABILITY, 
          RED_NO_REACHABILITY_DEPTH_BOUND, 
          flag_counter_example, 
          RED_TIME_PROGRESS, 
          RED_NORM_HYBRID_2REDUNDANCY_ELIMINATION_DOWNWARD, 
          flag_action_approx, 
          flag_reduction, 
          RED_NOAPPROX, 
          RED_NO_SYMMETRY, 
            flag_tconvexity_shared_partitions 
          | flag_time_progress_options
          | flag_lub_extrapolation, 
          RED_NO_PRINT 
        );
        break; 
      case FLAG_ANALYSIS_BACKWARD: 
        rr = red_reach_bck( 
          red_query_diagram_initial(), 
          red_query_diagram_enhanced_global_invariance(), 
          red_query_diagram_zeno(), // This is to be destroyed. 
          RED_TASK_ZENO, 
          RED_PARAMETRIC_ANALYSIS, 
          RED_ALL_ROLES, 
          RED_FULL_REACHABILITY, 
          RED_NO_REACHABILITY_DEPTH_BOUND, 
          flag_counter_example, 
          RED_TIME_PROGRESS, 
          RED_NORM_HYBRID_2REDUNDANCY_ELIMINATION_DOWNWARD, 
          flag_action_approx, 
          flag_reduction, 
          RED_NOAPPROX, 
          RED_NO_SYMMETRY, 
            flag_tconvexity_shared_partitions 
          | flag_time_progress_options
          | flag_lub_extrapolation, 
          RED_NO_PRINT 
        );
        break; 
      default: 
        fprintf(RED_OUT, "\nIllegal analysis direction.\n"); 
        exit(0); 
      }
      break; 
    default: 
      switch (flag_analysis_direction) { 
      case FLAG_ANALYSIS_FORWARD: 
        rr = red_reach_fwd( 
          red_query_diagram_initial(), 
          red_query_diagram_enhanced_global_invariance(), 
          red_query_diagram_zeno(), // This is to be destroyed. 
          RED_TASK_ZENO, 
          RED_NO_PARAMETRIC_ANALYSIS, 
          RED_ALL_ROLES, 
          flag_full_reachability, // RED_NO_FULL_REACHABILITY, 
          RED_NO_REACHABILITY_DEPTH_BOUND, 
          flag_counter_example, 
          RED_TIME_PROGRESS, 
          RED_NORM_ZONE_MAGNITUDE_REDUCED, 
          flag_action_approx, 
          flag_reduction, 
          flag_approx, // RED_NOAPPROX, 
          RED_NO_SYMMETRY, 
            flag_tconvexity_shared_partitions 
          | flag_time_progress_options
          | flag_lub_extrapolation, 
          RED_NO_PRINT 
        );
        break; 
      case FLAG_ANALYSIS_BACKWARD: 
        rr = red_reach_bck( 
          red_query_diagram_initial(), 
          red_query_diagram_enhanced_global_invariance(), 
          red_query_diagram_zeno(), // This is to be destroyed. 
          RED_TASK_ZENO, 
          RED_NO_PARAMETRIC_ANALYSIS, 
          RED_ALL_ROLES, 
          flag_full_reachability, // RED_NO_FULL_REACHABILITY, 
          RED_NO_REACHABILITY_DEPTH_BOUND, 
          flag_counter_example, 
          RED_TIME_PROGRESS, 
          RED_NORM_ZONE_MAGNITUDE_REDUCED, 
          flag_action_approx, 
          flag_reduction, 
          flag_approx, // RED_NOAPPROX, 
          RED_NO_SYMMETRY, 
            flag_tconvexity_shared_partitions 
          | flag_time_progress_options
          | flag_lub_extrapolation, 
          RED_NO_PRINT 
        );
        break; 
      default: 
        fprintf(RED_OUT, "\nIllegal analysis direction.\n"); 
        exit(0); 
      }
      break; 
    } 
    red_print_reachable_return(rr); 
    return (rr->iteration_count); 
    break; 
  case SAFETY_CHECK: 
    ds = red_diagram(s); 
    ds = red_not(ds); 
    switch (red_query_system_type()) { 
    case RED_SYSTEM_HYBRID: 
      switch (flag_analysis_direction) { 
      case FLAG_ANALYSIS_FORWARD: 
        rr = red_reach_fwd( 
          red_query_diagram_initial(), 
          red_query_diagram_enhanced_global_invariance(), 
          ds, // This is to be destroyed. 
          RED_TASK_SAFETY, 
          RED_PARAMETRIC_ANALYSIS, 

          RED_ALL_ROLES, 
          RED_FULL_REACHABILITY, 
          RED_NO_REACHABILITY_DEPTH_BOUND, 
          flag_counter_example, 
          RED_TIME_PROGRESS, 

          RED_NORM_HYBRID_2REDUNDANCY_ELIMINATION_DOWNWARD, 
          flag_action_approx, 
          flag_reduction, 
          RED_NOAPPROX, 
          RED_NO_SYMMETRY, 

            flag_tconvexity_shared_partitions 
          | flag_time_progress_options
          | flag_lub_extrapolation, 
          RED_NO_PRINT 
        );
        break; 
      case FLAG_ANALYSIS_BACKWARD: 
        rr = red_reach_bck( 
          red_query_diagram_initial(), 
          red_query_diagram_enhanced_global_invariance(), 
          ds, // This is to be destroyed. 
          RED_TASK_SAFETY, 
          RED_PARAMETRIC_ANALYSIS, 

          RED_ALL_ROLES, 
          RED_FULL_REACHABILITY, 
          RED_NO_REACHABILITY_DEPTH_BOUND, 
          flag_counter_example, 
          RED_TIME_PROGRESS, 

          RED_NORM_HYBRID_2REDUNDANCY_ELIMINATION_DOWNWARD, 
          flag_action_approx, 
          flag_reduction, 
          RED_NOAPPROX, 
          RED_NO_SYMMETRY, 

            flag_tconvexity_shared_partitions 
          | flag_time_progress_options
          | flag_lub_extrapolation, 
          RED_NO_PRINT 
        );
        break; 
      default: 
        fprintf(RED_OUT, "\nIllegal analysis direction.\n"); 
        exit(0); 
      }
      break; 
    default: 
      switch (flag_analysis_direction) { 
      case FLAG_ANALYSIS_FORWARD: 
        rr = red_reach_fwd( 
          red_query_diagram_initial(), 
          red_query_diagram_enhanced_global_invariance(), 
          ds, // This is to be destroyed. 
          RED_TASK_SAFETY, 
          RED_NO_PARAMETRIC_ANALYSIS, 
          RED_ALL_ROLES, 
          flag_full_reachability, // RED_NO_FULL_REACHABILITY, 
          RED_NO_REACHABILITY_DEPTH_BOUND, 
          flag_counter_example, 
          RED_TIME_PROGRESS, 
          RED_NORM_ZONE_MAGNITUDE_REDUCED, 
          flag_action_approx, 
          flag_reduction, 
          flag_approx, // RED_NOAPPROX, 
          RED_NO_SYMMETRY, 
            flag_tconvexity_shared_partitions 
          | flag_time_progress_options
          | flag_lub_extrapolation, 
          RED_NO_PRINT 
        );
        break; 
      case FLAG_ANALYSIS_BACKWARD: 
        rr = red_reach_bck( 
          red_query_diagram_initial(), 
          red_query_diagram_enhanced_global_invariance(), 
          ds, // This is to be destroyed. 
          RED_TASK_SAFETY, 
          RED_NO_PARAMETRIC_ANALYSIS, 
          RED_ALL_ROLES, 
          flag_full_reachability, // RED_NO_FULL_REACHABILITY, 
          RED_NO_REACHABILITY_DEPTH_BOUND, 
          flag_counter_example, 
          RED_TIME_PROGRESS, 
          RED_NORM_ZONE_MAGNITUDE_REDUCED, 
          flag_action_approx, 
          flag_reduction, 
          flag_approx, // RED_NOAPPROX, 
          RED_NO_SYMMETRY, 
            flag_tconvexity_shared_partitions 
          | flag_time_progress_options
          | flag_lub_extrapolation, 
          RED_NO_PRINT 
        );
        break; 
      default: 
        fprintf(RED_OUT, "\nIllegal analysis direction.\n"); 
        exit(0); 
      }
    } 
    red_print_reachable_return(rr); 
    return (rr->iteration_count); 
    break;
  case RISK_CHECK:
    switch (red_query_system_type()) { 
    case RED_SYSTEM_HYBRID: 
      switch (flag_analysis_direction) { 
      case FLAG_ANALYSIS_FORWARD: 
        rr = red_reach_fwd( 
          red_query_diagram_initial(), 
          red_query_diagram_enhanced_global_invariance(), 
          red_diagram(s), // This is to be destroyed. 
          RED_TASK_RISK, 
          RED_PARAMETRIC_ANALYSIS, 
          RED_ALL_ROLES, 
          RED_FULL_REACHABILITY, 
          RED_NO_REACHABILITY_DEPTH_BOUND, 
          flag_counter_example, 
          RED_TIME_PROGRESS, 
          RED_NORM_HYBRID_2REDUNDANCY_ELIMINATION_DOWNWARD, 
          flag_action_approx, 
          flag_reduction, 
          RED_NOAPPROX, 
          RED_NO_SYMMETRY, 
            flag_tconvexity_shared_partitions 
          | flag_time_progress_options
          | flag_lub_extrapolation, 
          RED_NO_PRINT 
        );
        break; 
      case FLAG_ANALYSIS_BACKWARD: 
        rr = red_reach_bck( 
          red_query_diagram_initial(), 
          red_query_diagram_enhanced_global_invariance(), 
          red_diagram(s), // This is to be destroyed. 
          RED_TASK_RISK, 
          RED_PARAMETRIC_ANALYSIS, 
          RED_ALL_ROLES, 
          RED_FULL_REACHABILITY, 
          RED_NO_REACHABILITY_DEPTH_BOUND, 
          flag_counter_example, 
          RED_TIME_PROGRESS, 
          RED_NORM_HYBRID_2REDUNDANCY_ELIMINATION_DOWNWARD, 
          flag_action_approx, 
          flag_reduction, 
          RED_NOAPPROX, 
          RED_NO_SYMMETRY, 
            flag_tconvexity_shared_partitions 
          | flag_time_progress_options
          | flag_lub_extrapolation, 
          RED_NO_PRINT 
        );
        break; 
      default: 
        fprintf(RED_OUT, "\nIllegal analysis direction.\n"); 
        exit(0); 
      }
      break; 
    default: 
      switch (flag_analysis_direction) { 
      case FLAG_ANALYSIS_FORWARD: 
        rr = red_reach_fwd( 
          red_query_diagram_initial(), 
          red_query_diagram_enhanced_global_invariance(), 
          red_diagram(s), // This is to be destroyed. 
          RED_TASK_RISK, 
          RED_NO_PARAMETRIC_ANALYSIS, 

          RED_ALL_ROLES, 
          flag_full_reachability, // RED_NO_FULL_REACHABILITY, 
          RED_NO_REACHABILITY_DEPTH_BOUND, 
          flag_counter_example, 
          RED_TIME_PROGRESS, 

          RED_NORM_ZONE_MAGNITUDE_REDUCED, 
          flag_action_approx, 
          flag_reduction, 
          flag_approx, // RED_NOAPPROX, 
          RED_NO_SYMMETRY, 

            flag_tconvexity_shared_partitions 
          | flag_time_progress_options
          | flag_lub_extrapolation, 
          RED_NO_PRINT 
        );
        break; 
      case FLAG_ANALYSIS_BACKWARD: 
        rr = red_reach_bck( 
          red_query_diagram_initial(), 
          red_query_diagram_enhanced_global_invariance(), 
          red_diagram(s), // This is to be destroyed. 
          RED_TASK_RISK, 
          RED_NO_PARAMETRIC_ANALYSIS, 

          RED_ALL_ROLES, 
          flag_full_reachability, // RED_NO_FULL_REACHABILITY, 
          RED_NO_REACHABILITY_DEPTH_BOUND, 
          flag_counter_example, 
          RED_TIME_PROGRESS, 

          RED_NORM_ZONE_MAGNITUDE_REDUCED, 
          flag_action_approx, 
          flag_reduction, 
          flag_approx, // RED_NOAPPROX, 
          RED_NO_SYMMETRY, 

            flag_tconvexity_shared_partitions 
          | flag_time_progress_options
          | flag_lub_extrapolation, 
          RED_NO_PRINT 
        );
        break; 
      default: 
        fprintf(RED_OUT, "\nIllegal analysis direction.\n"); 
        exit(0); 
      }
    } 
    red_print_reachable_return(rr); 
    return (rr->iteration_count); 
    break;
  case GOAL_CHECK:
    switch (red_query_system_type()) { 
    case RED_SYSTEM_HYBRID: 
      switch (flag_analysis_direction) { 
      case FLAG_ANALYSIS_FORWARD: 
        rr = red_reach_fwd( 
          red_query_diagram_initial(), 
          red_query_diagram_enhanced_global_invariance(), 
          red_diagram(s), // This is to be destroyed. 
          RED_TASK_GOAL, 
          RED_PARAMETRIC_ANALYSIS, 
          RED_ALL_ROLES, 
          RED_FULL_REACHABILITY, 
          RED_NO_REACHABILITY_DEPTH_BOUND, 
          flag_counter_example, 
          RED_TIME_PROGRESS, 
          RED_NORM_HYBRID_2REDUNDANCY_ELIMINATION_DOWNWARD, 
          flag_action_approx, 
          flag_reduction, 
          RED_NOAPPROX, 
          RED_NO_SYMMETRY, 
            flag_tconvexity_shared_partitions 
          | flag_time_progress_options
          | flag_lub_extrapolation, 
          RED_NO_PRINT 
        );
        break; 
      case FLAG_ANALYSIS_BACKWARD: 
        rr = red_reach_bck( 
          red_query_diagram_initial(), 
          red_query_diagram_enhanced_global_invariance(), 
          red_diagram(s), // This is to be destroyed. 
          RED_TASK_GOAL, 
          RED_PARAMETRIC_ANALYSIS, 
          RED_ALL_ROLES, 
          RED_FULL_REACHABILITY, 
          RED_NO_REACHABILITY_DEPTH_BOUND, 
          flag_counter_example, 
          RED_TIME_PROGRESS, 
          RED_NORM_HYBRID_2REDUNDANCY_ELIMINATION_DOWNWARD, 
          flag_action_approx, 
          flag_reduction, 
          RED_NOAPPROX, 
          RED_NO_SYMMETRY, 
            flag_tconvexity_shared_partitions 
          | flag_time_progress_options
          | flag_lub_extrapolation, 
          RED_NO_PRINT 
        );
        break; 
      default: 
        fprintf(RED_OUT, "\nIllegal analysis direction.\n"); 
        exit(0); 
      }
      break; 
    default: 
      switch (flag_analysis_direction) { 
      case FLAG_ANALYSIS_FORWARD: 
        rr = red_reach_fwd( 
          red_query_diagram_initial(), 
          red_query_diagram_enhanced_global_invariance(), 
          red_diagram(s), // This is to be destroyed. 
          RED_TASK_GOAL, 
          RED_NO_PARAMETRIC_ANALYSIS, 
          RED_ALL_ROLES, 
          flag_full_reachability, // RED_NO_FULL_REACHABILITY, 
          RED_NO_REACHABILITY_DEPTH_BOUND, 
          flag_counter_example, 
          RED_TIME_PROGRESS, 
          RED_NORM_ZONE_MAGNITUDE_REDUCED, 
          flag_action_approx, 
          flag_reduction, 
          flag_approx, // RED_NOAPPROX, 
          RED_NO_SYMMETRY, 
            flag_tconvexity_shared_partitions 
          | flag_time_progress_options
          | flag_lub_extrapolation, 
          RED_NO_PRINT 
        );
        break; 
      case FLAG_ANALYSIS_BACKWARD: 
        rr = red_reach_bck( 
          red_query_diagram_initial(), 
          red_query_diagram_enhanced_global_invariance(), 
          red_diagram(s), // This is to be destroyed. 
          RED_TASK_GOAL, 
          RED_NO_PARAMETRIC_ANALYSIS, 
          RED_ALL_ROLES, 
          flag_full_reachability, // RED_NO_FULL_REACHABILITY, 
          RED_NO_REACHABILITY_DEPTH_BOUND, 
          flag_counter_example, 
          RED_TIME_PROGRESS, 
          RED_NORM_ZONE_MAGNITUDE_REDUCED, 
          flag_action_approx, 
          flag_reduction, 
          flag_approx, // RED_NOAPPROX, 
          RED_NO_SYMMETRY, 
            flag_tconvexity_shared_partitions 
          | flag_time_progress_options
          | flag_lub_extrapolation, 
          RED_NO_PRINT 
        );
        break; 
      default: 
        fprintf(RED_OUT, "\nIllegal analysis direction.\n"); 
        exit(0); 
      }
    } 
    red_print_reachable_return(rr); 
    return (rr->iteration_count); 
    break;

  case TCTL_CHECK:
    mr = red_model_check(
      red_query_diagram_initial(), 
      red_query_diagram_enhanced_global_invariance(), 
      flag_normality, 
      flag_action_approx, 
      flag_reduction, 
      RED_NOAPPROX, 
      flag_zeno, 
        flag_time_progress_analysis
      | flag_tconvexity_shared_partitions 
      | flag_time_progress_options  
      | flag_gfp_on_the_fly 
      | flag_fairness_assumptions_eval 
      | flag_gfp_path, 
      RED_NO_PRINT,  
      s 
    );
    
    fprintf(RED_OUT, "\nRED>> Checked TCTL formula:\n%s\n", s); 
    if (mr->status & FLAG_MODEL_CHECK_SATISFIED) { 
      fprintf(RED_OUT, 
        "\nRED>> The above specification in TCTL is satisfied.\n" 
      ); 
    } 
    else { 
      fprintf(RED_OUT, 
        "\nRED>> The above specification in TCTL is violated.\n" 
      ); 	
    }
/*
    fprintf(RED_OUT, "\nTrying an abstraction after model-checking:\n"); 
    fprintf(RED_OUT, "Original:\n"); 
    red_print_graph(mr->failed_state_diagram);  
    fprintf(RED_OUT, "Abstract:\n"); 
    red_print_graph(red_abstract(mr->failed_state_diagram, 
        RED_OAPPROX
      | RED_OAPPROX_MODL_GAME_MODE_ONLY
      | RED_OAPPROX_SPEC_GAME_UNTIMED
      | RED_NOAPPROX_ENVR_GAME
      | RED_NOAPPROX_GLOBAL_GAME, 
      "2;3;"
    ) ); 

    result = red_and(mr->failed_state_diagram, red_query_diagram_initial()); 
    result = red_norm(result, RED_NORM_ZONE_CLOSURE); 
    if (result == red_false()) { 
      fprintf(RED_OUT, 
        "\nThe following specification in TCTL is satisfied:\n%s\n", 
        s
      ); 
    } 
    else { 
      fprintf(RED_OUT, 
        "\nThe following specification in TCTL is violated:\n%s\n", 
        s
      ); 	
    }
*/
  }
}
/* my_verifier() */




main(argc, argv) 
  int	argc; 
  char	**argv; 
{ 
  redgram 	sub, conj; 
  char		*spec; 
  int		i; 
 
/*
  for (i = 1; i < argc; i++)
    printf("\nReceiving command-line argument %1d:%s.\n", 
      i, argv[i]
    ); 
*/    
  if (!my_process_command_line(argc, argv) /* the number of files */) 
    fprintf(RED_OUT, 
      "Use a line beginning with \"%%end\" to end the formula input.\n\n"
    );

//  printf("\nAfter processing command line\n"); 
  
  red_begin_session(flag_my_system_type, model_fname, my_proc_count);
/* 
  fprintf(RED_OUT, "after process command line, PC=%1d\n", my_proc_count); 
*/
  switch (task_type) { 
  case SIM_CHECK: 
  case BISIM_CHECK: 
    red_input_model(model_fname, RED_NO_REFINED_GLOBAL_INVARIANCE); 
    break; 
  default: 
    if (flag_analysis_direction == FLAG_ANALYSIS_FORWARD) 
      red_input_model(model_fname, RED_NO_REFINED_GLOBAL_INVARIANCE); 
    else 
      red_input_model(model_fname, RED_REFINE_GLOBAL_INVARIANCE); 
  }

/*
  sub = red_sync_xtion_event_string_bck(
    "u@(1)", 
    red_diagram("client_end@(2)"), 
    red_true(), 
    RED_USE_DECLARED_SYNC_XTION,  
    RED_GAME_MODL | RED_GAME_SPEC | RED_GAME_ENVR,  
    RED_TIME_PROGRESS, 
    RED_NORM_ZONE_CLOSURE, 
    RED_NO_ACTION_APPROX, 
    RED_REDUCTION_INACTIVE, 
    RED_NOAPPROX_MODL_GAME | RED_NOAPPROX_SPEC_GAME 
    | RED_NOAPPROX_ENVR_GAME | RED_NOAPPROX_GLOBAL_GAME, 
    RED_NO_SYMMETRY, 
    0 
  ); 
  fprintf(RED_OUT, "\nevents u@(1):\n"); 
  red_print_graph(sub); 
  sub = red_sync_xtion_event_string_bck(
    "c@(1)", 
    red_diagram("client_end@(2)"), 
    red_true(), 
    RED_USE_DECLARED_SYNC_XTION,  
    RED_GAME_MODL | RED_GAME_SPEC | RED_GAME_ENVR,  
    RED_TIME_PROGRESS, 
    RED_NORM_ZONE_CLOSURE, 
    RED_NO_ACTION_APPROX, 
    RED_REDUCTION_INACTIVE, 
    RED_NOAPPROX_MODL_GAME | RED_NOAPPROX_SPEC_GAME 
    | RED_NOAPPROX_ENVR_GAME | RED_NOAPPROX_GLOBAL_GAME, 
    RED_NO_SYMMETRY, 
    0 
  ); 
  fprintf(RED_OUT, "\nevents c@(1):\n"); 
  red_print_graph(sub); 

  sub = red_sync_xtion_event_string_bck(
    "f@(1)", 
    red_diagram("client_end@(2)"), 
    red_true(), 
    RED_USE_DECLARED_SYNC_XTION,  
    RED_GAME_MODL | RED_GAME_SPEC | RED_GAME_ENVR,  
    RED_TIME_PROGRESS, 
    RED_NORM_ZONE_CLOSURE, 
    RED_NO_ACTION_APPROX, 
    RED_REDUCTION_INACTIVE, 
    RED_NOAPPROX_MODL_GAME | RED_NOAPPROX_SPEC_GAME 
    | RED_NOAPPROX_ENVR_GAME | RED_NOAPPROX_GLOBAL_GAME, 
    RED_NO_SYMMETRY, 
    0 
  ); 
  fprintf(RED_OUT, "\nevents f@(1):\n"); 
  red_print_graph(sub); 
*/
  printf("Specification is in file %s\n", spec_fname); 
  spec = red_file_to_string(spec_fname); 
/*
  report_red_management(); 
  fprintf(RED_OUT, "RT[ABSTRACT_IMAGE=%1d]:\n", ABSTRACT_IMAGE); 
  red_print_graph(RT[ABSTRACT_IMAGE]); 
  fflush(out); 
  bk("End"); 
  fprintf(RED_OUT, "\ntest red sync xtion string(sxi=%1d):\n%s\n", 
    1, red_sync_xtion_string(1)
  ); 
*/
  my_verifier(task_type, spec); 
/*  
  fprintf(RED_OUT, "RED>> Total system time: %fsec., user time: %fsec.\n", 
    red_query_system_time(), 
    red_query_user_time() 
  ); 
  fprintf(RED_OUT, "RED>> Total memory (HRD+CRD+MDD): %1dbytes.\n", 
    red_query_memory()
  ); 
*/
  red_end_session(model_fname); 
}
  /* main() */ 
