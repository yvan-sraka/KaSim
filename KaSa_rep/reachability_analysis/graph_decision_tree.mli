type 'a tree

val build_decision_tree_list:
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  Cckappa_sig.kappa_handler ->
  (Ckappa_sig.c_site_name * Ckappa_sig.c_agent_name *
   Ckappa_sig.c_site_name)
    list list ->
  Exception.method_handler * Build_graph.in_progress tree list

val print:
  Remanent_parameters_sig.parameters ->
  Exception.method_handler ->
  Build_graph.in_progress tree ->
  Exception.method_handler
