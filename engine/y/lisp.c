void
init_list (void)
{
  /* Load Burro's Guile procedures into the burro environment. */
  guile_c_resolve_module_safe ("burro");
  init_guile_guile_procedures ();
  init_guile_obj_procedures ();
}
