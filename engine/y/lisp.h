/*-----------------------------------------------------------------------------

	lisp.h -- set up scheme interpreter
	Copyright (C) 2014, 2015
		Michael L. Gran (spk121)

	GPL3+
-----------------------------------------------------------------------------*/
/** @file lisp.h
 *  @brief set up custom scheme interpreter
 *
 *  The custom scheme interpreter is all callback based because this is a
 *  Gtk event loop program
 *
 *  The main module needs to to a few things.
 *  - it needs to provide on-idle and after-draw callback functions.
 *    Each procedure receives one argument, the current loop time.
 *  - it needs to register these callbacks with the engine using
 *    'loop-set-idle-callback' and 'loop-set-after-draw-callback'
*/

#ifndef BURRO_LISP_H
#define BURRO_LISP_H

/** Initialize the scheme interpreter.  Also, choose a module to be
 *  the main module.  It is expected that the main module will provide
 *  the on-idle and after-draw callback.
 *
 * @param main_module - a filename to a Guile module to be loaded.
 */
void init_lisp (const char *main_script);


void lisp_init_guile_procedures (void);


#endif
