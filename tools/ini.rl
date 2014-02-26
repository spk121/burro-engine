#include <stdio.h>
#include <string.h>


%%{
  machine Ini;
  write data;

  action KEY
  {
    /* store key */
  }
  action VAL
  {
    /* store val */
  }
  action ASSIGN
  {
    /* store qval */
  }
  action SECTION
  {
    /* store section name */
  }

  whitespace = [\t ];
  
  line_ending = '\r'{0,1} '\n';

  empty_line = whitespace* line_ending;

  section_name = (print - [\[\];="])+ @SECTION;
  section_line = whitespace* '[' section_name ']' whitespace* line_ending;

  comment_line = whitespace* ';' print* line_ending;

  key = alpha (alnum | '_')* @KEY;
  unquoted_val = (graph - [=;"])+ @VAL;
  quoted_val = (print - '"')* @VAL;
  val = (unquoted_val | '"' quoted_val '"');
  assignment_line = whitespace* key whitespace* '=' whitespace* val line_ending @ASSIGN;

  ini = (empty_line | comment_line | section_line | assignment_line)*;


  main := |*
    ini;
  *|;
}%%

/////////////
// INIT
void init()
{
%% write init;
}

////////////
// EXEC
void exec()
{
%% write exec;
}
