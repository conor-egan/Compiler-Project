/*--------------------------------------------------------------------------*/
/*                                                                          */
/*       parser1                                                            */
/*                                                                          */
/*                                                                          */
/*       Group Members:          ID numbers                                 */
/*                                                                          */
/*           Conor Egan          13138782                                   */
/*                                                                          */
/*                                                                          */
/*       Currently just a copy of "smallparser.c".  To create "parser1.c",  */
/*       modify this source to reflect the CPL grammar.                     */
/*                                                                          */
/*--------------------------------------------------------------------------*/
/*                                                                          */
/*       smallparser                                                        */
/*                                                                          */
/*       An illustration of the use of the character handler and scanner    */
/*       in a parser for the language                                       */
/*                                                                          */
/*       <Program>     :== "BEGIN" { <Statement> ";" } "END" "."            */
/*       <Statement>   :== <Identifier> ":=" <Expression>                   */
/*       <Expression>  :== <Identifier> | <IntConst>                        */
/*                                                                          */
/*                                                                          */
/*       Note - <Identifier> and <IntConst> are provided by the scanner     */
/*       as tokens IDENTIFIER and INTCONST respectively.                    */
/*                                                                          */
/*       Although the listing file generator has to be initialised in       */
/*       this program, full listing files cannot be generated in the        */
/*       presence of errors because of the "crash and burn" error-          */
/*       handling policy adopted. Only the first error is reported, the     */
/*       remainder of the input is simply copied to the output (using       */
/*       the routine "ReadToEndOfFile") without further comment.            */
/*                                                                          */
/*--------------------------------------------------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "global.h"
#include "scanner.h"
#include "line.h"


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Global variables used by this parser.                                   */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE FILE *InputFile;           /*  CPL source comes from here.          */
PRIVATE FILE *ListFile;            /*  For nicely-formatted syntax errors.  */

PRIVATE TOKEN  CurrentToken;       /*  Parser lookahead token.  Updated by  */
                                   /*  routine Accept (below).  Must be     */
                                   /*  initialised before parser starts.    */


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Function prototypes                                                     */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE int  OpenFiles( int argc, char *argv[] );
PRIVATE void ParseProgram( void );
PRIVATE void ParseStatement( void );
PRIVATE void Expression( void );
PRIVATE void Accept( int code );
/*ReadToEndOfFile unnecessary after error recovery functionality added*/
/*PRIVATE void ReadToEndOfFile( void );*/
PRIVATE void ParseDeclarations( void );
PRIVATE void ParseProcDeclaration( void );
PRIVATE void ParseBlock( void );
PRIVATE void ParameterList( void );
PRIVATE void FormalParameter( void );
PRIVATE void SimpleStatement( void );
PRIVATE void WhileStatement( void );
PRIVATE void IfStatement( void );
PRIVATE void ReadStatement( void );
PRIVATE void WriteStatement( void );
PRIVATE void VarOrProcName( void );
PRIVATE void RestOfStatement( void );
PRIVATE void Variable( void );
PRIVATE void Expression( void );
PRIVATE void BooleanExpression( void );
PRIVATE void Identifier( void );
PRIVATE void ActualParameter( void );
PRIVATE void ProcCallList( void );
PRIVATE void Assignment( void );
PRIVATE void CompoundTerm( void );
PRIVATE void AddOp( void );
PRIVATE void RelOp( void );
PRIVATE void MultOp( void );
PRIVATE void Term( void );
PRIVATE void SubTerm( void );
PRIVATE void IntConst( void );

/*S-Algol Recovery Functions*/
PRIVATE void Synchronise( SET *F, SET *FB );
PRIVATE void SetupSets( void );


SET ParseBlockFBS;
SET ParseBlockFS_aug;

SET ParseProgramFBS;
SET ParseProgramFS_aug1;
SET ParseProgramFS_aug2;

SET ParseProcDeclarationsFS_aug1;
SET ParseProcDeclarationsFS_aug2;
SET ParseProcDeclarationsFSB;
/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Main: Smallparser entry point.  Sets up parser globals (opens input and */
/*        output files, initialises current lookahead), then calls          */
/*        "ParseProgram" to start the parse.                                */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PUBLIC int main ( int argc, char *argv[] )
{
    if ( OpenFiles( argc, argv ) )  {
        InitCharProcessor( InputFile, ListFile );
        CurrentToken = GetToken();
	SetupSets();
        ParseProgram();
        fclose( InputFile );
        fclose( ListFile );
        return  EXIT_SUCCESS;
    }
    else 
        return EXIT_FAILURE;
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Parser routines: Recursive-descent implementaion of the grammar's       */
/*                   productions.                                           */
/*                                                                          */
/*                                                                          */
/*  ParseProgram implements:                                                */
/*                                                                          */
/*       <Program>     :== "BEGIN" { <Statement> ";" } "END" "."            */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/


PRIVATE void ParseProgram( void )
{
    Accept( PROGRAM );
    Accept( IDENTIFIER );
    Accept( SEMICOLON );
    Synchronise( &ParseProgramFS_aug1, &ParseProgramFBS );
    if ( CurrentToken.code == VAR )  ParseDeclarations();
    Synchronise( &ParseProgramFS_aug2, &ParseProgramFBS );
    while ( CurrentToken.code == PROCEDURE )  ParseProcDeclaration();
    Synchronise( &ParseProgramFS_aug2, &ParseProgramFBS );
    ParseBlock();
    Accept( ENDOFPROGRAM );
}



/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseStatement implements:                                              */
/*                                                                          */
/*       <Statement>   :== <Identifier> ":=" <Expression>                   */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseStatement( void )
{
  if ( CurrentToken.code == IDENTIFIER ) SimpleStatement();
  else if ( CurrentToken.code == WHILE ) WhileStatement();
  else if ( CurrentToken.code == IF ) IfStatement();
  else if ( CurrentToken.code == READ ) ReadStatement();
  else if ( CurrentToken.code == WRITE ) WriteStatement();
}

PRIVATE void SimpleStatement( void )
{
  VarOrProcName();
  RestOfStatement();
}

PRIVATE void WhileStatement( void )
{
  Accept( WHILE );
  BooleanExpression();
  Accept( DO );
  ParseBlock();
}



PRIVATE void IfStatement( void )
{
  Accept( IF );
  BooleanExpression();
  Accept( THEN );
  ParseBlock();
  while ( CurrentToken.code == ELSE ){
    Accept( ELSE);
    ParseBlock();
  }
}

PRIVATE void ReadStatement( void )
{
  Accept( READ );
  Accept( LEFTPARENTHESIS );
  Variable();
  while ( CurrentToken.code == COMMA ){
    Variable();
  }
  Accept( RIGHTPARENTHESIS );
}

PRIVATE void RestOfStatement( void )
{
  if ( CurrentToken.code == LEFTPARENTHESIS ) ProcCallList();
  else if ( CurrentToken.code == ASSIGNMENT ) Assignment();

}

PRIVATE void WriteStatement( void )
{
  Accept( WRITE );
  Accept( LEFTPARENTHESIS );
  Expression();
  while( CurrentToken.code == COMMA ){
    Expression();
  }
  Accept( RIGHTPARENTHESIS );
}

PRIVATE void VarOrProcName( void )
{
  Identifier();
}

PRIVATE void Identifier( void )
{
  Accept( IDENTIFIER );

}

PRIVATE void Variable( void )
{
  Accept( IDENTIFIER );
}

PRIVATE void BooleanExpression( void )
{
  Expression();
  RelOp();
  Expression();
}

PRIVATE void RelOp( void )
{
  if ( CurrentToken.code == EQUALITY ) Accept( EQUALITY );
  else if ( CurrentToken.code == LESSEQUAL ) Accept( LESSEQUAL );
  else if ( CurrentToken.code == GREATEREQUAL ) Accept( GREATEREQUAL );
  else if ( CurrentToken.code == LESS ) Accept( LESS );
  else if ( CurrentToken.code == GREATER ) Accept( GREATER );
}

PRIVATE void AddOp( void )
{
  if ( CurrentToken.code == ADD ) Accept( ADD );
  else if ( CurrentToken.code == SUBTRACT ) Accept( SUBTRACT );
}

PRIVATE void Assignment( void )
{
  Accept( ASSIGNMENT );
  Expression();
}

PRIVATE void CompoundTerm( void )
{
  Term();
  while ( CurrentToken.code == MULTIPLY || CurrentToken.code == DIVIDE ){
    MultOp();
    Term();
  }
}

PRIVATE void Term( void )
{
  if ( CurrentToken.code == SUBTRACT ) Accept( SUBTRACT );
  SubTerm();
}

PRIVATE void SubTerm( void )
{
  if ( CurrentToken.code == IDENTIFIER) Variable();
  else if ( CurrentToken.code == INTCONST ) IntConst();
  else if ( CurrentToken.code == LEFTPARENTHESIS ){
    Expression();
    Accept( RIGHTPARENTHESIS );
  }
}

PRIVATE void MultOp( void )
{
  if ( CurrentToken.code == MULTIPLY ) Accept( MULTIPLY );
  else if ( CurrentToken.code == DIVIDE ) Accept ( DIVIDE );
}

PRIVATE void IntConst( void )
{
  Accept( INTCONST );
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseExpression implements:                                             */
/*                                                                          */
/*       <Expression>  :== <Identifier> | <IntConst>                        */
/*                                                                          */
/*       Note that <Identifier> and <IntConst> are handled by the scanner   */
/*       and are returned as tokens IDENTIFER and INTCONST respectively.    */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void Expression( void )
{
  CompoundTerm();
  while ( CurrentToken.code == ADD || CurrentToken.code == SUBTRACT ){
    AddOp();
    CompoundTerm();
  }
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseDeclarations implements:                                           */
/*                                                                          */
/*        <Declarations> ::== "VAR" <Variable> { "," <Variable> } ";"       */
/*                                                                          */
/*       Note that <Variable> is handled by the scanner                     */
/*       and are returned as tokens IDENTIFER and INTCONST respectively.    */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*                                                                          */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParseDeclarations( void )
{
    Accept( VAR );
    Accept( IDENTIFIER );
    while ( CurrentToken.code == COMMA )  {
        Accept( COMMA );
        Accept( IDENTIFIER );
    }
    Accept( SEMICOLON );
}

PRIVATE void ParseProcDeclaration( void )
{
  Accept( PROCEDURE );
  Accept( IDENTIFIER );  
  if ( CurrentToken.code == LEFTPARENTHESIS ) ParameterList();
  Accept(SEMICOLON );
  Synchronise( &ParseProcDeclarationsFS_aug1, &ParseProcDeclarationsFBS );
  if ( CurrentToken.code == VAR ) ParseDeclarations();
  Synchronise( &ParseProcDeclarationsFS_aug2, &ParseProcDeclarationsFBS );
  while ( CurrentToken.code == PROCEDURE ) ParseProcDeclaration();
  ParseBlock();
  Accept( SEMICOLON );
  Synchronise( &ParseProcDeclarationsFS_aug2, &ParseProcDeclarationsFS_aug2 );
}

PRIVATE void ParseBlock( void ) 
{
  Accept( BEGIN );
  Synchronise( &ParseBlockFS_aug, &ParseBlockFBS );
  while (CurrentToken.code == IDENTIFIER || CurrentToken.code == WHILE || CurrentToken.code == IF || 
	 CurrentToken.code == READ || CurrentToken.code == WRITE)
        {
    ParseStatement();
    Accept( SEMICOLON );
    Synchronise( &ParseBlockFS_aug, &ParseBlockFBS );
  }
  Accept( END);

}

PRIVATE void ParameterList( void )
{
  Accept( LEFTPARENTHESIS );
  FormalParameter();
  while( CurrentToken.code == IDENTIFIER ) {
    Accept( COMMA );
    FormalParameter();
  }
  Accept( RIGHTPARENTHESIS );

}

PRIVATE void FormalParameter( void )
{
  Accept ( REF );
  Accept( IDENTIFIER );
}

PRIVATE void ActualParameter( void )
{
  Expression();
}

PRIVATE void ProcCallList( void )
{
  Accept( LEFTPARENTHESIS );
  ActualParameter();
  while ( CurrentToken.code == COMMA ) ActualParameter();
  Accept( RIGHTPARENTHESIS );
}





/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  End of parser.  Support routines follow.                                */
/*                                                                          */
/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Accept:  Takes an expected token name as argument, and if the current   */
/*           lookahead matches this, advances the lookahead and returns.    */
/*                                                                          */
/*           If the expected token fails to match the current lookahead,    */
/*           this routine reports a syntax error and exits ("crash & burn"  */
/*           parsing).  Note the use of routine "SyntaxError"               */
/*           (from "scanner.h") which puts the error message on the         */
/*           standard output and on the listing file, and the helper        */
/*           "ReadToEndOfFile" which just ensures that the listing file is  */
/*           completely generated.                                          */
/*                                                                          */
/*                                                                          */
/*    Inputs:       Integer code of expected token                          */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: If successful, advances the current lookahead token     */
/*                  "CurrentToken".                                         */
/*                                                                          */
/*--------------------------------------------------------------------------*/


 PRIVATE void Accept( int ExpectedToken )
{
  static int recovering = 0;
  if ( recovering ) {
    while ( CurrentToken.code != ExpectedToken && CurrentToken.code != ENDOFINPUT ) CurrentToken = GetToken();
    recovering = 0;
  }
  if ( CurrentToken.code != ExpectedToken ) {
      SyntaxError( ExpectedToken, CurrentToken );
      recovering = 1;
    }
  else CurrentToken = GetToken();
} 



/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  OpenFiles:  Reads strings from the command-line and opens the           */
/*              associated input and listing files.                         */
/*                                                                          */
/*    Note that this routine mmodifies the globals "InputFile" and          */
/*    "ListingFile".  It returns 1 ("true" in C-speak) if the input and     */
/*    listing files are successfully opened, 0 if not, allowing the caller  */
/*    to make a graceful exit if the opening process failed.                */
/*                                                                          */
/*                                                                          */
/*    Inputs:       1) Integer argument count (standard C "argc").          */
/*                  2) Array of pointers to C-strings containing arguments  */
/*                  (standard C "argv").                                    */
/*                                                                          */
/*    Outputs:      No direct outputs, but note side effects.               */
/*                                                                          */
/*    Returns:      Boolean success flag (i.e., an "int":  1 or 0)          */
/*                                                                          */
/*    Side Effects: If successful, modifies globals "InputFile" and         */
/*                  "ListingFile".                                          */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE int  OpenFiles( int argc, char *argv[] )
{

    if ( argc != 3 )  {
        fprintf( stderr, "%s <inputfile> <listfile>\n", argv[0] );
        return 0;
    }

    if ( NULL == ( InputFile = fopen( argv[1], "r" ) ) )  {
        fprintf( stderr, "cannot open \"%s\" for input\n", argv[1] );
        return 0;
    }

    if ( NULL == ( ListFile = fopen( argv[2], "w" ) ) )  {
        fprintf( stderr, "cannot open \"%s\" for output\n", argv[2] );
        fclose( InputFile );
        return 0;
    }

    return 1;
}


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ReadToEndOfFile:  Reads all remaining tokens from the input file.       */
/*              associated input and listing files.                         */
/*                                                                          */
/*    This is used to ensure that the listing file refects the entire       */
/*    input, even after a syntax error (because of crash & burn parsing,    */
/*    if a routine like this is not used, the listing file will not be      */
/*    complete.  Note that this routine also reports in the listing file    */
/*    exactly where the parsing stopped.  Note that this routine is         */
/*    superfluous in a parser that performs error-recovery.                 */
/*                                                                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Reads all remaining tokens from the input.  There won't */
/*                  be any more available input after this routine returns. */
/*                                                                          */
/*--------------------------------------------------------------------------*/
/*
PRIVATE void ReadToEndOfFile( void )
{
    if ( CurrentToken.code != ENDOFINPUT )  {
        Error( "Parsing ends here in this program\n", CurrentToken.pos );
        while ( CurrentToken.code != ENDOFINPUT )  CurrentToken = GetToken();
    }
}
*/
PRIVATE void Synchronise( SET *F, SET *FB )
{
  SET S;
  S = Union( 2, F, FB );
  if ( !InSet( F, CurrentToken.code ) ) {
    SyntaxError2( *F, CurrentToken );
    while ( !InSet( &S, CurrentToken.code ) )
      CurrentToken = GetToken();
  }
} 

PRIVATE void SetupSets( void )
{
  ClearSet( &ParseBlockFS_aug );
  AddElements( &ParseBlockFS_aug, 6, IDENTIFIER, WHILE, IF, READ, WRITE, END );
  ClearSet( &ParseBlockFBS );
  AddElements( &ParseBlockFBS, 4, ENDOFPROGRAM, SEMICOLON, ELSE, ENDOFINPUT );

  ClearSet( &ParseProgramFS_aug1 );
  AddElements( &ParseProgramFS_aug1, 3, VAR, PROCEDURE, BEGIN );

  ClearSet( &ParseProgramFS_aug2 );
  AddElements( &ParseProgramFS_aug2, 2, PROCEDURE, BEGIN );

  ClearSet( &ParseProcDeclarations_aug1 );
  AddElements( &ParseProcDeclarations_aug1, 3, VAR, PROCEDURE, BEGIN );

  ClearSet( &ParseProcDeclarations_aug2 );
  AddElements( &ParseProceDeclarations_aug2, 2, PROCEDURE, BEGIN );

  ClearSet( &ParseProcDeclarationsFBS );
  AddElements( &ParseProcDeclarationsFBS, 3, END,ENDOFPROGRAM,ENDOFINPUT );
 
}
