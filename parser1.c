/*--------------------------------------------------------------------------*/
/*                                                                          */
/*       parser1                                                            */
/*                                                                          */
/*                                                                          */
/*       Group Members:          ID numbers                                 */
/*                                                                          */
/*           Conor Egan          13138782                                   */
/*           Niall Phillips      13153382                                   */
/*--------------------------------------------------------------------------*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "global.h"
#include "scanner.h"
#include "line.h"
#include "strtab.h"
#include "code.h"
#include "symbol.h"


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
PRIVATE int ErrorFlag;             /*  Set if Syntax errors detected*/

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
PRIVATE void ReadToEndOfFile( void );
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

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Main: Parser entry point.  Sets up parser globals (opens input and */
/*        output files, initialises current lookahead), then calls          */
/*        "ParseProgram" to start the parse.                                */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PUBLIC int main ( int argc, char *argv[] )
{
    ErrorFlag = 0;
    if ( OpenFiles( argc, argv ) )  {
        InitCharProcessor( InputFile, ListFile );
        CurrentToken = GetToken();
        ParseProgram();
        fclose( InputFile );
        fclose( ListFile );
        if(ErrorFlag == 1) {
        printf("Syntax Error Detected\n");
        return EXIT_FAILURE;
    }
    printf("Valid Syntax\n");
    return  EXIT_SUCCESS;
        return  EXIT_SUCCESS;
        printf("Valid Syntax\n");
    }
    else 
        return EXIT_FAILURE;
        printf("Syntax Error Detected\n");
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
    if ( CurrentToken.code == VAR )  ParseDeclarations();
    while ( CurrentToken.code == PROCEDURE ){
      ParseProcDeclaration();
    }
    ParseBlock();
    Accept( ENDOFPROGRAM );
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

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseProcDeclarations implements:                                       */
/*                                                                          */
/*    <ProcDeclarations> ::== "PROCEDURE" <Identifier> [<ParameterList>]    */
/*	                      ";" [<Declarations>]{<ProcDeclaration>}       */
/*                            <Block> ";"                                   */
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

PRIVATE void ParseProcDeclaration( void )
{
  Accept( PROCEDURE );
  Accept( IDENTIFIER );  
  if ( CurrentToken.code == LEFTPARENTHESIS ) ParameterList();
  Accept(SEMICOLON );
  if ( CurrentToken.code == VAR ) ParseDeclarations();
  while ( CurrentToken.code == PROCEDURE ){
    ParseProcDeclaration();
  }
  ParseBlock();
  Accept( SEMICOLON );
  
}
/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseBlock implements:                                                  */
/*                                                                          */
/*    <Block> ::== "BEGIN" {<Statement> ";" } "END"                         */
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
PRIVATE void ParseBlock( void ) 
{
  Accept( BEGIN );
  while (CurrentToken.code == IDENTIFIER || CurrentToken.code == WHILE || CurrentToken.code == IF || 
	 CurrentToken.code == READ || CurrentToken.code == WRITE)
        {
    ParseStatement();
    Accept( SEMICOLON );
	}
  Accept( END);

}



/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseStatement implements:                                              */
/*                                                                          */
/*       <Statement>   :== <SimpleStatement>|<WhileStatement>|<IfStatement> */
/*                         |<ReadStatement>|<WriteStatement>                */
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
  else SyntaxError( IDENTIFIER, CurrentToken );
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  SimpleStatement implements:                                             */
/*                                                                          */
/*       <SimpleStatement>   :== <VarOrProcName><RestOfStatement>           */
/*                                                                          */
/*    Note: Function VarOrProcName() has identical functionality to         */
/*    function Variable().                                                  */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*--------------------------------------------------------------------------*/
PRIVATE void SimpleStatement( void )
{
  VarOrProcName();
  RestOfStatement();
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  WhileStatement implements:                                              */
/*                                                                          */
/*       <WhileStatement>   :== "WHILE" <BooleanExpression> "DO" <Block>    */
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

PRIVATE void WhileStatement( void )
{
  Accept( WHILE );
  BooleanExpression();
  Accept( DO );
  ParseBlock();
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  IfStatement implements:                                                 */
/*                                                                          */
/*       <IfStatement>   :== "IF" <BooleanExpression> "THEN" <Block>        */
/*                           ["ELSE"<Block>]                                */
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

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ReadStatement implements:                                               */
/*                                                                          */
/*       <ReadStatement>   :== "READ" "(" <Variable>{","<Variable>}")"      */
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

PRIVATE void ReadStatement( void )
{
  Accept( READ );
  ProcCallList();
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  RestOfStatement implements:                                             */
/*                                                                          */
/*       <RestOfStatement>   :== <ProcCallList>|<Assignment>|ε              */
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

PRIVATE void RestOfStatement( void )
{
  if ( CurrentToken.code == LEFTPARENTHESIS ) ProcCallList();
  else if ( CurrentToken.code == ASSIGNMENT ) Assignment();
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  WriteStatement implements:                                              */
/*                                                                          */
/*       <WriteStatement>   :==  "WRITE" "("<Variable>{","<Variable>} ")"   */
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

PRIVATE void WriteStatement( void )
{
  Accept( WRITE );
  ProcCallList();
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  VarOrProcName implements:                                               */
/*                                                                          */
/*       <VarOrProcName>   :==  <Identifier>                                */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*--------------------------------------------------------------------------*/
PRIVATE void VarOrProcName( void )
{
  Identifier();
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Identifier implements:                                                  */
/*                                                                          */
/*       <Identifier>   :==  <Alpha>{<AlphaNum>}                            */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void Identifier( void )
{
  Accept( IDENTIFIER );

}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Variable implements:                                                    */
/*                                                                          */
/*       <Variable>   :==  <Identifier>                                     */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void Variable( void )
{
  Identifier();
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  BooleanExpression implements:                                           */
/*                                                                          */
/*       <BooleanExpression>   :==  <Expression><RelOp><Expression>         */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void BooleanExpression( void )
{
  Expression();
  RelOp();
  Expression();
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  RelOp implements:                                                       */
/*                                                                          */
/*       <RelOp>   :== "="|"<="|">="|"<"|">"                                */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void RelOp( void )
{
  if ( CurrentToken.code == EQUALITY ) Accept( EQUALITY );
  else if ( CurrentToken.code == LESSEQUAL ) Accept( LESSEQUAL );
  else if ( CurrentToken.code == GREATEREQUAL ) Accept( GREATEREQUAL );
  else if ( CurrentToken.code == LESS ) Accept( LESS );
  else if ( CurrentToken.code == GREATER ) Accept( GREATER );
  else SyntaxError( IDENTIFIER, CurrentToken );
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  AddOp implements:                                                       */
/*                                                                          */
/*       <AddOp>   :==  "+"|"-"                                             */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void AddOp( void )
{
  if ( CurrentToken.code == ADD ) Accept( ADD );
  else if ( CurrentToken.code == SUBTRACT ) Accept( SUBTRACT );
  else SyntaxError( IDENTIFIER, CurrentToken );
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  MultOp implements:                                                      */
/*                                                                          */
/*       <MultOp>   :==  "*"|"/"                                            */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void MultOp( void )
{
  if ( CurrentToken.code == MULTIPLY ) Accept( MULTIPLY );
  else if ( CurrentToken.code == DIVIDE ) Accept ( DIVIDE );
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Assignment implements:                                                  */
/*                                                                          */
/*       <Assignment>   :==  ":="<Expression>                               */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void Assignment( void )
{
  Accept( ASSIGNMENT );
  Expression();
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  CompoundTerm implements:                                                */
/*                                                                          */
/*       <CompoundTerm>   :==  <Term>{<MultOp><Term>}                       */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void CompoundTerm( void )
{
  Term();
  while ( CurrentToken.code == MULTIPLY || CurrentToken.code == DIVIDE ){
    MultOp();
    Term();
  }
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Term implements:                                                        */
/*                                                                          */
/*       <Term>   :==  ["-"]<SubTerm>                                       */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void Term( void )
{
  if ( CurrentToken.code == SUBTRACT ) Accept( SUBTRACT );
  SubTerm();
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  SubTerm implements:                                                     */
/*                                                                          */
/*       <SubTerm>   :==  <Variable>|<IntConst>| "(" <Expression> ")"       */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void SubTerm( void )
{
  if ( CurrentToken.code == IDENTIFIER) Variable();
  else if ( CurrentToken.code == INTCONST ) IntConst();
  else if ( CurrentToken.code == LEFTPARENTHESIS ){
    Accept( LEFTPARENTHESIS );
    Expression();
    Accept( RIGHTPARENTHESIS );
  }
  else SyntaxError(IDENTIFIER,CurrentToken);
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  IntConst implements:                                                    */
/*                                                                          */
/*       <IntConst>   :==  <Digit>{<Digit>}                                 */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*--------------------------------------------------------------------------*/

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
/*  ParameterList implements:                                               */
/*                                                                          */
/*      <ParameterList>   :== "("<FormalParameter>{","<FormalParameter>}")" */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ParameterList( void )
{
  Accept( LEFTPARENTHESIS );
  FormalParameter();
  while( CurrentToken.code == COMMA ) {
    Accept( COMMA );
    FormalParameter();
  }
  Accept( RIGHTPARENTHESIS );

}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  FormalParameter implements:                                             */
/*                                                                          */
/*       <FormalParameter>   :== ["REF"]<Variable>                          */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void FormalParameter( void )
{
  if( CurrentToken.code == REF ) Accept ( REF );
  Variable();
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ActualParameter implements:                                             */
/*                                                                          */
/*       <ActualParameter>   :== <Variable>|<Expression>                    */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ActualParameter( void )
{
  Expression();
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ProcCallList implements:                                                */
/*                                                                          */
/*     <ProcCallList>   :== "(" <ActualParameter>{","<ActualParameter>}")"  */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE void ProcCallList( void )
{
  Accept( LEFTPARENTHESIS );
  ActualParameter();
  while ( CurrentToken.code == COMMA ){
    Accept( COMMA );
    ActualParameter();
   }  
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
    if ( CurrentToken.code != ExpectedToken )  {
        SyntaxError( ExpectedToken, CurrentToken );
        ReadToEndOfFile();
        fclose( InputFile );
        fclose( ListFile );
        ErrorFlag = 1;
        exit( EXIT_FAILURE );
    }
    else  CurrentToken = GetToken();
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

PRIVATE void ReadToEndOfFile( void )
{
    if ( CurrentToken.code != ENDOFINPUT )  {
        Error( "Parsing ends here in this program\n", CurrentToken.pos );
        while ( CurrentToken.code != ENDOFINPUT )  CurrentToken = GetToken();
    }
}


