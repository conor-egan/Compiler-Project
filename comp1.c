/*--------------------------------------------------------------------------*/
/*                                                                          */
/*       comp1                                                              */
/*                                                                          */
/*                                                                          */
/*       Group Members:          ID numbers                                 */
/*                                                                          */
/*           Conor Egan          13138782                                   */  
/*           Niall Phillips      13153382                                   */
/*                                                                          */
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
#include "debug.h"


/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Global variables used by this parser.                                   */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PRIVATE FILE *InputFile;           /*  CPL source comes from here.          */
PRIVATE FILE *ListFile;            /*  For nicely-formatted syntax errors.  */
PRIVATE FILE *CodeFile;				/* File to store generated code */
PRIVATE int ErrorFlag;             /*  Set if Syntax errors detected*/
PRIVATE TOKEN  CurrentToken;       /*  Parser lookahead token.  Updated by  */
                                   /*  routine Accept (below).  Must be     */
                                   /*  initialised before parser starts.    */
PRIVATE int scope = 0;

PRIVATE int writing;               /* set to one while parsing arguments*/
PRIVATE int reading;			   /* set to one while parsing arguments*/

int prec[256];						/*Array to store operator precedences*/
int operatorInstruction[256];		/*Array to store operator instructions*/

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
PRIVATE void RestOfStatement( SYMBOL *target );
PRIVATE void Variable( void );
PRIVATE void Expression( void );
PRIVATE int BooleanExpression( void );
PRIVATE void Identifier( void );
PRIVATE void ActualParameter( void );
PRIVATE void ProcCallList( void );
PRIVATE void Assignment( void );
PRIVATE void CompoundTerm( void );
PRIVATE void AddOp( void );
PRIVATE int RelOp( void );
PRIVATE void MultOp( void );
PRIVATE void Term( void );
PRIVATE void SubTerm( void );
PRIVATE void IntConst( void );
PRIVATE SYMBOL *LookupSymbol( void );
PRIVATE void ParseOpPrec( int minPrec );
PRIVATE void MakeSymbolTableEntry( int symtype );

/*S-Algol Recovery Functions*/
PRIVATE void Synchronise( SET *F, SET *FB );
PRIVATE void SetupSets( void );


/*S-Algol Recovery Sets*/
SET ParseBlockFBS;
SET ParseBlockFS_aug;

SET ParseProgramFBS;
SET ParseProgramFS_aug1;
SET ParseProgramFS_aug2;

SET ParseProcDeclarationsFS_aug1;
SET ParseProcDeclarationsFS_aug2;
SET ParseProcDeclarationsFBS;



/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  Main: Parser entry point.  Sets up parser globals (opens input and */
/*        output files, initialises current lookahead), then calls          */
/*        "ParseProgram" to start the parse.                                */
/*                                                                          */
/*--------------------------------------------------------------------------*/

PUBLIC int main ( int argc, char *argv[] )
{
  /*Initialize READ & WRITE flags*/	
  reading = 0;					
  writing = 0;
  
  /*Assign precedence values to each operator*/	
  prec[ADD] = 10;				
  prec[SUBTRACT] = 10;
  prec[MULTIPLY] = 20;
  prec[DIVIDE] = 20;
  prec[SEMICOLON] = -1;
  prec[RIGHTPARENTHESIS] = -1;
  prec[LESS] = -1;
  prec[GREATER] = -1;
  prec[LESSEQUAL] = -1;
  prec[GREATEREQUAL] = -1;
  prec[DO] = -1;
  prec[THEN] = -1;
       
  /*Assign instructions to operators*/
  operatorInstruction[ADD] = I_ADD;		
  operatorInstruction[SUBTRACT] = I_SUB;
  operatorInstruction[MULTIPLY] = I_MULT;
  operatorInstruction[DIVIDE] = I_DIV;
	
  ErrorFlag = 0;
  if ( OpenFiles( argc, argv ) )  {
    InitCharProcessor( InputFile, ListFile );
    InitCodeGenerator(CodeFile);	/*Initialize the code generator*/
    CurrentToken = GetToken();
    SetupSets();
    ParseProgram();
    WriteCodeFile();		/*Write out assembly to CodeFile and close it*/
    fclose( InputFile );
    fclose( ListFile );
    if(ErrorFlag == 1) {
      printf("Syntax Error Detected\n");
      return EXIT_FAILURE;
    }
    printf("Valid Syntax\n");
    return  EXIT_SUCCESS;
  }
  else
    {
      printf("Invalid Command Line Inputs\n");
      return EXIT_FAILURE;
    }   
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
    while ( CurrentToken.code == PROCEDURE ){
      ParseProcDeclaration();
    }
    Synchronise( &ParseProgramFS_aug2, &ParseProgramFBS );
    ParseBlock();
    Accept( ENDOFPROGRAM );
    _Emit(I_HALT);
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
    int vcount = 0;	/*Counter used to count program variable*/
	
    Accept( VAR );
    MakeSymbolTableEntry(STYPE_VARIABLE);	/*Make variable entry in table*/
    Accept( IDENTIFIER );

    vcount++;
    while ( CurrentToken.code == COMMA )
	{
        Accept( COMMA );
	MakeSymbolTableEntry(STYPE_VARIABLE); /*Make variable entry*/
        Accept( IDENTIFIER );
	vcount++;
    }
    Accept( SEMICOLON );
    Emit(I_INC, vcount);	/*Inc number of variables counted*/

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
  MakeSymbolTableEntry( STYPE_PROCEDURE ); /*Make symbol table entry*/
  Accept( IDENTIFIER );  
  scope++;				/*Increase precedence for procedure variables*/
  if ( CurrentToken.code == LEFTPARENTHESIS ) ParameterList();
  Accept(SEMICOLON );
  Synchronise( &ParseProcDeclarationsFS_aug1, &ParseProcDeclarationsFBS );
  if ( CurrentToken.code == VAR ) ParseDeclarations();
  Synchronise( &ParseProcDeclarationsFS_aug2, &ParseProcDeclarationsFBS );
  while ( CurrentToken.code == PROCEDURE ){
    ParseProcDeclaration();
  }
  Synchronise( &ParseProcDeclarationsFS_aug2, &ParseProcDeclarationsFS_aug2 );
  ParseBlock();
  Accept( SEMICOLON );
  
  RemoveSymbols(scope);
  scope--;
  
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
  Synchronise( &ParseBlockFS_aug, &ParseBlockFBS );

  while (CurrentToken.code == IDENTIFIER || CurrentToken.code == WHILE || CurrentToken.code == IF || 
	 CurrentToken.code == READ || CurrentToken.code == WRITE)
        {
  
 ParseStatement();
    
    Accept( SEMICOLON );
	}
  Synchronise( &ParseBlockFS_aug, &ParseBlockFBS );
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
    SYMBOL *target; 			/*Create a SYMBOL pointer*/
    target = LookupSymbol(); 	/*Look up IDENTIFIER in lookahead*/
    Accept( IDENTIFIER );
    RestOfStatement(target);
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
	int Label1, Label2, L2BackPatchLoc;		/*Declare integer variables*/
	Accept( WHILE );

	Label1 = CurrentCodeAddress( );			/*Get current code address*/
	L2BackPatchLoc = BooleanExpression( );	/*Call BooleanExpression and Assign to L2BackPatchLoc*/
	Accept( DO );
      	ParseBlock();						/*Call ParseBlock()*/
	Emit( I_BR, Label1 );					/*Branch back to Label1 at Start of the while loop*/
	Label2 = CurrentCodeAddress( );
	BackPatch( L2BackPatchLoc, Label2 );	/*Backpatch from Label2 to L2BackPatchLoc*/
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
/*	BackPatching works almost the same as in WhileStatement()	    */
/*	but without the branch back to the start			    */
/*--------------------------------------------------------------------------*/

PRIVATE void IfStatement( void )
{
	int L1Label, L2Label, LBackPatchLoc;	/*Declare Integer variables*/
	Accept( IF );
	LBackPatchLoc = BooleanExpression();	/*Call BooleanExpression and Assign to L2BackPatchLoc*/
	Accept( THEN );
	ParseBlock();				/*Call ParseBlock()*/
	while ( CurrentToken.code == ELSE ){
		Accept( ELSE);
		L1Label = CurrentCodeAddress();		/*Get current code address*/
		BackPatch(LBackPatchLoc, L1Label);	/*Backpatch from L1Label to LBackPatchLoc*/
		ParseBlock();
         }
	BackPatch(LBackPatchLoc, L2Label);		/*Backpatch from L2Label to LBackPatchLoc*/
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
	reading = 1;
    Accept( READ );
    ProcCallList();
	reading = 0;
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  RestOfStatement implements:                                             */
/*                                                                          */
/*       <RestOfStatement>   :== <ProcCallList>|<Assignment>|ε              */
/*                                                                          */
/*    Inputs:       SYMBOL *target                                          */
/*                                                                          */
/*    Outputs:      None                                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*    Side Effects: Lookahead token advanced.                               */
/*                                                                          */
/*	Function uses a switch statement to determine how to parse to the   */
/*	rest of a statement.						    */
/*--------------------------------------------------------------------------*/


PRIVATE void RestOfStatement( SYMBOL *target )
{
	switch ( CurrentToken.code ) {
		case LEFTPARENTHESIS:
			ProcCallList( );
			
		case SEMICOLON:
			if ( target != NULL && target->type == STYPE_PROCEDURE ) {
				Emit( I_CALL, target->address );	/*Call to the target Address*/
			}
			else {
				printf("Error: Not a procedure\n");	/*Print Error and kill Code generation*/
				KillCodeGeneration();
			}
			break; 

		case ASSIGNMENT:
		default:
			Assignment();
			if ( target != NULL && target->type == STYPE_VARIABLE ) {
				Emit( I_STOREA, target->address );	/*Store to the target Address*/
			}
			else {
				printf("Error: undeclared variable\n");	/*Print Error and kill Code generation*/
				KillCodeGeneration();
			}
			break;
	} /* End switch code */
} /* End of ParseRestOfStatement code */

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
	writing = 1;
    Accept( WRITE );
    ProcCallList();
	writing = 0;
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
/*    Outputs:      int BackPatchAddr                                       */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*	This function determines the relationship between two expressions.  */
/*	It emits some assembly code and returns an int BackPatchAddr	    */
/*--------------------------------------------------------------------------*/

PRIVATE int BooleanExpression( void )
{
	int BackPatchAddr;					/*Declare integer variables*/
	int RelOpInstruction;
	Expression();						/*Expression 1*/
	RelOpInstruction = RelOp();
	Expression();						/*Expression 2*/
	_Emit( I_SUB );
	BackPatchAddr = CurrentCodeAddress( ); /*Get the current code address*/
	
	Emit( RelOpInstruction, 0 );		/*Emit the operator from Relop() Dummy Target address for branch
	                                         Backpatched to correct address by caller*/
	return BackPatchAddr;
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  RelOp implements:                                                       */
/*                                                                          */
/*       <RelOp>   :== "="|"<="|">="|"<"|">"                                */
/*                                                                          */
/*    Inputs:       None                                                    */
/*                                                                          */
/*    Outputs:      int RelopInstruction                                    */
/*                                                                          */
/*    Returns:      Nothing                                                 */
/*                                                                          */
/*	This function uses a switch statement to determine which operator   */
/*	is being parsed. It accepts the operator and then returns the  	    */
/*	instruction for that operator					    */
/*--------------------------------------------------------------------------*/

PRIVATE int RelOp(void)
{
    int RelOpInstruction;
    switch(CurrentToken.code)
    {
        case EQUALITY:
            RelOpInstruction = I_BZ;
            Accept(EQUALITY);
            break;
        case LESSEQUAL:
            RelOpInstruction = I_BG;
            Accept(LESSEQUAL);
            break;
        case GREATEREQUAL:
            RelOpInstruction = I_BL;
            Accept(GREATEREQUAL);
            break;
        case LESS:
            RelOpInstruction = I_BGZ;
            Accept(LESS);
            break;
        case GREATER:
            RelOpInstruction = I_BLZ;
            Accept(GREATER);
            break;
    }
    return RelOpInstruction;
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
  else {
	  SyntaxError( IDENTIFIER, CurrentToken );
	  KillCodeGeneration();
  }
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

/*COMPOUNDTERM IS NOT USED IN COMP*/

PRIVATE void CompoundTerm( void )
{
	int op;
	Term();
	while ( (op = CurrentToken.code) == MULTIPLY || op == DIVIDE ){
    MultOp();
    Term();
	if ( op == MULTIPLY ) _Emit( I_MULT ); else _Emit( I_DIV );
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
	int negateflag = 0;						/*Flag to determine if term
											  is negative*/
	if ( CurrentToken.code == SUBTRACT ) {
		negateflag = 1;
		Accept( SUBTRACT );
	}
	SubTerm();
	if ( negateflag ) _Emit( I_NEG );		/*if flag is set Emit I_NEG*/
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
/*	Function uses a switch statement whether current token code is	    */
/*	an IDENTIFIER, INTCONST or LEFTPARENTHESIS and then parses	    */
/*	them accordingly						    */
/*--------------------------------------------------------------------------*/
PRIVATE void SubTerm( void )
 {
	 SYMBOL *var;						/*Declare SYMBOL pointer*/
	 switch ( CurrentToken.code ) {
		 case IDENTIFIER:
		 default:
			 var = LookupSymbol( );		/*Look in Symbol table*/
			 if ( var != NULL && var->type == STYPE_VARIABLE ) {
				 
				if (writing) {					/*Flag handlers for READ 
												  and WRITE*/
                    Emit(I_LOADA,var->address);
                }
                else if (reading) {
                    Emit(I_STOREA,var->address);
                }
				else {
					Emit( I_LOADA, var->address );
				  }
			 }
			 else {						/*Print error and kill
										  code generation*/
				printf("Error: Name undeclared or not a variable\n");
				KillCodeGeneration();
			 }
			 Accept( IDENTIFIER );
			 break; 
		 
		 case INTCONST:
			 Emit( I_LOADI, CurrentToken.value );
			 Accept( INTCONST );
			 break;
			 
		 case LEFTPARENTHESIS :
			 Accept( LEFTPARENTHESIS );
			 Expression();
			 Accept( RIGHTPARENTHESIS );
			 break;
	 }
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
  Term();				/*Call to parse term and operator precedence*/
  ParseOpPrec(0);
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
  
  if(reading) {                 	/*emit READ before each parameter*/
        _Emit(I_READ);
    }
  ActualParameter();
  if(writing) {               		/*emit WRITE after each parameter*/
       _Emit(I_WRITE);
    }
	
  while ( CurrentToken.code == COMMA ){
    Accept( COMMA );
	if(reading) {                 	/*emit READ before each parameter*/
			_Emit(I_READ);
		}
    ActualParameter();
	if(writing) {               	/*emit WRITE after each parameter*/
			_Emit(I_WRITE);
		}
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
  static int recovering = 0;
  if ( recovering ) {
    while ( CurrentToken.code != ExpectedToken && CurrentToken.code != ENDOFINPUT ) CurrentToken = GetToken();
    recovering = 0;
  }
  if ( CurrentToken.code != ExpectedToken ) {
      SyntaxError( ExpectedToken, CurrentToken );
      recovering = 1;
      ErrorFlag = 1;
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

    if ( argc != 4 )  {
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
    
    if ( NULL == ( CodeFile = fopen( argv[3], "w" ) ) )  {
        fprintf( stderr, "cannot open \"%s\" for output\n", argv[3] );
        fclose( InputFile );
        fclose(ListFile);
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

/*--------------------------------------------------------------------------*/
/*    S-Algol Error Recovery                                                */
/*                                                                          */
/*    Synchronise: Implements S-Algol recovery. Allows parser to            */
/*      resync on any element of a given set.                               */                                       
/*                                                                          */
/*    Inputs: SET *F, SET*FB                                                */
/*    Outputs: SyntaxError2( *F, CurrentToken )                             */
/*    Returns: Nothing                                                      */
/*--------------------------------------------------------------------------*/

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

/*--------------------------------------------------------------------------*/
/*    S-Algol Error Recovery                                                */
/*                                                                          */
/*    SetUpSets: Implements S-Algol recovery. Sets up sets of elements the  */
/*    parser can re-synchronise on.                                         */                                       
/*                                                                          */
/*    Inputs: Nothing                                                       */
/*    Outputs: Sync Sets                                                    */
/*    Returns: Nothing                                                      */
/*--------------------------------------------------------------------------*/

PRIVATE void SetupSets( void )
{
  /*ParseBlock S-Algol Sync Sets*/
  ClearSet( &ParseBlockFS_aug );
  AddElements( &ParseBlockFS_aug, 6, IDENTIFIER, WHILE, IF, READ, WRITE, END );
  ClearSet( &ParseBlockFBS );
  AddElements( &ParseBlockFBS, 4, ENDOFPROGRAM, SEMICOLON, ELSE, ENDOFINPUT );

  /*ParseProgram S-Algol Sync Sets*/
  ClearSet( &ParseProgramFS_aug1 );
  AddElements( &ParseProgramFS_aug1, 3, VAR, PROCEDURE, BEGIN );

  ClearSet( &ParseProgramFS_aug2 );
  AddElements( &ParseProgramFS_aug2, 2, PROCEDURE, BEGIN );

  ClearSet( &ParseProgramFBS );
  AddElements( &ParseProgramFBS,3,END,ENDOFPROGRAM,ENDOFINPUT);

  /*ParseProcDeclarations S-Algol Sync Sets*/
  ClearSet( &ParseProcDeclarationsFS_aug1 );
  AddElements( &ParseProcDeclarationsFS_aug1, 3, VAR, PROCEDURE, BEGIN );

  ClearSet( &ParseProcDeclarationsFS_aug2 );
  AddElements( &ParseProcDeclarationsFS_aug2, 2, PROCEDURE, BEGIN );

  ClearSet( &ParseProcDeclarationsFBS );
  AddElements( &ParseProcDeclarationsFBS, 3, END,ENDOFPROGRAM,ENDOFINPUT );
 
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  MakeSymbolTableEntry(int symtype)                                       */
/*                                                                          */
/*  Function takes int symtype as a parameter, checks whether current token */
/*  is already in the symbol table and if not, makes an entry		    */
/*--------------------------------------------------------------------------*/

PRIVATE void MakeSymbolTableEntry( int symtype )
{
    /*〈Variable Declarations here〉*/
    SYMBOL *newsptr; /*new symbol pointer*/
    SYMBOL *oldsptr; /*old symbol pointer*/
    char *cptr;      /*current pointer*/
    int hashindex;
    static int varaddress = 0;
	 
	 if ( CurrentToken.code == IDENTIFIER ) {
		 if ( NULL == ( oldsptr = Probe( CurrentToken.s, &hashindex )) || oldsptr->scope < scope ) {
			 if ( oldsptr == NULL ) cptr = CurrentToken.s; else cptr = oldsptr->s;
			 if ( NULL == ( newsptr = EnterSymbol( cptr, hashindex ))) {
				/*<Fatal internal error in EnterSymbol, compiler must exit: code for this goes here>*/
				printf("Fatal internal error in EnterSymbol\n");
				KillCodeGeneration();
			 }	
			 else {
				 if ( oldsptr == NULL ) PreserveString();
				 newsptr->scope = scope;
				 newsptr->type = symtype;
				 if ( symtype == STYPE_VARIABLE ) {
					newsptr->address = varaddress; varaddress++;
				 }
				 else {
					 newsptr->address = -1;
				 }
			 }
		 }
		 else {
			 /*<Error, variable already declared: code for this goes here>*/
			 printf("Error, variable already declared\n");
			 KillCodeGeneration();
		 }
	 }
} 

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  *LookupSymbol( void )                                       			*/
/*                                                                        	*/
/*--------------------------------------------------------------------------*/

PRIVATE SYMBOL *LookupSymbol( void )
{
	static char errorbuffer[M_LINE_WIDTH+2];	/*Used for printing errors*/
	
	 SYMBOL *sptr;								/*Declare SYMBOL pointer*/
	 if ( CurrentToken.code == IDENTIFIER ) {
		 sptr = Probe( CurrentToken.s, NULL );	/*Check for previous symbol
												  table entry*/
		 if ( sptr == NULL ) {					/*Print error and kill
												  code generation*/
			 sprintf(errorbuffer, "Identifier \"%s\" not declared\n", CurrentToken.s);
			 printf( "%d Identifier not declared", CurrentToken.pos );
			 KillCodeGeneration( );
		 }
	 }
	 else sptr = NULL;
	 return sptr;
}

/*--------------------------------------------------------------------------*/
/*                                                                          */
/*  ParseOpPrec( int minPrec )                                       		*/
/*                                                                        	*/
/*	This function determines the precedence of different operators 			*/
/*	when parsing an expression. It takes int minPrec as a parameter to 		*/
/*	guard against end of statement operators								*/
/*--------------------------------------------------------------------------*/

PRIVATE void ParseOpPrec( int minPrec )
{
  int op1, op2;					/*Declare variables*/

  op1 = CurrentToken.code;		/*Assign the current operator codeto op1*/
  while( prec[op1] >= minPrec ){	/*while op1 is a valid operator*/
    CurrentToken = GetToken();		/*Advance token*/
    Term();
    op2 = CurrentToken.code;	/*Assign the current operator codeto op2*/	
    if( prec[op2] > prec[op1] ){	/*Determine whether op1 or op2 has
									  higher precedence*/
      ParseOpPrec( prec[op1] + 1 );
    }
    _Emit( operatorInstruction[op1] );	/*Emit operator instruction*/
    op1 = CurrentToken.code;	/*Assign the current operator codeto op1*/
  }
}
