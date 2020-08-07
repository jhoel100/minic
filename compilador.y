/* Diseo de las reglas de un MINIC */

%{

#include<stdio.h>
#include<ctype.h>
#include<string.h>
#include<stdlib.h>
#include<stdbool.h>

int yylex();
void yyerror();

//Estructura de tipovalor, solo se puede escojer uno de los dos
typedef struct { int entero;
		float real;
		} tipovalor;

void IS(int ,int,int);
char lexema[80];

//Definimos la estructura tipoTablaSimbolo para poder hacer la tabla de simbolos
typedef struct {char nombre[30];
		int a1,a2;  	// a1: INT/FLOAT	a2: FUN/VAR
		tipovalor a3; 	// guarda valor
		int ambito;
	} tipoTablaSimbolo;

//creamos el objeto tabla simbolo y un puntero para comenzar a recorrer
tipoTablaSimbolo TS[100], *pTS;
int nTS = 0;
int insertaSimbolo(char *, int);
int localizaSimbolo(char *);
void muestraSimbolo();
int tipoVar;

// Definicion de la tabla de codigo, que son op=operacion
typedef struct { int op,a1,a2,a3,ambito;}
	tipoCodigo;

tipoCodigo TABCOD[100];
int cx = -1; 		// indice de codigo actual
int indicevartemp = 0;  	//numero de variables temporales
void genCodigo(int ,int ,int ,int,int );
int genvartemp(int);
void muestraCodigo();

void interprete();

int amb=0;
int amb_maximo=0;
bool global=true;

// Definicion de las operaciones de lenguaje intermedio
#define MOVER	1
#define SUMAR	2
#define SALTAR	3
#define SALTARV	4
#define SALTARF	5
#define MENOR	6
#define RESTAR       7 
#define MULTIPLICAR  8
#define DIVIDIR      9
#define MAYOR	10

%}

%token INT FLOAT FORMATO ID IF ELSE NUM REAL WHILE DO FOR FUNCION RETURN
%token LEENUM IMPRINUM LLAMAR
%token CENTERO	%token CFLOAT	%token ID VAR
%right '='
%left '?'
%left OR	%left AND	%right NOT
%left IGUAL NOIGUAL MENORIGUAL MAYORIGUAL '<' '>'
%left '+' '-'
%left '*' '/'

%%
	//programa C, compuesto de lista de declaraciones
	programaC : listaDeclC ;
	//lista de declaraciones puede tener ninguna o varias declaraciones
	listaDeclC : listaDeclC declC | ;

	declC: declC1 | declC2;

	//1--declaracion puede ser un tipo, con una lista de variables de ese tipo
	declC1 : Tipo listaVar ';' asignaciones declC;
	asignaciones: prop ';' asignaciones | ;

	//2--declaracion puede ser una funcion con tipo de devolucion ID y parametros
	//si reconoce esta parte llama a la funcion por ejemplo int main( llamado parametros ) ....
	declC2 : Tipo ID '(' { amb_maximo++; amb=amb_maximo; global=false; IS($1,FUNCION,amb); }	listaPar ')' bloque {global=true;} asignaciones declC | ;

	
	//Por el momento solo acepta INT y FLOAT
	Tipo : INT  | FLOAT ;

	//1.1--Ingresa esa variable a la tabla de simbolos
	listaVar : ID ',' { IS(tipoVar,VAR,amb); }	listaVar | ID { IS(tipoVar,VAR,amb); };
	//2.1--Ingresa los parametros a la tabla de simbolos 
	listaPar : Tipo ID { IS($1,VAR,amb); }',' listaPar   | Tipo ID { IS($1,VAR,amb); };

	//2.2--Un bloque esta conformado por lista de variables y proposiciones
	bloque : '{' repetidor RETURN expr ';' '}'  | '{' repetidor '}';

	repetidor: listaVarLoc listaProp repetidor | ;

	//2.2.1--Ingresa las variables locales
	listaVarLoc : /* Nada */ | Tipo listaVar ';' listaVarLoc ;
	//2.2.2--Lista de proposiciones
	listaProp : listaProp prop | ;
	prop : ';' ;
	prop : bloque ;

	//prop : IF '(' expr ')' {if(global){amb=0;}; genCodigo(SALTARF,$3,0,-1,amb); $$ = cx; } prop  { TABCOD[$5].a3 = cx + 1;  } ;
	prop : IF '(' expr ')' {if(global){amb=0;};genCodigo(SALTARF,$3,0,-1,amb); $$ = cx; } prop  { TABCOD[$5].a3 = cx + 1;  }  ELSE {if(global){amb=0;};genCodigo(SALTARV,$3,0,-1,amb); $$ = cx; } prop { TABCOD[$9].a3 = cx + 1;  } ; 

	prop : WHILE '(' {$$ = cx + 1;}  expr ')' {if(global){amb=0;};genCodigo(SALTARF,$4,0,-1,amb); $$ = cx;} prop	
				      {if(global){amb=0;};genCodigo(SALTAR,0,0,$3,amb); TABCOD[$6].a3 = cx + 1; } ;

	prop :  DO {$$ = cx+1;} prop WHILE '(' {$$ = cx + 1;}  expr ')' {if(global){amb=0;};genCodigo(SALTARF,$7,0,-1,amb); $$ = cx;} {if(global){amb=0;};genCodigo(SALTAR,0,0,$2,amb); TABCOD[$9].a3 = cx + 1; };
	
	prop : FOR '(' expr ';' {$$ = cx + 1;} expr ';' expr ')' {if(global){amb=0;};genCodigo(SALTARF,$6,0,-1,amb); $$ = cx; }  prop {if(global){amb=0;};genCodigo(SALTAR,0,0,$5,amb); TABCOD[$10].a3 = cx + 1; } ;
	
	prop : IMPRINUM '('  expr ')'	{if(global){amb=0;};genCodigo(IMPRINUM,$3,0,0,amb);};
	prop : expr;

	//Posibles proposiciones 
	expr: '(' expr ')' ;
	expr : expr OR expr {if(global){amb=0;}; int n = genvartemp(amb);	genCodigo(OR,n,$1,$3,amb);  $$=n;} ;
	expr : expr AND expr {if(global){amb=0;}; int n = genvartemp(amb); genCodigo(AND,n,$1,$3,amb);  $$=n;} ;
	expr : NOT expr {if(global){amb=0;}; int n = genvartemp(amb);	genCodigo(NOT,n,$2,0,amb);  $$=n;};
	expr : expr IGUAL expr {if(global){amb=0;}; int n = genvartemp(amb); genCodigo(IGUAL,n,$1,$3,amb);  $$=n;} ;
	expr : expr NOIGUAL expr {if(global){amb=0;}; int n = genvartemp(amb);	genCodigo(NOIGUAL,n,$1,$3,amb);  $$=n;} ;
	expr : expr '<' expr   {if(global){amb=0;}; int n = genvartemp(amb); genCodigo(MENOR,n,$1,$3,amb);  $$=n;} ;  /* n = $1 < $3   */
	expr : expr '>' expr {if(global){amb=0;}; int n = genvartemp(amb); genCodigo(MAYOR,n,$1,$3,amb);  $$=n;} ;		/*	a +  b  */
	expr : expr MENORIGUAL expr {if(global){amb=0;}; int n = genvartemp(amb); genCodigo(MENORIGUAL,n,$1,$3,amb);  $$=n;} ;	/*	sumar,a+b,a,b */
	expr : expr MAYORIGUAL expr {if(global){amb=0;}; int n = genvartemp(amb); genCodigo(MAYORIGUAL,n,$1,$3,amb);  $$=n;};
	expr : expr '+' expr  	{if(global){amb=0;}; int n = genvartemp(amb); genCodigo(SUMAR,n,$1,$3,amb);  $$=n;}; /* sumar,a+b,a,b */
	
   expr : expr '-' expr {if(global){amb=0;}; int n = genvartemp(amb); genCodigo(RESTAR,n,$1,$3,amb);$$=n;} ; /*  restar,a-b,a,b   */;
   expr : expr '*' expr {if(global){amb=0;}; int n = genvartemp(amb); genCodigo(MULTIPLICAR,n,$1,$3,amb);$$=n;}; /*multiplicar,a*b,a,b*/;
   expr : expr '/' expr {if(global){amb=0;}; int n = genvartemp(amb); genCodigo(DIVIDIR,n,$1,$3,amb);$$=n;} ; /*  dividir,a/b,a,b   */;
	
	prop : expr '?' {genCodigo(SALTARF,$1,0,-1,amb); $$ = cx; } expr ':' { TABCOD[$3].a3 = cx + 1;  }  {if(global){amb=0;};genCodigo(SALTARV,$1,0,-1,amb); $$ = cx; } expr { TABCOD[$7].a3 = cx + 1;  } ;
	
	//llamadas a funciones
	expr : ID {$$ = localizaSimbolo(lexema);} {if(global){amb=0;};genCodigo(LLAMAR,$2,0,0,amb);}'(' listaParametros ')' ';'  ;
	listaParametros: expr ',' listaParametros | expr ;

	//localiza el lexema y devuelve su posicion
	expr : ID {$$ = localizaSimbolo(lexema);};
	//inserta, localiza el simbolo y convierte el lexema
	expr : NUM  {IS($1,NUM,amb);$$ = localizaSimbolo(lexema);TS[$$].a3.entero = atoi(lexema);};	/* Codigo 1 */
	expr : REAL	{float v; IS($1,REAL,amb);$$ = localizaSimbolo(lexema);	sscanf(lexema,"%f",&v);TS[$$].a3.real = v;};	/* Codigo 2 */
	expr : ID '=' { $$ = localizaSimbolo(lexema); }	expr {if(global){amb=0;};genCodigo(MOVER,$3,$4,0,amb);};	/* Codigo 3 */
	expr : ID '[' expr ']' ;
	expr : ID '[' expr ']' '=' expr;

%%
//llena la tabla de codigo
void genCodigo(int op,int a1,int a2,int a3,int ambito)
{
	tipoCodigo *p;

	cx++;
	p = &TABCOD[cx];
	p->op = op;
	p->a1 = a1;
	p->a2 = a2;
	p->a3 = a3;
	p->ambito=ambito;
}

//varible temporal
int genvartemp(int ambito)
{
	char t[30];
	sprintf(t,"_T%-2d",indicevartemp++);
	strcpy(TS[nTS].nombre,t);
	TS[nTS].a1 = VAR;
	TS[nTS].ambito=ambito;
	return nTS++;
}

//Muestra la tabla de instrucciones de lenguaje intermedio
void muestraCodigo()
{
	int i,op,a1,a2,a3,ambito;
	printf("Instrucciones lenguaje intermedio:\n \n");
	for(i=0;i<=cx;i++) {
		op = TABCOD[i].op; a1 = TABCOD[i].a1;
		a2 = TABCOD[i].a2; a3 = TABCOD[i].a3;
		ambito=TABCOD[i].ambito;
		printf("%2d) ",i);
		switch(op)
		{
			case SUMAR: printf("SUMAR %s = %s + %s    Ambito=%i\n",TS[a1].nombre,TS[a2].nombre,TS[a3].nombre,ambito);break;
			case NOT: printf("NOT %s = %s    Ambito=%i\n",TS[a1].nombre,TS[a2].nombre,ambito);break;
			case MENOR: printf("MENOR %s = %s < %s    Ambito=%i\n",TS[a1].nombre,TS[a2].nombre,TS[a3].nombre,ambito);break;
			case MAYOR: printf("MAYOR %s = %s > %s    Ambito=%i\n",TS[a1].nombre,TS[a2].nombre,TS[a3].nombre,ambito);break;
			case AND: printf("AND %s = %s && %s    Ambito=%i\n",TS[a1].nombre,TS[a2].nombre,TS[a3].nombre,ambito);break;
			case OR: printf("OR %s = %s || %s    Ambito=%i\n",TS[a1].nombre,TS[a2].nombre,TS[a3].nombre,ambito);break;
			case MAYORIGUAL: printf("MAYOR IGUAL %s = %s >= %s    Ambito=%i\n",TS[a1].nombre,TS[a2].nombre,TS[a3].nombre,ambito);break;
			case MENORIGUAL: printf("MENOR IGUAL %s = %s <= %s    Ambito=%i\n",TS[a1].nombre,TS[a2].nombre,TS[a3].nombre,ambito);break;
			case NOIGUAL: printf("NOIGUAL %s = %s != %s    Ambito=%i\n",TS[a1].nombre,TS[a2].nombre,TS[a3].nombre,ambito);break;
			case IGUAL: printf("IGUAL %s = %s == %s    Ambito=%i\n",TS[a1].nombre,TS[a2].nombre,TS[a3].nombre,ambito);break;		
			case SALTAR: printf("SALTAR %d     Ambito=%i\n",a3,ambito);break;
			case SALTARV: printf("SALTARV %s %d     Ambito=%i\n",TS[a1].nombre,a3,ambito);break;
			case SALTARF: printf("SALTARF %s %d     Ambito=%i\n",TS[a1].nombre,a3,ambito);break;
			case MOVER: printf("MOVER %s %s     Ambito=%i\n",TS[a1].nombre,TS[a2].nombre,ambito);break;
			case IMPRINUM: printf("IMPRINUM %s     Ambito=%i\n",TS[a1].nombre,ambito);break;
         case RESTAR      : printf("RESTAR %s = %s - %s    Ambito=%i\n",TS[a1].nombre,TS[a2].nombre,TS[a3].nombre,ambito);break;
         case MULTIPLICAR : printf("MULTIPLICAR %s = %s * %s    Ambito=%i\n", TS[a1].nombre,TS[a2].nombre,TS[a3].nombre,ambito); break;
         case DIVIDIR     : printf("DIVIDIR %s = %s / %s    Ambito=%i\n",TS[a1].nombre,TS[a2].nombre,TS[a3].nombre,ambito);break;
			case LLAMAR     : printf("LLAMAR FUNCION %s NRO DE AMBITO=%i   Ambito=%i\n",TS[a1].nombre,TS[a1].ambito,ambito);break;
		// Los demas casos
		}
	}
	return;
}

//busca el nombre de la variable de argumento en la tabla de simbolos y retorna posicion
int localizaSimbolo(char *n)
{
	int i;
	for(i=0;i<nTS;i++) 
		if(strcmp(n,TS[i].nombre) == 0) 
			return i;
	return -1;
}

//Si no esta en la tabla de simbolos lo agrega 
int insertaSimbolo(char *n, int t)
{
	if(localizaSimbolo(n)>=0) 
		return -1;
	strcpy(TS[nTS].nombre,n);
	TS[nTS].a1 = t;
	TS[nTS].a2 = TS[nTS].a3.real = 0;
	return nTS++;	
}

//Insertar simbolo en la tabla de simbolos 
void IS(int tipo,int clase,int ambito)
{
	int i;
	i = insertaSimbolo(lexema, tipo); 
	TS[i].a2=clase;
	TS[i].ambito=ambito;
}

//Imprime la tabla de simbolos
void muestraSimbolo()
{
	int i;
	printf("Tabla de Simbolos\n \n");
	printf("Nombre       a1    a2   Valor  Ambito\n \n");
	for(i=0,pTS=TS;i<nTS;i++,pTS++)
		printf("%10s %5d %5d %5g %5d\n",pTS->nombre,pTS->a1,pTS->a2,pTS->a3.real,pTS->ambito);
} 

//analizador lexico
int yylex()

{
  int c;  char *p;
  do  c=getchar(); while(isspace(c));
  ;
  //reconoce que palabra resevada es
  if (isalpha(c))
    { p=lexema;
      do  { *p++=c; c=getchar(); } while (isalpha(c));
      ungetc(c,stdin); *p=0;
      if (strcmp(lexema,"if")==0) return IF;
      if (strcmp(lexema,"else")==0) return ELSE;
      if (strcmp(lexema,"int")==0) return tipoVar=yylval=INT;
      if (strcmp(lexema,"float")==0) return tipoVar=yylval=FLOAT;
      if (strcmp(lexema,"while")==0) return WHILE;
      if (strcmp(lexema,"for")==0) return FOR;
      if (strcmp(lexema,"do")==0) return DO;
      if (strcmp(lexema,"print")==0) return IMPRINUM;
      if (strcmp(lexema,"chao")==0) return EOF;
      if (strcmp(lexema,"return")==0) return RETURN;

      /* van otras palabras reservadas */
      
      return yylval=ID;
    }
  
    if ( c=='(' || c==')' || c==';' || c==',' || c=='{' || c=='}' ||
         c==',' || c=='*' || c=='/' || c=='+' || c=='-' || c=='?' ||
         c=='[' || c==']' || c==':' ) return yylval=c;

    if ( c=='!') { 
    	c=getchar();
    	if(c=='=') return NOIGUAL;
    	ungetc(c,stdin); return NOT;
    	}
    	
    if ( c=='=' ) {
    	c=getchar();
    	if(c=='=') return IGUAL;
    	ungetc(c,stdin); return '=';
    	}
    	
    if ( c=='>' ) {
    	c=getchar();
    	if(c=='=') return MAYORIGUAL;
    	ungetc(c,stdin); return '>';
    	}
    	
    if ( c=='<' ) {
    	c=getchar();
    	if(c=='=') return MENORIGUAL;
    	ungetc(c,stdin); return '<';
    	}

    if ( c=='&' ) {
    	c=getchar();
    	if(c=='&') return AND;
    	ungetc(c,stdin); return '&';
    	}
	
    if ( c=='|' ) {
    	c=getchar();
    	if(c=='|') return OR;
    	ungetc(c,stdin); return '|';
    	}

    if (isdigit(c)) { 
    	p=lexema;
      	do  { *p++=c; c=getchar(); } while (isdigit(c))
      	;
      	if(c=='.') { do  { *p++=c; c=getchar(); } while (isdigit(c));
      			 ungetc(c,stdin); *p=0;  return yylval=REAL;}
      	ungetc(c,stdin); *p=0;
		
    	return yylval=REAL;
    }

	//Si no es ninguno manda error	
    yyerror("caracter ilegal !!!");
}

//manda error de sintaxis
void yyerror(char *m)  { 
	fprintf(stderr,"error de sintaxis %s\n",m); 
	getchar(); 
	exit(0);
}

//ejecuta el codigo
void inter_llamada(int objetivo){
	int icx,op,a1,a2,a3,ambito_instruccion;
	float v;
	int main_ambito=objetivo;

	printf("Sub Programa en ejecucion: \n \n");
	icx = 0;
	while(1){
		if(icx==cx+1) break;
		op = TABCOD[icx].op;
		a1 = TABCOD[icx].a1;
		a2 = TABCOD[icx].a2;
		a3 = TABCOD[icx].a3; //valor a donde saltar
		ambito_instruccion=TABCOD[icx].ambito;
		//muestraSimbolo();
		if(ambito_instruccion==main_ambito){
		switch(op)
		{
			//regresa a la tabla de simbolos posicion a3
			case SALTAR : icx = a3; continue;
			//si TS[a1] es falso regresa a la posicion a3 de la tabla de codigo
			case SALTARF : if(TS[a1].a3.real==0) { icx = a3; continue;}
				else break;
			case SALTARV : if(TS[a1].a3.real==1) { icx = a3; continue;}
				else break;
			case IMPRINUM : printf("%8.2f \n",TS[a1].a3.real); break;
			//mueve el valor real de a2 a a1
			case MOVER : TS[a1].a3.real = TS[a2].a3.real; break;
			case NOT : TS[a1].a3.real = ! TS[a2].a3.real; break;
			case SUMAR : TS[a1].a3.real = TS[a2].a3.real + TS[a3].a3.real; break;
			case AND : TS[a1].a3.real = TS[a2].a3.real && TS[a3].a3.real; break;
			case OR : TS[a1].a3.real = TS[a2].a3.real || TS[a3].a3.real; break;			
			case MENOR : TS[a1].a3.real = (TS[a2].a3.real < TS[a3].a3.real); break;
			case MAYOR : TS[a1].a3.real = (TS[a2].a3.real > TS[a3].a3.real); break;
			case MENORIGUAL : TS[a1].a3.real = (TS[a2].a3.real <= TS[a3].a3.real); break;
			case MAYORIGUAL : TS[a1].a3.real = (TS[a2].a3.real >= TS[a3].a3.real); break;
			case NOIGUAL : TS[a1].a3.real = (TS[a2].a3.real != TS[a3].a3.real); break;
			case IGUAL : TS[a1].a3.real = (TS[a2].a3.real == TS[a3].a3.real); break;
         case RESTAR      : TS[a1].a3.real = TS[a2].a3.real - TS[a3].a3.real; break;
         case MULTIPLICAR : TS[a1].a3.real = TS[a2].a3.real * TS[a3].a3.real; break;
         case DIVIDIR     : TS[a1].a3.real = TS[a2].a3.real / TS[a3].a3.real; break;
			case LLAMAR :  inter_llamada(TS[a1].ambito) ; break;
		}
		}
	icx++;
	}
	printf("Fin de Sub Programa \n \n");  //Fin de 'while'
}  //Fin de funcion

//ejecuta el codigo
void interprete(){
	int icx,op,a1,a2,a3,ambito_instruccion;
	float v;
	//Buscamos el ambito del main
	int ubicacion_main=localizaSimbolo("main");
	int main_ambito=TS[ubicacion_main].ambito;

	printf("Programa en ejecucion: \n \n");
	icx = 0;
	while(1){
		if(icx==cx+1) break;
		op = TABCOD[icx].op;
		a1 = TABCOD[icx].a1;
		a2 = TABCOD[icx].a2;
		a3 = TABCOD[icx].a3; //valor a donde saltar
		ambito_instruccion=TABCOD[icx].ambito;
		//muestraSimbolo();
		if(ambito_instruccion==main_ambito || ambito_instruccion==0){
		switch(op)
		{
			//regresa a la tabla de simbolos posicion a3
			case SALTAR : icx = a3; continue;
			//si TS[a1] es falso regresa a la posicion a3 de la tabla de codigo
			case SALTARF : if(TS[a1].a3.real==0) { icx = a3; continue;}
				else break;
			case SALTARV : if(TS[a1].a3.real==1) { icx = a3; continue;}
				else break;
			case IMPRINUM : printf("%8.2f \n",TS[a1].a3.real); break;
			//mueve el valor real de a2 a a1
			case MOVER : TS[a1].a3.real = TS[a2].a3.real; break;
			case NOT : TS[a1].a3.real = ! TS[a2].a3.real; break;
			case SUMAR : TS[a1].a3.real = TS[a2].a3.real + TS[a3].a3.real; break;
			case AND : TS[a1].a3.real = TS[a2].a3.real && TS[a3].a3.real; break;
			case OR : TS[a1].a3.real = TS[a2].a3.real || TS[a3].a3.real; break;			
			case MENOR : TS[a1].a3.real = (TS[a2].a3.real < TS[a3].a3.real); break;
			case MAYOR : TS[a1].a3.real = (TS[a2].a3.real > TS[a3].a3.real); break;
			case MENORIGUAL : TS[a1].a3.real = (TS[a2].a3.real <= TS[a3].a3.real); break;
			case MAYORIGUAL : TS[a1].a3.real = (TS[a2].a3.real >= TS[a3].a3.real); break;
			case NOIGUAL : TS[a1].a3.real = (TS[a2].a3.real != TS[a3].a3.real); break;
			case IGUAL : TS[a1].a3.real = (TS[a2].a3.real == TS[a3].a3.real); break;
         case RESTAR      : TS[a1].a3.real = TS[a2].a3.real - TS[a3].a3.real; break;
         case MULTIPLICAR : TS[a1].a3.real = TS[a2].a3.real * TS[a3].a3.real; break;
         case DIVIDIR     : TS[a1].a3.real = TS[a2].a3.real / TS[a3].a3.real; break;
			case LLAMAR : inter_llamada(TS[a1].ambito) ; break;
		}
		}
	icx++;
	}  //Fin de 'while'
}  //Fin de funcion
	
int main()  {
	printf("\n");
	//llamada al generador
	yyparse();
	muestraCodigo();
	printf("\n");	
	interprete();
	printf("\n");
	muestraSimbolo();
	return 0;
}
