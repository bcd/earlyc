/*
 *	C pass 2 header
 */

/* BCD: Note in early C, the field name of the structures had to be unique across all
 * structs.  Each structure does not provide its own namespace.
 *
 * bnode, tname, and tconst are properly specializations of tnode, what would be
 * described by a union in future versions of C. */

struct	tnode { /* BCD: Expression tree node */
	int	op;
	int	type;
	int	degree;
	struct	tnode *tr1, *tr2;
};

struct	bnode { /* BCD: Boolean tree node */
	int	bop;
	struct	tnode *btree; /* BCD: btree == boolean expression */
	int	lbl;
	int	cond;
};

struct	tname { /* BCD: Name tree node */
	int	nop;
	int	ntype;
	int	elsize;
	char	class;
	char	regno;
	int	offset;
	int	nloc;
};

struct	tconst { /* BCD: Constant tree node. */
	int	cop;
	int	ctype;
	int	cdeg;
	int	value;
};

struct	optab { /* BCD: A codegen table entry */
	char	tabdeg1; /* BCD: Maximum degree of operand 1 */
	char	tabtyp1; /* BCD: Shape of operand 1 */
	char	tabdeg2; /* BCD: Maximum degree of operand 2 */
	char	tabtyp2; /* BCD: Shape of operand 2 */
	char	*tabstring; /* BCD: Expansion rule */
};

struct	table { /* BCD: one of regtab, efftab, optab, or sptab */
	int	tabop;
	struct	optab *tabp;
};

struct	instab {
	int	iop;
	char	*str1;
	char	*str2;
};

struct	swtab {
	int	swlab;
	int	swval;
};

char	maprel[];
char	notrel[];
int	nreg;
int	isn;
int	namsiz;
int	line;
char	binbuf[518];
char	ascbuf[518];
int	nerror;
/* BCD: The first four tables are output from the 'cvopt' program.
 * What is lsptab?  instab is defined in c1t.s. but doesn't appear
 * in the pass 2 source otherwise. */
struct	table	cctab[];
struct	table	efftab[];
struct	table	regtab[];
struct	table	sptab[];
struct	table	lsptab[];
struct	instab	instab[];
int	opdope[];
int	nstack;
int	nfloat;
int	*spacep;
int	*spacemax;
int	*treebase;
struct tconst czero, cone, fczero;

#define	swsiz	200
#define	ossiz	500
/*
	operators
*/

/* BCD: note these are shared between pass 1 (parsing) and pass 2
 * (code generation)
 */
#define	EOF	0
#define	SEMI	1
#define	LBRACE	2
#define	RBRACE	3
#define	LBRACK	4
#define	RBRACK	5
#define	LPARN	6
#define	RPARN	7
#define	COLON	8
#define	COMMA	9

#define	KEYW	19
#define	NAME	20
#define	CON	21
#define	STRING	22
#define	FCON	23
#define	SFCON	24

#define	AUTOI	27
#define	AUTOD	28
#define	INCBEF	30 /* BCD: preincrement */
#define	DECBEF	31 /* BCD: predecrement */
#define	INCAFT	32 /* BCD: postincrement */
#define	DECAFT	33 /* BCD: postdecrement */
#define	EXCLA	34
#define	AMPER	35
#define	STAR	36
#define	NEG	37
#define	COMPL	38

#define	DOT	39
#define	PLUS	40
#define	MINUS	41
#define	TIMES	42
#define	DIVIDE	43
#define	MOD	44
#define	RSHIFT	45
#define	LSHIFT	46
#define	AND	47
#define	OR	48
#define	EXOR	49
#define	ARROW	50
#define	ITOF	51
#define	FTOI	52
#define	LOGAND	53
#define	LOGOR	54

#define	EQUAL	60
#define	NEQUAL	61
#define	LESSEQ	62
#define	LESS	63
#define	GREATEQ	64
#define	GREAT	65
#define	LESSEQP	66
#define	LESSP	67
#define	GREATQP	68
#define	GREATP	69

#define	ASPLUS	70
#define	ASMINUS	71
#define	ASTIMES	72
#define	ASDIV	73
#define	ASMOD	74
#define	ASRSH	75
#define	ASLSH	76
#define	ASSAND	77
#define	ASOR	78
#define	ASXOR	79
#define	ASSIGN	80
#define	ASSNAND	81

#define	QUEST	90
#define	CALL1	98
#define	CALL2	99
#define	CALL	100
#define	MCALL	101
#define	JUMP	102
#define	CBRANCH	103
#define	INIT	104
#define	SETREG	105
#define	LOAD	106
#define	RFORCE	110
#define	BRANCH	111
#define	LABEL	112

/*
	types
*/
#define	INT	0
#define	CHAR	1
#define	FLOAT	2
#define	DOUBLE	3
#define	STRUCT	4
#define	RSTRUCT	5
#define	PTR	010
#define	FUNC	020
#define	ARRAY	030

/*
	storage	classes
*/
#define	KEYWC	1
#define	MOS	4
#define	AUTO	5
#define	EXTERN	6
#define	STATIC	7
#define	REG	8
#define	STRTAG	9
#define	ARG	10
#define	OFFS	12
#define	XOFFS	13
#define	SOFFS	14

/*
	Flag	bits
*/

#define	BINARY	01
#define	LVALUE	02
#define	RELAT	04
#define	ASSGOP	010
#define	LWORD	020
#define	RWORD	040
#define	COMMUTE	0100
#define	RASSOC	0200
#define	LEAF	0400
