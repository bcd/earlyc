#
/*
 *  C compiler
 */

#include "c1h.c"

/* BCD: Many helper routines here */

max(a, b)
{
	if (a>b)
		return(a);
	return(b);
}

/* BCD: From the Tour:
 * The `degree of difficulty' computed is actually finer than the mere number of
 * registers required; a constant is considered simpler than the address of a static
 * or external, which is simpler than reference to a variable.
 */
degree(at)
struct tnode *at;
{
	register struct tnode *t, *t1;

	if ((t=at)==0 || t->op==0)
		return(0);
	if (t->op == CON)
		return(-3);
	if (t->op == AMPER)
		return(-2);
	if (t->op==ITOL && (t1 = isconstant(t)) && t1->value>= 0)
		return(-2);
	if ((opdope[t->op] & LEAF) != 0) {
		if (t->type==CHAR || t->type==FLOAT)
			return(1);
		return(0);
	}
	return(t->degree);
}

/* BCD: Print a tree node, assembler-friendly */
pname(ap, flag)
struct tnode *ap;
{
	register i;
	register struct tnode *p;

	p = ap;
loop:
	switch(p->op) {

	case SFCON:
	case CON:
		printf("$");
		psoct(p->value);
		return;

	case FCON:
		printf("L%d", p->value);
		return;

	case NAME:
		i = p->offset;
		if (flag==2)
			i =+ 2;
		if (i) {
			psoct(i);
			if (p->class!=OFFS)
				putchar('+');
			if (p->class==REG)
				regerr();
		}
		switch(p->class) {

		case SOFFS:
		case XOFFS:
			pbase(p);

		case OFFS:
			printf("(r%d)", p->regno);
			return;

		case EXTERN:
		case STATIC:
			pbase(p);
			return;

		case REG:
			printf("r%d", p->nloc);
			return;

		}
		error("Compiler error: pname");
		return;

	case AMPER:
		putchar('$');
		p = p->tr1;
		if (p->op==NAME && p->class==REG)
			regerr();
		goto loop;

	case AUTOI:
		printf("(r%d)%c", p->nloc, flag==1?0:'+');
		return;

	case AUTOD:
		printf("%c(r%d)", flag==1?0:'-', p->nloc);
		return;

	case STAR:
		p = p->tr1;
		putchar('*');
		goto loop;

	}
	error("pname called illegally");
}

regerr()
{
	error("Illegal use of register");
}

pbase(ap)
struct tnode *ap;
{
	register struct tnode *p;

	p = ap;
	if (p->class==SOFFS || p->class==STATIC)
		printf("L%d", p->nloc);
	else
		printf("_%.8s", &(p->nloc));
}

/* BCD: Yet another variation of dcalc(), which penalizes some
 * byte-mode instructions */
xdcalc(ap, nrleft)
struct tnode *ap;
{
	register struct tnode *p;
	register d;

	p = ap;
	d = dcalc(p, nrleft);
	if (d<20 && p->type==CHAR) {
		if (nrleft>=1)
			d = 20;
		else
			d = 24;
	}
	return(d);
}


/* BCD: dcalc() is a layer above degree(), as it is not just based on Sethi-
 * Ullman numbering, but other factors of difficulty.  Floating-point and
 * forms of indirection are penalized further.  Also, if the estimated number
 * of registers needed is bigger than the number available, penalize more.
 * Not sure what the values here actually mean, but it is probably related
 * to PDP-11 cycle counts. */
dcalc(ap, nrleft)
struct tnode *ap;
{
	register struct tnode *p, *p1;

	if ((p=ap)==0)
		return(0);
	switch (p->op) {

	case NAME:
		if (p->class==REG)
			return(9);

	case AMPER:
	case FCON:
	case AUTOI:
	case AUTOD:
		return(12);

	case CON:
	case SFCON:
		if (p->value==0)
			return(4);
		if (p->value==1)
			return(5);
		if (p->value > 0)
			return(8);
		return(12);

	case STAR:
		p1 = p->tr1;
		if (p1->op==NAME||p1->op==CON||p1->op==AUTOI||p1->op==AUTOD)
			if (p->type!=LONG)
				return(12);
	}
	if (p->type==LONG)
		nrleft--;
	return(p->degree <= nrleft? 20: 24);
}

/* BCD: The main routine for checking if a tree 'ap' has incompatible
 * type with the spec given in 'ast'.  op is the parent operator.
 * This routine could use some preprocessor love.
 * See "types" in c1h.c for the bit definitions, also remember that
 * composite types have multiple values shifted up.
 */
notcompat(ap, ast, op)
struct tnode *ap;
{
	register at, st;
	register struct tnode *p;

	p = ap;
	at = p->type;
	st = ast;
	if (st==0)		/* word, byte */
		return(at>CHAR && at<PTR);
	if (st==1)		/* word */
		return(at>INT && at<PTR);
	st =- 2;
	if ((at&(~(TYPE+XTYPE))) != 0)
		at = 020;
	if ((at&(~TYPE)) != 0)
		at = at&TYPE | 020;
	if (st==FLOAT && at==DOUBLE)
		at = FLOAT;
	if (p->op==NAME && p->class==REG && op==ASSIGN && st==CHAR)
		return(0);
	return(st != at);
}

prins(op, c, itable)
struct instab *itable;
{
	register struct instab *insp;
	register char *ip;

	for (insp=itable; insp->op != 0; insp++) {
		if (insp->op == op) {
			ip = c? insp->str2: insp->str1;
			if (ip==0)
				break;
			printf("%s", ip);
			return;
		}
	}
	error("No match' for op %d", op);
}

collcon(ap)
struct tnode *ap;
{
	register op;
	register struct tnode *p;

	p = ap;
	if (p->op==STAR)
		p = p->tr1;
	if (p->op==PLUS) {
		op = p->tr2->op;
		if (op==CON || op==AMPER)
			return(1);
	}
	return(0);
}

/* BCD: Returns a character to be appended to an instruction mnemonic.
 * Both 'float' and 'double' types will return 'f', else a zero character
 * (which doesn't print anything). */
isfloat(at)
struct tnode *at;
{
	register struct tnode *t;

	t = at;
	if ((opdope[t->op]&RELAT)!=0)
		t = t->tr1;
	if (t->type==FLOAT || t->type==DOUBLE) {
		nfloat = 1;
		return('f');
	}
	return(0);
}

oddreg(t, areg)
struct tnode *t;
{
	register reg;

	reg = areg;
	if (!isfloat(t))
		switch(t->op) {
		case DIVIDE:
		case MOD:
		case ASDIV:
		case ASMOD:
			reg++;

		case TIMES:
		case ASTIMES:
			return(reg|1);
		}
	return(reg);
}

/* BCD: get argument length of a simple type as it appears on the stack. */
arlength(t)
{
	if (t>=PTR)
		return(2);
	switch(t) {

	case INT:
	case CHAR:
		return(2);

	case LONG:
		return(4);

	case FLOAT:
	case DOUBLE:
		return(8);
	}
	return(1024);
}

/*
 * Strings for switch code.
 */

char	dirsw[] {"\
cmp	r0,$%o\n\
jhi	L%d\n\
asl	r0\n\
jmp	*L%d(r0)\n\
.data\n\
L%d:\
" };

char	simpsw[] {"\
mov	$L%d,r1\n\
mov	r0,L%d\n\
L%d:cmp	r0,(r1)+\n\
jne	L%d\n\
jmp	*L%d-L%d(r1)\n\
.data\n\
L%d:\
"};

char	hashsw[] {"\
mov	r0,r1\n\
clr	r0\n\
div	$%o,r0\n\
asl	r1\n\
add	$L%d,r1\n\
mov	r0,*(r1)+\n\
mov	(r1)+,r1\n\
L%d:cmp	r0,-(r1)\n\
jne	L%d\n\
jmp	*L%d-L%d(r1)\n\
.data\n\
L%d:\
"};

/* BCD: Special handling to emit switch statements.
 * Considerable effort is done here to make switches efficient.  No doubt,
 * the compiler itself depended on these techniques; observe the number
 * of switch statements that are present in the source code here. */
pswitch(afp, alp, deflab)
struct swtab *afp, *alp;
{
	int tlab, ncase, i, j, tabs, worst, best, range;
	register struct swtab *swp, *fp, *lp;
	int poctab[swsiz];

	fp = afp;
	lp = alp;
	if (fp==lp) {
		printf("jbr	L%d\n", deflab);
		return;
	}
	tlab = isn++;
	if (sort(fp, lp))
		return;
	ncase = lp-fp;
	lp--;

	/* BCD: The table ranges from 'afp' (first value) to 'alp' (last value). */
	range = lp->swval - fp->swval;

	/* direct switch */
	if (range>0 && range <= 3*ncase) {
		/* BCD: The canonical implementation of a jump table.  The minimum
		 * value is subtracted off, to yield an index.  If the expression is
		 * higher than any case value, then jump to the default.  Else,
		 * scale the index by a pointer size (2x) and jump indirect.
		 * This implementation works best when the case value are dense over
		 * the range.
		 *
		 * If the range is too large, then this is skipped, to avoid making a
		 * big jump table, when it would mostly have default entries. */
		if (fp->swval)
			printf("sub	$%o,r0\n", fp->swval);
		printf(dirsw, range, deflab, isn, isn);
		isn++;
		for (i=fp->swval; i<=lp->swval; i++) {
			if (i==fp->swval) {
				printf("L%d\n", fp->swlab);
				fp++;
			} else
				printf("L%d\n", deflab);
		}
		goto esw;
	}
	/* simple switch */
	if (ncase<8) {
		/* BCD: Alternate implementation as a variable length of array of
		 * value/label pairs.  At most 8 of these also to save space.  The range
		 * does not have to be continuous though.  Uses indirect addressing.
		 * The range is irrelevant here. */
		i = isn++;
		j = isn++;
		printf(simpsw, i, j, isn, isn, j, i, i);
		isn++;
		for (; fp<=lp; fp++)
			printf("%o\n", fp->swval);
		printf("L%d:..\n", j);
		for (fp = afp; fp<=lp; fp++)
			printf("L%d\n", fp->swlab);
		printf("L%d\n", deflab);
		goto esw;
	}
	/* hash switch */
	best = 077777;
	for (i=ncase/4; i<=ncase/2; i++) {
		for (j=0; j<i; j++)
			poctab[j] = 0;
		for (swp=fp; swp<=lp; swp++)
			poctab[lrem(0, swp->swval, i)]++;
		worst = 0;
		for (j=0; j<i; j++)
			if (poctab[j]>worst)
				worst = poctab[j];
		if (i*worst < best) {
			tabs = i;
			best = i*worst;
		}
	}
	i = isn++;
	printf(hashsw, tabs, isn, i, i, isn+tabs+1, isn+1, isn);
	isn++;
	for (i=0; i<=tabs; i++)
		printf("L%d\n", isn+i);
	for (i=0; i<tabs; i++) {
		printf("L%d:..\n", isn++);
		for (swp=fp; swp<=lp; swp++)
			if (lrem(0, swp->swval, tabs) == i)
				printf("%o\n", ldiv(0, swp->swval, tabs));
	}
	printf("L%d:", isn++);
	for (i=0; i<tabs; i++) {
		printf("L%d\n", deflab);
		for (swp=fp; swp<=lp; swp++)
			if (lrem(0, swp->swval, tabs) == i)
				printf("L%d\n", swp->swlab);
	}
esw:
	printf(".text\n");
}

/* BCD: Sort the array of switch statement values (the 'case's). */
sort(afp, alp)
struct swtab *afp, *alp;
{
	register struct swtab *cp, *fp, *lp;
	int intch, t;

	fp = afp;
	lp = alp;
	while (fp < --lp) {
		intch = 0;
		for (cp=fp; cp<lp; cp++) {
			if (cp->swval == cp[1].swval) {
				error("Duplicate case (%d)", cp->swval);
				return(1);
			}
			if (cp->swval > cp[1].swval) {
				intch++;
				t = cp->swval;
				cp->swval = cp[1].swval;
				cp[1].swval = t;
				t = cp->swlab;
				cp->swlab = cp[1].swlab;
				cp[1].swlab = t;
			}
		}
		if (intch==0)
			break;
	}
	return(0);
}

/* BCD: Return tree value if it is an integer constant power of 2. */
ispow2(atree)
{
	register int d;
	register struct tnode *tree;

	tree = atree;
	if (!isfloat(tree) && tree->tr2->op==CON) {
		d = tree->tr2->value;
		if (d>1 && (d&(d-1))==0)
			return(d);
	}
	return(0);
}

/* BCD: Convert multiply/divide into equivalent shift when the
 * operand is a constant integer power of two */
pow2(atree)
struct tnode *atree;
{
	register int d, i;
	register struct tnode *tree;

	tree = atree;
	if (d = ispow2(tree)) {
		for (i=0; (d=>>1)!=0; i++);
		tree->tr2->value = i;
		d = tree->op;
		tree->op = d==TIMES? LSHIFT:
			  (d==DIVIDE? RSHIFT:
			  (d==ASTIMES? ASLSH: ASRSH));
		/* BCD: New in v6, the new tree might be further optimized. */
		tree = optim(tree);
	}
	return(tree);
}

cbranch(atree, albl, cond, areg)
struct tnode *atree;
{
	int l1, op;
	register lbl, reg;
	register struct tnode *tree;

	lbl = albl;
	reg = areg;
	if ((tree=atree)==0)
		return;
	switch(tree->op) {

	case LOGAND:
		if (cond) {
			cbranch(tree->tr1, l1=isn++, 0, reg);
			cbranch(tree->tr2, lbl, 1, reg);
			label(l1);
		} else {
			cbranch(tree->tr1, lbl, 0, reg);
			cbranch(tree->tr2, lbl, 0, reg);
		}
		return;

	case LOGOR:
		if (cond) {
			cbranch(tree->tr1, lbl, 1, reg);
			cbranch(tree->tr2, lbl, 1, reg);
		} else {
			cbranch(tree->tr1, l1=isn++, 1, reg);
			cbranch(tree->tr2, lbl, 0, reg);
			label(l1);
		}
		return;

	case EXCLA:
		cbranch(tree->tr1, lbl, !cond, reg);
		return;

	case COMMA:
		rcexpr(tree->tr1, efftab, reg);
		tree = tree->tr2;
		break;
	}
	op = tree->op;
	if (tree->type==LONG || opdope[op]&RELAT&&tree->tr1->type==LONG) {
		if (tree->type!=LONG) {
			tree->op = MINUS;
			tree->type = LONG;
			tree = optim(tree);
		} else
			op = NEQUAL;
		rcexpr(tree, regtab, 0);
		printf("ashc	$0,r0\n");
		branch(lbl, op, !cond);
		return;
	}
	rcexpr(tree, cctab, reg);
	op = tree->op;
	if ((opdope[op]&RELAT)==0)
		op = NEQUAL;
	else {
		l1 = tree->tr2->op;
	 	if ((l1==CON || l1==SFCON) && tree->tr2->value==0)
			op =+ 200;		/* special for ptr tests */
		else
			op = maprel[op-EQUAL];
	}
	if (isfloat(tree))
		printf("cfcc\n");
	branch(lbl, op, !cond);
}

branch(lbl, aop, c)
{
	register op;

	if(op=aop)
		prins(op, c, branchtab);
	else
		printf("jbr");
	printf("\tL%d\n", lbl);
}

label(l)
{
	printf("L%d:", l);
}

/* BCD: Pop stack arguments after a function call.  The 2-byte and
 * 4-byte versions probably run faster on the PDP-11.  The tst/cmp
 * are not used later, they are just done to avoid any other side
 * effects. */
popstk(a)
{
	switch(a) {

	case 0:
		return;

	case 2:
		printf("tst	(sp)+\n");
		return;

	case 4:
		printf("cmp	(sp)+,(sp)+\n");
		return;
	}
	printf("add	$%o,sp\n", a);
}

error(s, p1, p2, p3, p4, p5, p6)
{
	register f;
	extern fout;

	nerror++;
	flush();
	f = fout;
	fout = 1;
	printf("%d: ", line);
	printf(s, p1, p2, p3, p4, p5, p6);
	putchar('\n');
	flush();
	fout = f;
}

psoct(an)
{
	register int n, sign;

	sign = 0;
	if ((n = an) < 0) {
		n = -n;
		sign = '-';
	}
	printf("%c%o", sign, n);
}

/*
 * Read in an intermediate file.
 */
getree()
{
	struct tnode *expstack[20];
	register struct tnode **sp;
	register t, op;
	static char s[9];
	struct swtab *swp;

	spacep = treespace;
	sp = expstack;
	for (;;) {
		if (sp >= &expstack[20])
			error("Stack botch");
		op = getw(ascbuf);
		if ((op&0177400) != 0177000) {
			error("Intermediate file error");
			exit(1);
		}
		switch(op =& 0377) {

	case EOF:
		return;

	case BDATA:
		printf(".byte ");
		seq(',');
		break;

	case WDATA:
		seq(';');
		break;

	case PROG:
		printf(".text\n");
		break;

	case DATA:
		printf(".data\n");
		break;

	case BSS:
		printf(".bss\n");
		break;

	case SYMDEF:
		printf(".globl	_%s\n", outname(s));
		break;

	case RETRN:
		printf("jmp	cret\n");
		break;

	case CSPACE:
		t = outname(s);
		printf(".comm	_%s,%o\n", t, getw(ascbuf));
		break;

	case SSPACE:
		printf(".=.+%o\n", getw(ascbuf));
		break;

	case EVEN:
		printf(".even\n");
		break;

	case SAVE:
		printf("jsr	r5,csv\n");
		t = getw(ascbuf)-6;
		if (t==2)
			printf("tst	-(sp)\n");
		else if (t > 2)
			printf("sub	$%o,sp\n", t);
		break;

	case PROFIL:
		t = getw(ascbuf);
		printf("mov	$L%d,r0\njsr	pc,mcount\n", t);
		printf(".bss\nL%d:.=.+2\n.text\n", t);
		break;

	case SNAME:
		t = outname(s);
		printf("~%s=L%d\n", t, getw(ascbuf));
		break;

	case ANAME:
		t = outname(s);
		printf("~%s=%o\n", t, getw(ascbuf));
		break;

	case RNAME:
		t = outname(s);
		printf("~%s=r%d\n", t, getw(ascbuf));
		break;

	case SWIT:
		t = getw(ascbuf);
		line = getw(ascbuf);
		swp = treespace;
		while (swp->swlab = getw(ascbuf)) {
			swp->swval = getw(ascbuf);
			swp++;
		}
		pswitch(treespace, swp, t);
		break;

	case EXPR:
		line = getw(ascbuf);
		if (sp != &expstack[1]) {
			error("Expression input botch\n");
			exit(1);
		}
		nstack = 0;
		rcexpr(optim(*--sp), efftab, 0);
		spacep = treespace;
		break;

	case NAME:
		t = getw(ascbuf);
		if (t==EXTERN) {
			t = getw(ascbuf);
			*sp = block(6, NAME, t, 0, EXTERN, 0, 0,0,0,0);
			outname(&(*sp)->nloc);
			sp++;
			break;
		}
		*sp = block(3, NAME, 0, 0, t, 0, 0);
		(*sp)->type = getw(ascbuf);
		(*sp)->nloc = getw(ascbuf);
		sp++;
		break;

	case CON:
	case SFCON:
	case FCON:
		t = getw(ascbuf);
		*sp++ = block(1, op, t, 0, getw(ascbuf));
		break;

	case FSEL:
		t = getw(ascbuf);
		sp[-1] = block(2, op, t, 0, sp[-1], getw(ascbuf));
		break;

	case NULL:
		*sp++ = block(0, 0, 0, 0);
		break;

	case CBRANCH:
		t = getw(ascbuf);
		sp[-1] = block(1, CBRANCH, sp[-1], t, getw(ascbuf));
		break;

	case LABEL:
		label(getw(ascbuf));
		break;

	case NLABEL:
		printf("_%s:\n", outname(s));
		break;

	case RLABEL:
		t = outname(s);
		printf("_%s:\n~~%s:\n", t, t);
		break;

	case BRANCH:
		branch(getw(ascbuf), 0);
		break;

	case SETREG:
		nreg = getw(ascbuf)-1;
		break;

	default:
		if (opdope[op]&BINARY) {
			if (sp < &expstack[1]) {
				error("Binary expression botch");
				exit(1);
			}
			t = *--sp;
			*sp++ = block(2, op, getw(ascbuf), 0, *--sp, t);
		} else {
			sp[-1] = block(1, op, getw(ascbuf), 0, sp[-1]);
		}
		break;
	}
	}
}

outname(s)
{
	register char *p, c;
	register n;

	p = s;
	n = 0;
	while (c = getc(ascbuf)) {
		*p++ = c;
		n++;
	}
	while (n++ < 8)
		*p++ = 0;
	return(s);
}

seq(c)
{
	register o;

	if (getw(ascbuf) == 0)
		return;
	for (;;) {
		printf("%o", getw(ascbuf));
		if ((o = getw(ascbuf)) != 1)
			break;
		printf("%c", c);
	}
	printf("\n");
}
