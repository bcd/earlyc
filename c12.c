#
/*
 *		C compiler part 2 -- expression optimizer
 *
 */

#include "c1h.c"
/* BCD: From the Tour paper:
 * Each expression tree, as it is read in, is subjected to a fairly comprehensive
 * analysis.  This is performed by the optim routine and a number of subroutines;
 * the major things done are 1. Modifications and simplifications of the tree so
 * its value may be computed more efficiently and conveniently by the code generator.
 * 2. Marking each interior node with an estimate of the number of registers
 * required to evaluate it.  This register count is needed to guide the code
 * generation algorithm. One thing that is definitely not done is discovery or
 * exploitation of common subexpressions, nor is this done anywhere in the compiler.
 *
 * BCD: Note, this returns another tree.  Also as this is c12.c, these routines were
 * not in the earlier versions of the compiler, so this is the first attempt at
 * optimizing the code.  This function performs both machine-independent and machine-
 * dependent changes.
 */
optim(atree)
struct tnode *atree;
{
	register op, dope;
	int d1, d2;
	struct tnode *t;
	register struct tnode *tree;

	/* BCD: Null trees, or trees without an operation (free trees), are returned as-is. */
	if ((tree=atree)==0)
		return(0);
	if ((op = tree->op)==0)
		return(tree);

	/* BCD: Automatic (local) variables have their class set to OFFS (offset from stack)
	 * using the base register r5 on the PDP-11. */
	if (op==NAME && tree->class==AUTO) {
		tree->class = OFFS;
		tree->regno = 5;
		tree->offset = tree->nloc;
	}
	dope = opdope[op];
	if ((dope&LEAF) != 0) /* BCD: leaf nodes can't be simplified */
		return(tree);
	if ((dope&BINARY) == 0) /* BCD: non-binary operators are simplfied by the unary optimizer */
		return(unoptim(tree));
	/* is known to be binary */
	/* BCD: New to v6, 'char' is promoted to 'int'. */
	if (tree->type==CHAR)
		tree->type = INT;
	if ((dope&COMMUTE)!=0) {
	acomm:	d1 = tree->type;
		tree = acommute(tree);
		tree->type = d1;
		/*
		 * PDP-11 special:
		 * replace a&b by a NAND ~ b.
		 * This will be undone when in
		 * truth-value context.
		 */
		if (tree->op!=AND)
			return(tree);
		tree->op = NAND;
		tree->tr2 = block(1, COMPL, tree->tr2->type, 0, tree->tr2);
	}
    again:
	/* BCD: below are non-commutative binary operations.
	 * Recursively optim. each of the operands first.
	 */
	tree->tr1 = optim(tree->tr1);
	tree->tr2 = optim(tree->tr2);
	if ((dope&RELAT) != 0) {
		/* BCD: From the Tour: Relationals are turned around so the more complicated
		 * expression is on the left.  (So that `2 > f(x)' becomes `f(x) < 2').
		 * This improves code generation since the algorithm prefers to have the right operand
		 * require fewer registers than the left. */
		if ((d1=degree(tree->tr1)) < (d2=degree(tree->tr2))
		 || d1==d2 && tree->tr1->op==NAME && tree->tr2->op!=NAME) {
			t = tree->tr1;
			tree->tr1 = tree->tr2;
			tree->tr2 = t;
			tree->op = maprel[op-EQUAL];
		}
		/* BCD: Change the type of a constant in the right operand
		 * from INT to CHAR, if it is within the valid range and the left
		 * operand is also a CHAR. */
		if (tree->tr1->type==CHAR && tree->tr2->op==CON
		 && (dcalc(tree->tr1) <= 12 || tree->tr1->op==STAR)
		 && tree->tr2->value <= 127 && tree->tr2->value >= 0)
			tree->tr2->type = CHAR;
	}

	/* BCD: Assign the Sethi-Ullman number to each operand.  degree() returns a
	 * generalized degree that is not just register-based.  It may be negative
	 * in some cases.  The max() function converts these values to something
	 * closer to a register count. */
	d1 = max(degree(tree->tr1), islong(tree->type));
	d2 = max(degree(tree->tr2), 0);
	if (tree->tr1->type==LONG && dope&RELAT)
		d1 = 10;
	switch (op) {

	case LTIMES:
	case LDIV:
	case LMOD:
	case LASTIMES:
	case LASDIV:
	case LASMOD:
		tree->degree = 10;
		break;

	/*
	 * PDP-11 special:
	 * generate new =&~ operator out of =&
	 * by complementing the RHS.
	 */
	case ASSAND:
		op = ASSNAND;
		tree->op = op;
		tree->tr2 = block(2, COMPL, tree->tr2->type, 0, tree->tr2);
		goto again;

	case NAND:
		if (isconstant(tree->tr2) && tree->tr2->value==0)
			return(tree->tr1);
		goto def;

	case CALL:
		tree->degree = 10;
		break;

	case QUEST:
	case COLON:
		tree->degree = max(d1, d2);
		break;

	case MINUS:
		if (t = isconstant(tree->tr2)) {
			/* BCD: Convert X-N to X+(-N) */
			tree->op = PLUS;
			/* BCD: Bug fix in v6: handle double constants being negated correctly */
			if (t->type==DOUBLE)
				/* PDP-11 FP representation */
				t->value =^ 0100000;
			else
				t->value = -t->value;
			/* BCD: and now the operation is commutative, so can simplify further there */
			goto acomm;
		}
		goto def;

	case DIVIDE:
	case ASDIV:
	case ASTIMES:
		/* BCD: Fold multiply/divide by constant 1 */
		if (tree->tr2->op==CON && tree->tr2->value==1)
			return(tree->tr1);

		/* BCD: Modulus has a higher degree, when not working with 2^N */
		if (ispow2(tree) == 0) {

		case MOD:
		case ASMOD:
			d1 =+ 2;
			d2 =+ 2;
		}
		if (tree->type==LONG)
			return(hardlongs(tree));
		goto constant;

	case LSHIFT:
	case RSHIFT:
	case ASRSH:
	case ASLSH:
		/* BCD: Eliminate zero shifts */
		if (tree->tr2->op==CON && tree->tr2->value==0)
			return(tree->tr1);
		/*
		 * PDP-11 special: turn right shifts into negative
		 * left shifts
		 */
		if (op==LSHIFT||op==ASLSH)
			goto constant;
		if (tree->tr2->op==CON && tree->tr2->value==1)
			goto constant;
		op =+ (LSHIFT-RSHIFT);
		tree->op = op;
		tree->tr2 = block(1, NEG, tree->type, 0, tree->tr2);
		goto again;

	constant:
		if (tree->tr1->op==CON && tree->tr2->op==CON) {
			/* BCD: Fold constant expressions.  Use tr1 to hold the result.
			 * No need to allocate a new tree for the answer.
			 * X prefix added to make friendly to newer tools where 'const' is
			 * reserved. */
			Xconst(op, &tree->tr1->value, tree->tr2->value);
			return(tree->tr1);
		}


	def:
	default:
		/* BCD: This is the Sethi-Ullman algorithm in a nutshell.  If the two
		 * operands have equal degree N, then the operator has degree N+1,
		 * else the maximum of the two operands. */
		tree->degree = d1==d2? d1+islong(tree->type): max(d1, d2);
		break;
	}
	return(tree);
}

unoptim(atree)
struct tnode *atree;
{
	register struct tnode *subtre, *tree;
	register int *p;
	double static fv;
	struct { int integer; };

	if ((tree=atree)==0)
		return(0);
	if (tree->op==CBRANCH) {
		tree->btree = optim(tree->btree);
		return(tree);
	}
	subtre = tree->tr1 = optim(tree->tr1);
	switch (tree->op) {

	case FSEL:
		tree->tr1 = block(2, RSHIFT, INT, 0, subtre,
		    block(1, CON, INT, 0, tree->bitoffs));
		tree->op = AND;
		tree->type = INT;
		tree->tr2 = block(1, CON, INT, 0, (1<<tree->flen)-1);
		return(optim(tree));

	case AMPER:
		if (subtre->op==STAR)
			return(subtre->tr1);
		if (subtre->op==NAME && subtre->class == OFFS) {
			p = block(2, PLUS, tree->type, 1, subtre, tree);
			subtre->type = tree->type;
			tree->op = CON;
			tree->type = INT;
			tree->degree = 0;
			tree->value = subtre->offset;
			subtre->class = REG;
			subtre->nloc = subtre->regno;
			subtre->offset = 0;
			return(p);
		}
		break;

	case STAR:
		if (subtre->op==AMPER)
			return(subtre->tr1);
		if (subtre->op==NAME && subtre->class==REG) {
			subtre->type = tree->type;
			subtre->class = OFFS;
			subtre->regno = subtre->nloc;
			return(subtre);
		}
		p = subtre->tr1;
		if ((subtre->op==INCAFT||subtre->op==DECBEF)&&tree->type!=LONG
		 && p->op==NAME && p->class==REG && p->type==subtre->type) {
			p->type = tree->type;
			p->op = subtre->op==INCAFT? AUTOI: AUTOD;
			return(p);
		}
		if (subtre->op==PLUS && p->op==NAME && p->class==REG) {
			if (subtre->tr2->op==CON) {
				p->offset =+ subtre->tr2->value;
				p->class = OFFS;
				p->type = tree->type;
				p->regno = p->nloc;
				return(p);
			}
			if (subtre->tr2->op==AMPER) {
				subtre = subtre->tr2->tr1;
				subtre->class =+ XOFFS-EXTERN;
				subtre->regno = p->nloc;
				subtre->type = tree->type;
				return(subtre);
			}
		}
		break;
	case EXCLA:
		if ((opdope[subtre->op]&RELAT)==0)
			break;
		tree = subtre;
		tree->op = notrel[tree->op-EQUAL];
		break;

	case NEG:
	case COMPL:
		if (tree->type==CHAR)
			tree->type = INT;
		if (tree->op == subtre->op)
			return(subtre->tr1);
		if (subtre->op==ITOL) {
			subtre->op = tree->op;
			subtre->type = INT;
			tree->op = ITOL;
		}
	}

	/* BCD: Integer constant folding for unaries */
	if (subtre->op == CON) switch(tree->op) {

	case NEG:
		subtre->value = -subtre->value;
		return(subtre);

	case COMPL:
		subtre->value = ~subtre->value;
		return(subtre);

	case ITOF:
		fv = subtre->value;
		p = &fv;
		p++;
		if (*p++==0 && *p++==0 && *p++==0) {
			subtre->type = DOUBLE;
			subtre->value = fv.integer;
			subtre->op = SFCON;
			return(subtre);
		}
		break;
	}

	/* BCD: Sethi-Ullman for a unary operator.  A slightly optimized version
	 * of the general algorithm above for binaries, where one of the arguments
	 * is zero. */
	tree->degree = max(islong(tree->type), degree(subtre));
	return(tree);
}

struct acl {
	int nextl;
	int nextn;
	struct tnode *nlist[20];
	struct tnode *llist[21];
};

/* BCD: From the Tour:
 * The acommute routine, called for associative and commutative operators,
 * discovers clusters of the same operator at the top levels of the current
 * tree, and arranges them in a list: for `a+((b+c)+(d+f))' the list would
 * be `a,b,c,d,e,f'.  After each subtree is optimized, the list is sorted in
 * decreasing difficulty of computation; as mentioned above, the code
 * generation algorithm works best when left operands are the difficult ones. */
acommute(atree)
{
	struct acl acl;
	int d, i, op, flt;
	register struct tnode *t1, **t2, *tree;
	struct tnode *t;

	acl.nextl = 0;
	acl.nextn = 0;
	tree = atree;
	op = tree->op;
	flt = isfloat(tree);
	insert(op, tree, &acl);
	acl.nextl--;
	t2 = &acl.llist[acl.nextl];
	if (!flt) {
		/* put constants together */
		for (i=acl.nextl;i>0&&t2[0]->op==CON&&t2[-1]->op==CON;i--) {
			acl.nextl--;
			t2--;
			Xconst(op, &t2[0]->value, t2[1]->value);
		}
	}
	if (op==PLUS || op==OR) {
		/* toss out "+0" */
		if (acl.nextl>0 && (t1 = isconstant(*t2)) && t1->value==0) {
			acl.nextl--;
			t2--;
		}
		if (acl.nextl <= 0)
			return(*t2);
		/* subsume constant in "&x+c" */
		if (op==PLUS && t2[0]->op==CON && t2[-1]->op==AMPER) {
			t2--;
			t2[0]->tr1->offset =+ t2[1]->value;
			acl.nextl--;
		}
	} else if (op==TIMES || op==AND) {
		t1 = acl.llist[acl.nextl];
		if (t1->op==CON) {
			/* BCD: Handle times zero (return zero) or times one (return self). */
			/* BCD: in v6, also handles X AND zero. */
			if (t1->value==0)
				return(t1);
			if (op==TIMES && t1->value==1 && acl.nextl>0)
				if (--acl.nextl <= 0)
					return(acl.llist[0]);
		}
	}
	if (op==PLUS && !flt)
		distrib(&acl);
	/* BCD: Put all the operands back together again */
	tree = *(t2 = &acl.llist[0]);
	d = max(degree(tree), islong(tree->type));
	if (op==TIMES && !flt)
		d++;
	for (i=0; i<acl.nextl; i++) {
		t1 = acl.nlist[i];
		t1->tr2 = t = *++t2;
		t1->degree = d = d==degree(t)? d+islong(t1->type): max(d, degree(t));
		t1->tr1 = tree;
		tree = t1;
		if (tree->type==LONG) {
			if (tree->op==TIMES)
				tree = hardlongs(tree);
			else if (tree->op==PLUS && (t = isconstant(tree->tr1))
			       && t->value < 0) {
				tree->op = MINUS;
				t->value = - t->value;
				t = tree->tr1;
				tree->tr1 = tree->tr2;
				tree->tr2 = t;
			}
		}
	}
	if (tree->op==TIMES && ispow2(tree))
		tree->degree = max(degree(tree->tr1), islong(tree->type));
	return(tree);
}

distrib(list)
struct acl *list;
{
/*
 * Find a list member of the form c1c2*x such
 * that c1c2 divides no other such constant, is divided by
 * at least one other (say in the form c1*y), and which has
 * fewest divisors. Reduce this pair to c1*(y+c2*x)
 * and iterate until no reductions occur.
 */
	register struct tnode **p1, **p2;
	struct tnode *t;
	int ndmaj, ndmin;
	struct tnode **dividend, **divisor;
	struct tnode **maxnod, **mindiv;

    loop:
	maxnod = &list->llist[list->nextl];
	ndmaj = 1000;
	dividend = 0;
	for (p1 = list->llist; p1 <= maxnod; p1++) {
		if ((*p1)->op!=TIMES || (*p1)->tr2->op!=CON)
			continue;
		ndmin = 0;
		for (p2 = list->llist; p2 <= maxnod; p2++) {
			if (p1==p2 || (*p2)->op!=TIMES || (*p2)->tr2->op!=CON)
				continue;
			if ((*p1)->tr2->value == (*p2)->tr2->value) {
				(*p2)->tr2 = (*p1)->tr1;
				(*p2)->op = PLUS;
				(*p1)->tr1 = (*p2);
				*p1 = optim(*p1);
				squash(p2, maxnod);
				list->nextl--;
				goto loop;
			}
			if (((*p2)->tr2->value % (*p1)->tr2->value) == 0)
				goto contmaj;
			if (((*p1)->tr2->value % (*p2)->tr2->value) == 0) {
				ndmin++;
				mindiv = p2;
			}
		}
		if (ndmin > 0 && ndmin < ndmaj) {
			ndmaj = ndmin;
			dividend = p1;
			divisor = mindiv;
		}
    contmaj:;
	}
	if (dividend==0)
		return;
	t = list->nlist[--list->nextn];
	p1 = dividend;
	p2 = divisor;
	t->op = PLUS;
	t->type = (*p1)->type;
	t->tr1 = (*p1);
	t->tr2 = (*p2)->tr1;
	(*p1)->tr2->value =/ (*p2)->tr2->value;
	(*p2)->tr1 = t;
	t = optim(*p2);
	if (p1 < p2) {
		*p1 = t;
		squash(p2, maxnod);
	} else {
		*p2 = t;
		squash(p1, maxnod);
	}
	list->nextl--;
	goto loop;
}

squash(p, maxp)
struct tnode **p, **maxp;
{
	register struct tnode **np;

	for (np = p; np < maxp; np++)
		*np = *(np+1);
}

const(op, vp, av)
int *vp;
{
	register int v;

	v = av;
	switch (op) {

	case PLUS:
		*vp =+ v;
		return;

	case TIMES:
		*vp =* v;
		return;

	case AND:
		*vp =& v;
		return;

	case OR:
		*vp =| v;
		return;

	case EXOR:
		*vp =^ v;
		return;

	case DIVIDE:
	case MOD:
		if (v==0)
			error("Divide check");
		else
			if (op==DIVIDE)
				*vp =/ v;
			else
				*vp =% v;
		return;

	case RSHIFT:
		*vp =>> v;
		return;

	case LSHIFT:
		*vp =<< v;
		return;

	case NAND:
		*vp =& ~ v;
		return;
	}
	error("C error: const");
}

insert(op, atree, alist)
struct acl *alist;
{
	register d;
	register struct acl *list;
	register struct tnode *tree;
	int d1, i;
	struct tnode *t;

	tree = atree;
	list = alist;
	if (tree->op == op) {
	ins:	list->nlist[list->nextn++] = tree;
		insert(op, tree->tr1, list);
		insert(op, tree->tr2, list);
		return;
	}
	tree = optim(tree);
	if (tree->op == op)
		goto ins;
	if (!isfloat(tree)) {
		/* c1*(x+c2) -> c1*x+c1*c2 */
		if ((tree->op==TIMES||tree->op==LSHIFT)
		  && tree->tr2->op==CON && tree->tr2->value>0
		  && tree->tr1->op==PLUS && tree->tr1->tr2->op==CON) {
			d = tree->tr2->value;
			if (tree->op==TIMES)
				tree->tr2->value =* tree->tr1->tr2->value;
			else
				tree->tr2->value = tree->tr1->tr2->value << d;
			tree->tr1->tr2->value = d;
			tree->tr1->op = tree->op;
			tree->op = PLUS;
			if (op==PLUS)
				goto ins;
		}
	}
	d = degree(tree);
	for (i=0; i<list->nextl; i++) {
		if ((d1=degree(list->llist[i]))<d) {
			t = list->llist[i];
			list->llist[i] = tree;
			tree = t;
			d = d1;
		}
	}
	list->llist[list->nextl++] = tree;
}

/* BCD: Allocates a tree node.  See c01.c for version in pass 1.
 * This is needed in some cases when the tree needs to be expanded
 * from what was passed in from pass 1.  spacep points to the first
 * word of free space.
 * The allocation will have at least 3 words, for the common size of a
 * tree (see struct tnode; it provides op, type, and degree).  an
 * specifies the number of additional words needed. Their initializers
 * are passed on the stack also, using a primitive form of varargs. */
block(an, args)
{
	register int *p;
	int *oldp;
	register *argp, n;

	oldp = p = spacep;
	n = an+3;
	argp = &args;
	do
		*p++ = *argp++;
	while (--n);
	if (p >= &treespace[ossiz]) {
		error("Exp. ov. pass 2");
		exit(1);
	}
	spacep = p;
	return(oldp);
}

islong(t)
{
	if (t==LONG)
		return(2);
	return(1);
}

isconstant(at)
struct tnode *at;
{
	register struct tnode *t;

	t = at;
	if (t->op==CON || t->op==SFCON)
		return(t);
	if (t->op==ITOL && t->tr1->op==CON)
		return(t->tr1);
	return(0);
}

hardlongs(at)
struct tnode *at;
{
	register struct tnode *t;

	t = at;
	switch(t->op) {

	case TIMES:
	case DIVIDE:
	case MOD:
		t->op =+ LTIMES-TIMES;
		break;

	case ASTIMES:
	case ASDIV:
	case ASMOD:
		t->op =+ LASTIMES-ASTIMES;
		t->tr1 = block(1, AMPER, LONG+PTR, 0, t->tr1);
		break;

	default:
		return(t);
	}
	return(optim(t));
}
