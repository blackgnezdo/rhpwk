# $Id$

SQLPORTSPATH=	/usr/local/share/sqlports

HC=		ghc
HCFLAGS=	-O2 \
		-cpp -DSQLPORTSPATH=\"${SQLPORTSPATH}\" \
		-package HDBC \
		-package HDBC-sqlite3 \
		-main-is RHPWK

PROG=	rhpwk
SRCS=	RHPWK.hs Database/Sqlports.hs

CLEANFILES+=	${OBJS:R:S/$/.hi/}

${PROG}: ${OBJS}
	${HC} ${HCFLAGS} -o ${.TARGET} ${OBJS}

.SUFFIXES: .hi .hs .o

.hs.hi:
	${HC} ${HCFLAGS} -c ${.IMPSRC} -ohi ${.TARGET:R}.hi -o ${.TARGET:R}.o

.hs.o:
	${HC} ${HCFLAGS} -c ${.IMPSRC} -ohi ${.TARGET:R}.hi -o ${.TARGET:R}.o

.PHONY: depend
depend: ${SRCS}
	@rm -f .depend
	${HC} -M -dep-makefile .depend ${HCFLAGS} ${.ALLSRC}

.include <bsd.prog.mk>
