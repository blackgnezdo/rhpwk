# To build this, you have to
#	pkg_add ghc hs-HDBC hs-HDBC-sqlite3 hs-regex-compat
#
# Then
#	make depend && make
#
# To use it (well, it's not yet very useful at all),
#	pkg_add sqlports

PREFIX=		/usr/local
LOCALBASE=	${PREFIX}
SQLPORTSPATH=	${LOCALBASE}/share/sqlports
GHCPKGDB=	${LOCALBASE}/lib/ghc/package.conf.d

HC=		ghc
HCFLAGS=	-O2 \
		-cpp -DSQLPORTSPATH=\"${SQLPORTSPATH}\" \
		     -DGHCPKGDB=\"${GHCPKGDB}\" \
		-package Cabal \
		-package HDBC \
		-package HDBC-sqlite3 \
		-package directory \
		-package filepath \
		-package regex-compat \
		-main-is RHPWK

PROG=	rhpwk
SRCS=	RHPWK.hs Cabal/Cabal.hs Database/Sqlports.hs Database/GhcPkg.hs

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
