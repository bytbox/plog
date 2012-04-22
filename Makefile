all: plog

plog: plog.hs
	ghc --make plog

clean:
	${RM} plog *.o *.hi

