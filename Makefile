all: plog

plog: plog.hs
	ghc -W --make plog

clean:
	${RM} plog *.o *.hi

