all:
	flex proiect.l
	yacc -d proiect.y
	gcc lex.yy.c y.tab.c -lm
	./a.out file.txt
