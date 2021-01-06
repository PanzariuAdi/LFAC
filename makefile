all:
	flex proiect.l
	yacc -d proiect.y
	gcc lex.yy.c y.tab.c
	./a.out file.txt
