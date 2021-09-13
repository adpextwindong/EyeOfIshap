clean:
		rm Lexer.hs Parser.hs

Parser:
		happy Parser.y

Lexer:
		alex Lexer.x

all:  Lexer Parser
