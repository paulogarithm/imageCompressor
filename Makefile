##
## EPITECH PROJECT, 2024
## Paradigm Pool
## File description:
## Makefile
##

NAME	=	imageCompressor

STACK_BUILD	=	stack build
STACK_PATH	=	stack path
RM	=	rm -rf

BIN_PATH	:=	$(shell $(STACK_PATH) --local-install-root)

all: $(NAME)

$(NAME):
	$(STACK_BUILD)
	cp $(BIN_PATH)/bin/$(NAME) .

clean:
	$(RM) *.hi
	$(RM) *.o

fclean: clean
	$(RM) $(NAME)
	stack clean --full

re:	fclean all
