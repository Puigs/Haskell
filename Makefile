##
## EPITECH PROJECT, 2020
## Makefile
## File description:
## Makefile
##

NAME	=	funEvalExpr

STACK		=	stack

all: $(NAME)

$(NAME):
	$(STACK) build
	$(STACK) install --local-bin-path .

clean:
	$(STACK) clean

fclean: clean
	rm -rf $(NAME)

re: fclean all
