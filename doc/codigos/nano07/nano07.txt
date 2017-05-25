# Inclusão do comando condicional

set     I1, 1 # atribuição
eq      I1, 1, VERDADEIRO
branch  FIM

VERDADEIRO:
print   I1
print   "\n"

FIM:
end
