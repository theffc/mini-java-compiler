# Inclusão do comando condicional senão

set     I1, 1
eq      I1, 1, VERDADEIRO
print   "0\n"
branch  FIM

VERDADEIRO:
print   I1
print   "\n"

FIM:
end
