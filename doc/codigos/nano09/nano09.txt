# Atribuição de duas operações aritmeticas sobre inteiros a uma variável

set     I1, 1
set     I2, 2
div     I3, I1, I2
add     I4, I1, I3

eq      I4, 1, VERDADEIRO
print   "0\n"
branch  FIM

VERDADEIRO:
print   I4
print   "\n"

FIM:
end
