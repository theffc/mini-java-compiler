# Comando condicional aninhado com um de repeticao

set     I1, 1
set     I2, 2
set     I3, 5

TESTE_ENQUANTO:
gt      I3, I1, LOOP
branch  FIM

LOOP:
eq      I1, I2, VERDADEIRO
print   "0\n"
branch  POS_CONDICIONAL

VERDADEIRO:
print   I1
print   "\n"

POS_CONDICIONAL:
dec     I3              # decrementa I3 (x)
branch  TESTE_ENQUANTO

FIM:
end
