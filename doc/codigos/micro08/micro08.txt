# Decide se um número é maior ou menor que 10
.loadlib 'io_ops'

set       I1, 1                     # variavel numero

TESTE_LOOP:
ne        I1, 0, LOOP
branch    FIM

LOOP:
print     "Digite um numero: "
read      S10, 3
set       I1, S10

gt        I1, 10, MAIOR
print     "O numero "
print     I1
print     " e menor que 10.\n"
branch    TESTE_LOOP

MAIOR:
print     "O numero "
print     I1
print     " e maior que 10.\n"
branch    TESTE_LOOP

FIM:
end
