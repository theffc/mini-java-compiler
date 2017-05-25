# Ler dois inteiros e decidir qual e maior
.loadlib 'io_ops'

set       S1, "Digite o primeiro numero: "
set       S2, "Digite o segundo numero: "
set       S3, "o primeiro numero"
set       S4, "o segundo numero"
set       S5, " e maior que "

print     S1
read      S10, 3
set       I1, S10
print     S2
read      S11, 3
set       I2, S11

gt        I1, I2, VERDADEIRO
print     S4
print     S5
print     S3
print     "\n"
branch    FIM

VERDADEIRO:
print     S3
print     S5
print     S4
print     "\n"

FIM:
end
