# Le um numero e verifica se ele esta entre 100 e 200
.loadlib 'io_ops'

set       S1, "Digite um numero: "
set       S2, "O numero esta no intervalo entre 100 e 200\n"
set       S3, "O numero nao esta no intervalo entre 100 e 200\n"

print     S1
read      S10, 3
set       I1, S10

ge        I1, 100, MAIOR_QUE_100
branch    NAO_ESTA_NO_INTERVALO

MAIOR_QUE_100:
le        I1, 200, MENOR_QUE_200

NAO_ESTA_NO_INTERVALO:
print     S3
branch    FIM

MENOR_QUE_200:
print     S2

FIM:
end
