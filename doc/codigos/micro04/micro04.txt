# Le numeros e informa quais estao entre 10 e 150
.loadlib 'io_ops'

set       S1, "Digite um numero: "
set       S2, "Ao total foram digitados "
set       S3, " numeros no intervalo entre 10 e 150."

set       I1, 1                                           # x
set       I2, 0                                           # intervalo

LOOP_TESTE:
le        I1, 5, INICIO_LOOP
branch    FIM

INICIO_LOOP:
print     S1
read      S10, 3
set       I10, S10

ge        I10, 10, MAIOR_QUE_10
branch    FIM_LOOP

MAIOR_QUE_10:
le        I10, 150, MENOR_QUE_150
branch    FIM_LOOP

MENOR_QUE_150:
inc       I2

FIM_LOOP:
inc       I1
branch LOOP_TESTE


FIM:
print     S2
print     I2
print     S3
print     "\n"
end
