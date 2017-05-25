# Le strings e caracteres
.loadlib 'io_ops'

set       S2, "H - Homem ou M - Mulher: "
set       S3, "Sexo so pode ser H ou M!\n"
set       S4, "Foram inseridos "
set       S5, "Foram inseridas "
set       S6, " homens"
set       S7, " mulheres"

set       I1, 1                             # x
set       I2, 0                             # homens
set       I3, 0                             # mulheres

LOOP_TESTE:
le        I1, 5, INICIO_LOOP
branch    FIM

INICIO_LOOP:
print     S2
read      S11, 2

eq        S11, "H\n", HOMEM
eq        S11, "M\n", MULHER

print     S3
branch    FIM_LOOP

HOMEM:
inc       I2
branch    FIM_LOOP

MULHER:
inc       I3

FIM_LOOP:
inc       I1
branch    LOOP_TESTE

FIM:
print     S4
print     I2
print     S6
print     "\n"

print     S5
print     I3
print     S7
print     "\n"
end
