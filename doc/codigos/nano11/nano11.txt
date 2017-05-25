# Introdução do comando de repetição enquanto

set     I1, 1 # n
set     I2, 2 # m
set     I3, 5 # x

TESTE:
gt      I3, I1, LOOP # gt = greater then
branch  FIM

LOOP:
add     I1, I1, I2
print   I1
print   "\n"
branch  TESTE

FIM:
end
