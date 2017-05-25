# Decide se um numero e positivo, zero ou negativo com auxilio de uma subrotina
.loadlib 'io_ops'

print       "Digite um numero: "
read        S1, 3
set         I1, S1

set         I2, 0                 # variavel que tera o resultado
branch      VERIFICA
RETORNO:

eq          I2, 1,  POSITIVO
eq          I2, 0,  ZERO
print       "Negativo\n"
branch      FIM

ZERO:
print       "Zero\n"
branch      FIM

POSITIVO:
print       "Positivo\n"

FIM:
end

VERIFICA:
gt          I1, 0, MAIOR
lt          I1, 0, MENOR
branch      FIM_SUB

MENOR:
set         I2, -1
branch      FIM_SUB

MAIOR:
set         I2, 1

FIM_SUB:
branch      RETORNO
