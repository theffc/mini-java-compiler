# Decide se os numeros sao positivos, zeros ou negativos
.loadlib 'io_ops'

LOOP:
print       "Digite um numero: "
read        S1, 3
set         I1, S1

# Testar se e maior que 0
gt          I1, 0, POSITIVO
eq          I1, 0, ZERO
lt          I1, 0, NEGATIVO

POSITIVO:
print       "Positivo!\n"
branch      FINALIZAR

ZERO:
print       "Zero!\n"
branch      FINALIZAR

NEGATIVO:
print       "Negativo!\n"

# Parte de DESEJA FINALIZAR?
FINALIZAR:
print       "Deseja finalizar? (S/N): "
read        S10, 2
eq          S10, "S\n", FIM
branch      LOOP

FIM:
end
