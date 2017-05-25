# Escrever um numero por extenso
.loadlib 'io_ops'

print       "Digite um numero de 1 a 5: "
read        S1, 2
set         I1, S1

eq          I1, 1, UM
eq          I1, 2, DOIS
eq          I1, 3, TRES
eq          I1, 4, QUATRO
eq          I1, 5, CINCO

print       "Numero invalido!!!"
branch      FIM

CINCO:
print       "Cinco"
branch      FIM

QUATRO:
print       "Quatro"
branch      FIM

TRES:
print       "Tres"
branch      FIM

DOIS:
print       "Dois"
branch      FIM

UM:
print       "Um"

FIM:
print       "\n"
end
