# Calcula o fatorial de um numero
.loadlib 'io_ops'

print       "Digite um numero: "
read        S1, 2
set         I1, S1
set         I10, S1

branch      FATORIAL
RETURN:
print       "O fatorial de "
print       I1
print       " e: "
print       I10
print       "\n"

end


FATORIAL:
set         I11, I10
dec         I11

TESTE:
eq          I11, 0, RETURN
mul         I10, I10, I11
dec         I11
branch      TESTE
