# Calculo de precos
.loadlib 'io_ops'


print         "Digite o preco (max. 2 digitos): "
read          S1, 3
set           N1, S1
print         "Digite a venda (max. 4 digitos): "
read          S1, 5
set           N2, S1

lt            N2, 500, AUMENTAR_10_PORCENTO
ge            N1, 30, FALSO1

AUMENTAR_10_PORCENTO:
mul           N3, 10, N1
div           N3, N3, 100
add           N3, N3, N1
branch        FIM

FALSO1:
lt            N2, 500, SEGUNDO_TESTE
lt            N2, 1200, AUMENTAR_15_PORCENTO
SEGUNDO_TESTE:
lt            N1, 30, FALSO2
ge            N1, 80, FALSO2

AUMENTAR_15_PORCENTO:
mul           N3, N1, 15
div           N3, N3, 100
add           N3, N3, N1
branch        FIM

FALSO2:
ge            N2, 1200, DIMINUIR_20_PORCENTO
lt            N1, 80, FIM

DIMINUIR_20_PORCENTO:
mul           N3, 20, N1
div           N3, N3, 100
sub           N3, N1, N3

FIM:
print         "O novo preco e: "
print         N3
print         "\n"
end
