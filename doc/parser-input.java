import java.util.Scanner;

public class ParserInput
{
    public static void main(String[] args)
    {
        new Scanner(System.in);
        int numero, x;
        System.out.print("Digite um numero: ");
        x = verifica(numero);
        if(x ==1) System.out.println("Numero Positivo");
        else if (x==0) System.out.println("Zero");
        else System.out.println("Numero Negativo");
    }

    public static int verifica(int n){
        int res;
        if(n>0) res =1;
        else if(n<0) res = -1;
        else res = 0;

        return res;
    }
}