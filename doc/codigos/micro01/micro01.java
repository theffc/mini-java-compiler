import java.util.Scanner;

public class micro01
{
	public static void main(String[] args)
	{
		Scanner s = new Scanner(System.in);
		float c, f;
		System.out.println("Celsius -> Fahrenheit");
		System.out.print("Digite a temperatura em Celsius: ");
		c = s.nextFloat();
		f = (9 * c + 160) / 5;
		System.out.println("A nova temperatura é:" + f + "F");
	}
}
