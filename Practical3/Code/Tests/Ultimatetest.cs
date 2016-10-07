class UltimateTest
{
    void main()
    {
        int i;
        int g;
        i = 2;
        g = i + 4;
        method();
        i = g + method2(3);
    }
    
    void method()
    {
        print(4, 5, 6);
    }
    
    int method2(int n)
    {
        return n + 3;
    }
}
