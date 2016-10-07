class ReturnTest
{
    void main()
    {
        int i;
        int j;
        i = 2;
        j = i + 2;
        i += fib(j);
        
    }
    
    int fib(int n)
    {
        int j;
        j = n;
        return (n * 2);
    }  
    
}
