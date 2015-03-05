#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int compare (const void * a, const void * b)
{
      return ( *(int*)a - *(int*)b );
}

int main(int argc, char* argv[])
{
    int N = atoi(argv[1]), L = atoi(argv[2]);
    int pos[N];
    int speed;
    srand(time(NULL));
    printf("%d %d\n", N, L);
    for(int i = 0; i < N; i++)
    {
        pos[i] = rand() % L;
    }
    qsort(pos, N, sizeof(int), compare);
    for(int i = 0; i < N; i++)
    {
        speed = (rand() % 499) + 1;
        if(i>0 && pos[i]==pos[i-1]) pos[i]++;
        printf("%d %.2lf\n", pos[i], speed/100.0);
    }
    return 0;
}
