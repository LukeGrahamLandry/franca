// TODO: old constant folding could get rid of this entirely but new gvn can't
// from coremark:
int printf(char*, ...);
char
check_data_types()
{
    char retval = 0;
    if (sizeof(char) != 1)
    {
        printf("ERROR: u8\n");
        retval++;
    }
    if (sizeof(short) != 2)
    {
        printf("ERROR: u16\n");
        retval++;
    }
    if (sizeof(short) != 2)
    {
        printf("ERROR: s16\n");
        retval++;
    }
    if (sizeof(int) != 4)
    {
        printf("ERROR: s32\n");
        retval++;
    }
    if (sizeof(int) != 4)
    {
        printf("ERROR: u32");
        retval++;
    }
    if (sizeof(long) != sizeof(int *))
    {
        printf(
            "ERROR: ptr\n");
        retval++;
    }
    if (retval > 0)
    {
        printf("ERROR\n");
    }
    return retval;
}