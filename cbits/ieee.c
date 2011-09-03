
double numeric_tools_representable_delta(double x, double h)
{
    /* temp is volatile to force loading from registers to memory. */
    volatile double temp = x + h;
    return temp - x;
}
