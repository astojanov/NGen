package ch.ethz.acl.ngen.precison;

public class JVector32 extends JVector
{
    public float [] values;
    //
    // Constructor
    //
    public JVector32(int size) {
        super(32, size);
    }

    //
    // 32-bit float implementation of a dot product
    //
    public float dot(JVector32 other) {
        float result = 0;
        for (int i = 0; i < size; i += 1) {
            result += values[i] * other.values[i];
        }
        return result;
    }
}
