package ch.ethz.acl.ngen.precison;

public class JVector16 extends JVector
{
    public short [] values;
    public float scale;
    //
    // Constructor
    //
    public JVector16(int size) {
        super(16, size);
    }
    //
    // A simplistic quantization algorhtm
    //
    public void quantize (float [] v)
    {
        values = new short[size];
        float max = findAbsMax(v);
        float s = 32767.0f / max;
        for (int i = 0; i < size; i += 1) {
            float rnd = (float) Math.random();
            values[i] = (short) Math.floor(s * v[i] + rnd);
        }
        scale = max / 32767f;
    }
    //
    // A dot product that works with 16-bit vectors. This is
    // a baseline implementation that suffers from huge overhead
    // imposed by JVM up-casting when performing integer
    // operations.
    //
    public float dot_baseline(JVector16 other) {
        float result = 0;
        for (int i = 0; i < size; i += 1) {
            result += values[i] * other.values[i];
        }
        return result * scale * other.scale;
    }

    //
    // An optimized version of the dot-product that tries to
    // minimize the number of casts performed.
    //
    public float dot(JVector16 other) {
        float result = 0;
        int n0 = size;
        int n1 = (size >> 6) << 6;
        for (int i = 0; i < n1; i += 64) {
            long sum = 0;
            for (int j = 0; j < 64; j += 1) {
                int idx = i + j;
                long mul = (long)(values[idx] * other.values[idx]);
                sum = sum + mul;
            }
            result += sum;
        }
        for (int i = n1; i < n0; i += 1) {
            result += values[i] * other.values[i];
        }
        return result * scale * other.scale;
    }

    public void print () {
        for (int i = 0; i < size; i += 1) {
            System.out.println(values[i] * scale);
        }
    }
}

