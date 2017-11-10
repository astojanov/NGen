package ch.ethz.acl.ngen.precison;

public class JVector8 extends JVector
{
    public byte [] values;
    public float scale;

    //
    // Constructor of the JVector8 class.
    //
    public JVector8(int size) {
        super(8, size);
    }

    //
    // A simplistic quantization algorithm
    //
    public void quantize (float [] v)
    {
        values = new byte[size];
        float max = findAbsMax(v);
        float s = 127.0f / max;
        for (int i = 0; i < size; i += 1) {
            float rnd = (float) Math.random();
            values[i] = (byte) Math.floor(s * v[i] + rnd);
        }
        scale = max / 127.0f;
    }

    //
    // A dot product that works with 8-bit vectors. This is
    // a baseline implementation that suffers from huge overhead
    // imposed by JVM up-casting when performing integer
    // operations.
    //
    public float dot_baseline(JVector8 other) {
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
    public float dot(JVector8 other) {
        float result = 0;
        int n0 = size;
        int n1 = (size >> 6) << 6;
        for (int i = 0; i < n1; i += 64) {
            int sum = 0;
            for (int j = 0; j < 64; j += 1) {
                int idx = i + j;
                sum += values[idx] * other.values[idx];
            }
            result += sum;
        }
        for (int i = n1; i < n0; i += 1) {
            result += values[i] * other.values[i];
        }
        return result * scale * other.scale;
    }
}

