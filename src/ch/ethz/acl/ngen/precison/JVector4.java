package ch.ethz.acl.ngen.precison;

public class JVector4 extends JVector
{
    public byte [] values;
    public float scale;
    //
    // Constructor of the JVector4 class. Note that
    // instead of initializing the values here, we
    // use the QVector4 and copy the data array.
    //
    public JVector4(int size)
    {
        super(4, size);
        if (size % 128 == 0) {
            size_pad = size;
        } else {
            size_pad = size + 128 - (size % 128);
        }
    }
    //
    // A dot product that works with 4-bit vectors. This is
    // a baseline implementation that suffers from huge overhead
    // imposed by JVM up-casting when performing bitwise
    // operations.
    //
    public float dot_baseline (JVector4 other)
    {
        int len = size_pad;
        byte [] u = this.values;
        byte [] v = other.values;

        float acc1 = 0.0f;
        float acc2 = 0.0f;

        int sign_mask_hi_ss = -2147483648;  // 0x80U
        int sign_mask_lo_ss = 8;            // 0x08U
        int sign_mask_ss    = -2147483640;  // 0x88U
        int base_mask_lo_ss = 7;            // 0x07U

        for (int idx = 0; idx < len; idx += 2)
        {
            int i = idx >> 1;

            int qu = (int) u[i];
            int qv = (int) v[i];

            int sign = (qu ^ qv) & sign_mask_ss;

            int sign1 = 1 - ((sign & sign_mask_hi_ss) >>> 30);
            int sign2 = 1 - ((sign & sign_mask_lo_ss) >>>  2);

            int qv1 = (qv >> 4) & base_mask_lo_ss;
            int qu1 = (qu >> 4) & base_mask_lo_ss;
            int qv2 = qu & base_mask_lo_ss;
            int qu2 = qv & base_mask_lo_ss;

            float val1 = (float)( sign1 * (qv1 * qu1) );
            float val2 = (float)( sign2 * (qv2 * qu2) );

            acc1 += val1;
            acc2 += val2;
        }

        return (acc1 + acc2) * this.scale * other.scale;
    }

    //
    // Optimized version of the dot product that tries to minimize the
    // mandatory casting imposed by the JVM
    //
    public float dot (JVector4 other)
    {
        int len = size_pad;
        byte [] u = this.values;
        byte [] v = other.values;

        float acc1 = 0.0f;
        float acc2 = 0.0f;

        int sign_mask_hi_ss = -2147483648;  // 0x80U
        int sign_mask_lo_ss = 8;            // 0x08U
        int sign_mask_ss    = -2147483640;  // 0x88U
        int base_mask_lo_ss = 7;            // 0x07U

        for (int idx = 0; idx < len; idx += 128)
        {
            int sum = 0;

            for (int j = 0; j < 128; j += 2) {

                int i = (idx + j) >> 1;

                int qu = (int) u[i];
                int qv = (int) v[i];

                int sign = (qu ^ qv) & sign_mask_ss;

                int sign1 = 1 - ((sign & sign_mask_hi_ss) >>> 30);
                int sign2 = 1 - ((sign & sign_mask_lo_ss) >>> 2);

                int qv1 = (qv >> 4) & base_mask_lo_ss;
                int qu1 = (qu >> 4) & base_mask_lo_ss;
                int qv2 = qu & base_mask_lo_ss;
                int qu2 = qv & base_mask_lo_ss;

                sum += (sign1 * (qv1 * qu1));
                sum += (sign2 * (qv2 * qu2));
            }

            acc1 += (float) sum;
        }

        return (acc1 + acc2) * this.scale * other.scale;
    }
}

