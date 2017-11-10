package ch.ethz.acl.ngen.precison;

abstract class JVector {

    protected int bits;
    protected int size;
    protected int size_pad;

    protected float findAbsMax(float [] v) {
        float result = Math.abs(v[0]);
        for (int i = 1; i < size; i += 1) {
            float tmp = Math.abs(v[i]);
            if (tmp > result) {
                result = tmp;
            }
        }
        return result;
    }

    public JVector(int bits, int size) {
        this.bits = bits;
        this.size = size;
    }
}
