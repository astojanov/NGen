package ch.ethz.acl.ngen.saxpy;

public class JSaxpy {
    public void apply(float[] a, float[] b, float s, int n){
        for (int i = 0; i < n; i += 1) {
            a[i] += b[i] * s;
        }
    }
}
