package ch.ethz.acl.ngen.mmm;

import java.util.Arrays;

public class JMMM {
    //
    // Baseline implementation of a Matrix-Matrix-Multiplication
    //
    public void baseline (float[] a, float[] b, float[] c, int n){
        for (int i = 0; i < n; i += 1) {
            for (int j = 0; j < n; j += 1) {
                float sum = 0.0f;
                for (int k = 0; k < n; k += 1) {
                    sum += a[i * n + k] * b[k * n + j];
                }
                c[i * n + j] = sum;
            }
        }
    }
    //
    // Blocked version of MMM, reference implementation available at:
    // http://csapp.cs.cmu.edu/2e/waside/waside-blocking.pdf
    //
    public void blocked(float[] a, float[] b, float[] c, int n) {
        int BLOCK_SIZE = 8;
        for (int kk = 0; kk < n; kk += BLOCK_SIZE) {
            for (int jj = 0; jj < n; jj += BLOCK_SIZE) {
                for (int i = 0; i < n; i++) {
                    for (int j = jj; j < jj + BLOCK_SIZE; ++j) {
                        float sum = c[i * n + j];
                        for (int k = kk; k < kk + BLOCK_SIZE; ++k) {
                            sum += a[i * n + k] * b[k * n + j];
                        }
                        c[i * n + j] = sum;
                    }
                }
            }
        }
    }

    public void fast(float[] a, float[] b, float[] c, int n) {
        float[] bBuffer = new float[n];
        float[] cBuffer = new float[n];
        int in = 0;
        for (int i = 0; i < n; ++i) {
            int kn = 0;
            for (int k = 0; k < n; ++k) {
                float aik = a[in + k];
                System.arraycopy(b, kn, bBuffer, 0, n);
                saxpy(n, aik, bBuffer, cBuffer);
                kn += n;
            }
            System.arraycopy(cBuffer, 0, c, in, n);
            Arrays.fill(cBuffer, 0f);
            in += n;
        }
    }

    private void saxpy(int n, float aik, float[] b, float[] c) {
        for (int i = 0; i < n; ++i) {
            c[i] += aik * b[i];
        }
    }

}
