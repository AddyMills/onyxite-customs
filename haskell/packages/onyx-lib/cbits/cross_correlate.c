#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define SAMPLE_RATE 44100
#define SEARCH_WINDOW_SEC 4.0
#define DOWNSAMPLE_FACTOR 4

// Should be given 2 arrays of ~15s of mono 44100 Hz audio.
// Returns an offset value in seconds to align them
double cross_correlate(float *data1, int length1, float *data2, int length2) {
    int search_samples = (int)(SEARCH_WINDOW_SEC * SAMPLE_RATE / DOWNSAMPLE_FACTOR);
    int min_length = (length1 < length2) ? length1 : length2;
    min_length /= DOWNSAMPLE_FACTOR;

    if (min_length < search_samples) {
        search_samples = min_length / 2;
    }

    double max_correlation = -1.0;
    int best_offset = 0;

    // Use coarse search with downsampling
    for (int offset = -search_samples; offset <= search_samples; offset += 2) {
        double correlation = 0.0;
        double norm1 = 0.0, norm2 = 0.0;
        int count = 0;

        int start1 = (offset > 0) ? offset : 0;
        int start2 = (offset < 0) ? -offset : 0;
        int length = min_length - abs(offset);

        if (length <= 0) continue;

        // Sample every DOWNSAMPLE_FACTOR samples for speed
        for (int i = 0; i < length; i += 1) {
            int idx1 = (start1 + i) * DOWNSAMPLE_FACTOR;
            int idx2 = (start2 + i) * DOWNSAMPLE_FACTOR;

            if (idx1 >= length1 || idx2 >= length2) break;

            float sample1 = data1[idx1];
            float sample2 = data2[idx2];

            correlation += sample1 * sample2;
            norm1 += sample1 * sample1;
            norm2 += sample2 * sample2;
            count++;
        }

        if (count > 100 && norm1 > 0 && norm2 > 0) {
            correlation = correlation / (sqrt(norm1) * sqrt(norm2));

            if (correlation > max_correlation) {
                max_correlation = correlation;
                best_offset = offset;
            }
        }
    }

    return (double)(best_offset * DOWNSAMPLE_FACTOR) / SAMPLE_RATE;
}
