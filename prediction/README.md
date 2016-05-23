# Purpose of this application

This application focuses on obtaining data time series format in order to be able to predict the average time each vehicle detection by each sensor. If high detection times are obtained they can be associated with the presence of traffic jams.

1. Obtain the data time sampled / accumulated every second detection.

A time series is defined as a set of data recorded at equidistant time intervals. So the natural first step of this application is to transform the original dataset for the average detection time sampled every second (from this first transformation to obtain new datasets sampled at different intervals is facilitated, such as 1, 10 , 15, 30, 45 and 60 minutes)

2. Getting predictions one-step-ahead detection time.

In this case it was decided to make predictions of detection times in each sensor in order to associate the high detection times recorded with the presence of traffic jams.

In this case we have made predictions for 10, 15, 30, 45 and 60 minutes. With the initial data and processing on those obtained in the last two months have been obtained runtimes less than 2 minutes.

This step generates an output file for prediction with 3 columns, which contain the time axis in UNIX time, the actual data used in training the model prediction and the set of output data contains the actual data to the penultimate prediction point and the last point (test), respectively.

# Future works

The next step nature of this work is to associate a timeout threshold from which a jam would be detected in said sensor. This threshold would be defined by the data and the characteristics of the road where the sensor is located.


# Additional comments

If you run the experiment on a Linux system, you can take advantage of parallelization caracterísicas implemented in the code by increasing the number of cores in these types of functions:
mclapply (x, function (x) {}, mc.cores = getOption ("mc.cores", 1))
