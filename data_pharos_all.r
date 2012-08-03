m <- NULL 
m <- matrix(c(
"randread",  1,  "8K",   1.368,    5.703, 0.1,      398,   14.6,  175 , 0, 2, 6,38, 7, 0, 3,20,19, 2, 0, 0, 0, 0, 0, 0,18.304,25.984,33.536,378.880,399.360,399.360
,"randread",  8,  "8K",  65.810,    0.935, 0.1,      202,    3.3, 8423 , 0, 2,17,42,33, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0,0.612,15.296,20.096,35.584,46.336,85.504
,"randread", 16,  "8K", 143.724,    0.862, 0.1,      469,    4.3,18396 , 0, 0,40,22,33, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0,0.676,15.296,20.608,37.632,48.896,177.152
,"randread", 32,  "8K", 395.979,    0.620, 0.1,      519,    3.6,50685 , 0, 0,47,49, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.434,12.736,19.584,39.168,51.456,120.320
,"randread", 64,  "8K", 158.474,    3.073, 0.1,      605,    6.7,20284 , 0, 0, 1, 3,12,38,35, 3, 3, 1, 0, 0, 0, 0, 0, 0,11.200,29.056,38.656,77.312,108.032,183.296
,    "read",  1,  "8K",   1.603,    4.869, 0.2,     3480,   97.2,  205 , 0, 0, 4,22,67, 2, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0,1.608,9.536,63.232,626.688,2736.128,3489.792
,    "read",  1, "32K",  30.844,    1.009, 0.2,      106,    3.1,  987 , 0, 0, 1,25,67, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0,1.416,12.224,19.328,45.824,61.696,105.984
,    "read",  1,"128K",  17.919,    6.969, 0.5,      119,    9.0,  143 , 0, 0, 0, 0,29,16, 5,23,17, 6, 0, 0, 0, 0, 0, 0,23.680,36.608,46.848,102.912,119.296,119.296
,    "read",  1,  "1M",  41.191,   24.270, 3.0,      216,   24.6,   41 , 0, 0, 0, 0, 0, 0,45, 0, 3,38,10, 0, 0, 0, 0, 0,58.000,96.000,118.000,217.000,217.000,217.000
,    "read",  8,  "1M", 127.852,   38.970, 3.0,      276,   36.3,  127 , 0, 0, 0, 0, 0, 0,26,10, 0,28,28, 5, 0, 0, 0, 0,103.000,163.000,190.000,239.000,277.000,277.000
,    "read", 16,  "1M", 185.016,   48.080, 3.0,      466,   42.1,  185 , 0, 0, 0, 0, 0, 0, 4,12,19,21,32, 9, 0, 0, 0, 0,125.000,172.000,204.000,371.000,465.000,465.000
,    "read", 32,  "1M", 272.547,   64.305, 0.3,     1531,   62.4,  272 , 0, 0, 0, 1, 3, 0, 1, 4,12,23,34,17, 0, 0, 0, 0,164.864,250.880,313.344,544.768,823.296,1531.904
,    "read", 64,  "1M", 471.111,   71.825, 0.3,     1540,   69.6,  471 , 0, 0, 0, 2, 3, 1, 2, 6, 7,17,31,25, 1, 0, 0, 0,179.200,301.056,366.592,544.768,643.072,1548.288
,   "write",  1,  "1K",   1.928,    0.503, 0.1,      141,    1.3, 1974 , 0, 0, 8,26,64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.604,0.700,0.772,1.560,4.896,100.864
,   "write",  1,  "8K",  15.191,    0.511, 0.2,        5,    0.1, 1944 , 0, 0, 5,29,65, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.628,0.700,0.772,1.240,1.768,4.640
,   "write",  1,"128K", 126.100,    0.987, 0.6,       17,    0.3, 1008 , 0, 0, 0, 0,87,12, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,1.064,1.304,2.352,3.632,4.576,9.664
,   "write",  4,  "1K",   6.828,    0.425, 0.1,      406,    2.6, 6992 , 0, 0,50,27,20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.660,1.004,6.688,14.912,23.680,64.256
,   "write",  4,  "8K",  52.673,    0.441, 0.2,        8,    0.2, 6742 , 0, 0, 4,59,36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.692,0.772,0.836,1.736,2.512,4.320
,   "write",  4,"128K", 213.272,    1.750, 0.7,     1748,   21.2, 1706 , 0, 0, 0, 0, 7,90, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,1.784,2.672,4.016,6.048,8.256,1744.896
,   "write", 16,  "1K",  21.328,    0.407, 0.1,      650,    3.4,21840 , 0, 0,14,81, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.470,0.804,3.344,14.400,22.400,60.160
,   "write", 16,  "8K", 182.295,    0.381, 0.2,       16,    0.2,23333 , 0, 0, 1,95, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.462,1.048,1.912,2.992,3.440,7.776
,   "write", 16,"128K", 277.128,    4.038, 0.7,     1453,   16.6, 2217 , 0, 0, 0, 0, 0, 0,93, 2, 1, 1, 0, 0, 0, 0, 0, 0,5.000,31.000,50.000,100.000,241.000,586.000
),nrow=31)
tm <- t(m)
m <-tm
colnames <- c("name","users","bs","MB","lat","min","max","std","iops"
, "us50","us100","us250","us500","ms1","ms2","ms4","ms10","ms20"
, "ms50","ms100","ms250","ms500","s1","s2","s5"
,"p95_00", "p99_00", "p99_50", "p99_90", "p99_95", "p99_99"
)
colnames(m)=colnames
m <- data.frame(m)
testtype <- "pharos_run10_10"
source("fiogp.r")
m <- NULL 
m <- matrix(c(
"randread",  1,  "8K",  20.211,    0.383, 0.1,       13,    0.2, 2587 , 0, 5,14,72, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.506,0.540,0.556,0.748,1.288,8.384
,"randread",  8,  "8K", 295.823,    0.207, 0.1,        2,    0.0,37865 , 0, 0,97, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.241,0.278,0.314,0.478,0.564,1.112
,"randread", 16,  "8K", 442.392,    0.277, 0.1,        8,    0.1,56626 , 0, 0,23,75, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.378,0.498,0.564,1.240,1.960,3.216
,"randread", 32,  "8K", 525.247,    0.453, 0.1,      443,    2.4,67231 , 0, 0,26,61, 9, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.844,2.640,4.640,17.536,34.048,101.888
,"randread", 64,  "8K", 551.628,    0.808, 0.1,     1380,   13.8,70608 , 0, 0,33,56, 6, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.828,3.536,7.968,85.504,280.576,700.416
,    "read",  1,  "8K",   1.390,    5.616, 0.1,     2430,   85.5,  177 , 0, 0,18,21,53, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,1.752,52.480,158.720,2375.680,2441.216,2441.216
,    "read",  1, "32K",  24.326,    1.281, 0.2,      110,    3.9,  778 , 0, 0, 1,24,65, 1, 1, 3, 1, 0, 0, 0, 0, 0, 0, 0,5.344,17.024,24.960,44.800,63.744,111.104
,    "read",  1,"128K",  13.743,    9.088, 0.5,      172,   11.7,  109 , 0, 0, 0, 0,18,22, 5,21,19,12, 1, 0, 0, 0, 0, 0,29.568,50.432,57.600,80.384,173.056,173.056
,    "read",  1,  "1M",  34.383,   29.070, 3.0,      450,   43.2,   34 , 0, 0, 0, 0, 0, 0,43, 4, 1,34,13, 2, 0, 0, 0, 0,74.000,243.000,302.000,453.000,453.000,453.000
,    "read",  8,  "1M",  92.234,   53.930, 3.0,      448,   41.1,   92 , 0, 0, 0, 0, 0, 0, 7, 4, 2,38,40, 6, 0, 0, 0, 0,110.000,215.000,297.000,449.000,449.000,449.000
,    "read", 16,  "1M", 242.373,   36.871, 0.2,      637,   51.5,  242 , 0, 0, 0, 8,16, 0, 6, 8,16,15,18, 8, 0, 0, 0, 0,129.536,246.784,296.960,395.264,399.360,634.880
,    "read", 32,  "1M", 331.058,   53.103, 0.3,      718,   78.5,  331 , 0, 0, 0, 2,24,10, 0, 3, 6,13,22,13, 2, 0, 0, 0,185.344,423.936,464.896,593.920,659.456,716.800
,    "read", 64,  "1M", 592.351,   52.773, 0.2,     3116,  115.4,  592 , 0, 0, 0, 1,10,14, 3, 7, 7,13,28,12, 1, 0, 0, 0,156.672,325.632,419.840,1581.056,2801.664,3129.344
,   "write",  1,  "1K",   1.970,    0.492, 0.2,       51,    0.4, 2017 , 0, 0, 8,27,64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.604,0.700,0.804,3.088,5.024,5.344
,   "write",  1,  "8K",  14.199,    0.546, 0.2,       13,    0.4, 1817 , 0, 0, 5,30,63, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.636,0.988,4.896,5.472,5.664,8.384
,   "write",  1,"128K",  91.656,    1.359, 0.6,       13,    1.3,  733 , 0, 0, 0, 0,82, 8, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0,5.280,5.664,5.856,11.072,11.584,13.888
,   "write",  4,  "1K",   8.497,    0.341, 0.1,       35,    0.4, 8701 , 0, 0,60,18,20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.652,0.804,1.080,7.264,9.152,11.328
,   "write",  4,  "8K",  45.276,    0.514, 0.2,       20,    0.8, 5795 , 0, 0, 6,57,35, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,0.692,4.896,7.776,10.560,11.200,16.512
,   "write",  4,"128K", 134.719,    2.776, 0.6,      924,   15.1, 1077 , 0, 0, 0, 0,25,58, 0,10, 5, 0, 0, 0, 0, 0, 0, 0,10.304,16.320,18.048,27.264,36.096,864.256
,   "write", 16,  "1K",  25.779,    0.336, 0.2,       64,    0.7,26398 , 0, 0,20,76, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.398,0.836,5.024,10.176,10.816,16.512
,   "write", 16,  "8K",  99.661,    0.700, 0.2,      704,    5.4,12756 , 0, 0, 1,87, 6, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0,0.756,9.792,10.816,17.024,18.560,33.024
,   "write", 16,"128K", 177.305,    6.309, 0.6,      401,   16.0, 1418 , 0, 0, 0, 0, 5,11,61, 8, 7, 3, 0, 0, 0, 0, 0, 0,19.840,70.144,118.272,189.440,272.384,387.072
),nrow=31)
tm <- t(m)
m <-tm
colnames <- c("name","users","bs","MB","lat","min","max","std","iops"
, "us50","us100","us250","us500","ms1","ms2","ms4","ms10","ms20"
, "ms50","ms100","ms250","ms500","s1","s2","s5"
,"p95_00", "p99_00", "p99_50", "p99_90", "p99_95", "p99_99"
)
colnames(m)=colnames
m <- data.frame(m)
testtype <- "pharos_run1_10"
source("fiogp.r")
m <- NULL 
m <- matrix(c(
"randread",  1,  "8K",   1.699,    4.591, 0.1,      295,   11.7,  217 , 0, 2, 9,46, 9, 0, 1,13,13, 3, 0, 0, 0, 0, 0, 0,17.792,33.024,43.264,207.872,252.928,296.960
,"randread",  8,  "8K",  68.793,    0.902, 0.1,      502,    5.3, 8805 , 0, 1,16,36,42, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0,0.596,14.400,19.584,38.656,52.480,264.192
,"randread", 16,  "8K", 214.574,    0.575, 0.1,      398,    3.4,27465 , 0, 0,68,15,13, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,0.604,10.432,15.680,30.848,42.240,111.104
,"randread", 32,  "8K", 428.291,    0.574, 0.1,      813,    4.1,54821 , 0, 0,28,66, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.478,9.920,15.680,35.072,49.408,127.488
,"randread", 64,  "8K", 283.900,    1.724, 0.1,      617,    5.9,36339 , 0, 0, 5,21,28,29,10, 1, 1, 0, 0, 0, 0, 0, 0, 0,3.600,19.072,27.008,57.088,86.528,240.640
,    "read",  1,  "8K",  15.179,    0.511, 0.1,        4,    0.2, 1942 , 0, 0,10,26,61, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.644,1.800,2.096,2.320,2.384,3.568
,    "read",  1, "32K",  19.676,    1.584, 0.2,      189,    6.7,  629 , 0, 0, 0,23,67, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,3.120,28.544,44.800,93.696,110.080,189.440
,    "read",  1,"128K",  21.672,    5.762, 0.5,      272,   13.1,  173 , 0, 0, 0, 0,50,17, 5, 9, 9, 5, 1, 0, 0, 0, 0, 0,25.728,60.160,81.408,130.560,272.384,272.384
,    "read",  1,  "1M",  38.642,   25.870, 3.0,      207,   26.9,   38 , 0, 0, 0, 0, 0, 0,33,14, 2,32,16, 1, 0, 0, 0, 0,72.000,105.000,172.000,208.000,208.000,208.000
,    "read",  8,  "1M",  93.039,   53.580, 3.0,      366,   32.5,   93 , 0, 0, 0, 0, 0, 0, 2, 2, 4,43,40, 7, 0, 0, 0, 0,108.000,157.000,206.000,367.000,367.000,367.000
,    "read", 16,  "1M", 249.527,   35.717, 0.2,      514,   49.7,  249 , 0, 0, 0, 5,14, 0, 9,19, 4,19,19, 7, 0, 0, 0, 0,128.512,236.544,276.480,411.648,448.512,514.048
,    "read", 32,  "1M", 453.161,   38.685, 0.2,     1019,   62.8,  453 , 0, 0, 0, 8,34, 3, 1, 2, 7,13,17,10, 1, 0, 0, 0,146.432,276.480,362.496,602.112,659.456,1019.904
,    "read", 64,  "1M", 609.111,   55.359, 0.2,     2135,   82.8,  609 , 0, 0, 0, 2, 6,13, 3, 8, 7,17,25,12, 2, 0, 0, 0,185.344,374.784,505.856,749.568,823.296,2146.304
,   "write",  1,  "1K",   1.947,    0.498, 0.1,       81,    0.7, 1994 , 0, 0, 9,28,61, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.620,0.764,1.144,1.800,3.248,36.096
,   "write",  1,  "8K",  14.977,    0.518, 0.2,        7,    0.1, 1917 , 0, 0, 4,28,66, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.644,0.788,0.868,1.144,1.384,2.160
,   "write",  1,"128K", 119.575,    1.041, 0.6,       15,    0.5,  956 , 0, 0, 0, 0,72,25, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,1.128,5.024,5.280,5.792,6.624,15.936
,   "write",  4,  "1K",   5.521,    0.526, 0.1,      615,    3.9, 5654 , 0, 0,37,35,23, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.796,3.920,7.136,22.656,31.616,66.048
,   "write",  4,  "8K",  50.791,    0.458, 0.2,        5,    0.2, 6501 , 0, 0, 4,55,40, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.700,0.820,0.932,1.448,1.848,3.408
,   "write",  4,"128K", 188.605,    1.983, 0.6,     1168,   16.2, 1508 , 0, 0, 0, 0,12,81, 1, 3, 1, 0, 0, 0, 0, 0, 0, 0,3.088,10.176,11.200,13.248,18.816,1138.688
,   "write", 16,  "1K",  14.435,    0.603, 0.2,      651,    4.3,14781 , 0, 0,14,73,10, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.700,6.048,12.608,32.384,46.336,119.296
,   "write", 16,  "8K", 184.774,    0.376, 0.2,      829,    2.2,23651 , 0, 0, 1,95, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.470,0.660,0.812,1.496,2.448,7.648
,   "write", 16,"128K", 149.646,    7.486, 0.7,     4715,  129.0, 1197 , 0, 0, 0, 0, 0, 0,90, 4, 2, 1, 0, 0, 0, 0, 0, 0,9.920,29.824,40.192,110.080,4685.824,4685.824
),nrow=31)
tm <- t(m)
m <-tm
colnames <- c("name","users","bs","MB","lat","min","max","std","iops"
, "us50","us100","us250","us500","ms1","ms2","ms4","ms10","ms20"
, "ms50","ms100","ms250","ms500","s1","s2","s5"
,"p95_00", "p99_00", "p99_50", "p99_90", "p99_95", "p99_99"
)
colnames(m)=colnames
m <- data.frame(m)
testtype <- "pharos_run2_10"
source("fiogp.r")
m <- NULL 
m <- matrix(c(
"randread",  1,  "8K",   1.905,    4.095, 0.1,      187,    8.0,  243 , 0, 2, 7,52, 1, 0, 2,16,14, 1, 0, 0, 0, 0, 0, 0,16.192,21.120,28.544,112.128,158.720,187.392
,"randread",  8,  "8K",  68.678,    0.903, 0.1,      424,    3.8, 8790 , 0, 1,17,37,39, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0,0.620,14.656,19.584,35.584,46.336,118.272
,"randread", 16,  "8K", 164.563,    0.750, 0.1,      602,    4.8,21064 , 0, 0,51,19,25, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0,0.644,13.504,19.328,36.096,48.896,185.344
,"randread", 32,  "8K", 406.174,    0.593, 0.1,      496,    3.8,51990 , 0, 0,39,57, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.438,10.816,18.560,38.656,50.944,134.144
,"randread", 64,  "8K", 208.590,    2.349, 0.1,      673,    7.1,26699 , 0, 0, 2,11,23,35,20, 2, 2, 1, 0, 0, 0, 0, 0, 0,6.368,25.216,35.584,75.264,104.960,284.672
,    "read",  1,  "8K",   6.231,    1.250, 0.1,     2276,   26.8,  797 , 0, 0, 7,27,61, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,0.732,8.384,18.048,67.072,358.400,2277.376
,    "read",  1, "32K",  21.854,    1.426, 0.2,      131,    4.4,  699 , 0, 0, 1,25,63, 1, 1, 3, 1, 1, 0, 0, 0, 0, 0, 0,6.048,21.632,30.592,57.088,64.768,132.096
,    "read",  1,"128K",  30.206,    4.133, 0.5,      265,   10.0,  241 , 0, 0, 0, 0,51,23, 2, 9, 8, 3, 0, 0, 0, 0, 0, 0,17.792,34.048,46.336,92.672,205.824,264.192
,    "read",  1,  "1M",  36.391,   27.470, 3.0,      124,   20.4,   36 , 0, 0, 0, 0, 0, 0,23, 4, 6,55, 9, 0, 0, 0, 0, 0,59.000,92.000,112.000,125.000,125.000,125.000
,    "read",  8,  "1M", 122.428,   40.620, 3.0,      201,   29.9,  122 , 0, 0, 0, 0, 0, 0,14, 7, 3,39,31, 3, 0, 0, 0, 0,90.000,145.000,163.000,200.000,202.000,202.000
,    "read", 16,  "1M", 274.126,   32.475, 0.2,      260,   32.5,  274 , 0, 0, 0, 5, 9, 0, 1, 9,24,24,20, 4, 0, 0, 0, 0,96.768,138.240,152.576,240.640,242.688,261.120
,    "read", 32,  "1M", 436.286,   40.210, 0.2,      379,   41.0,  436 , 0, 0, 0, 6,14, 0, 3, 4, 8,28,25, 7, 0, 0, 0, 0,116.224,179.200,193.536,257.024,292.864,378.880
,    "read", 64,  "1M", 519.964,   61.741, 0.2,      733,   66.6,  519 , 0, 0, 0, 5,10, 0, 2, 6, 8,18,26,19, 1, 0, 0, 0,181.248,305.152,366.592,497.664,544.768,733.184
,   "write",  1,  "1K",   1.899,    0.510, 0.1,      179,    1.6, 1945 , 0, 0, 9,29,60, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.612,0.732,0.796,3.920,9.920,96.768
,   "write",  1,  "8K",  14.707,    0.527, 0.2,        8,    0.2, 1882 , 0, 0, 4,29,66, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.660,0.756,0.868,1.800,2.992,5.152
,   "write",  1,"128K", 123.812,    1.005, 0.6,       21,    0.3,  990 , 0, 0, 0, 0,72,26, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,1.080,1.288,1.608,6.112,7.008,21.888
,   "write",  4,  "1K",   6.559,    0.441, 0.1,      478,    3.0, 6716 , 0, 0,52,25,21, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.676,1.464,6.688,19.072,25.728,126.464
,   "write",  4,  "8K",  51.292,    0.453, 0.2,       17,    0.3, 6565 , 0, 0, 3,58,36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.708,0.836,1.020,2.992,5.152,8.896
,   "write",  4,"128K", 219.896,    1.699, 0.6,      713,    8.8, 1759 , 0, 0, 0, 0, 7,89, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,1.784,6.176,8.640,13.120,15.808,659.456
,   "write", 16,  "1K",  17.178,    0.507, 0.2,      684,    4.1,17590 , 0, 0,10,82, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.604,3.728,6.752,22.400,30.592,100.864
,   "write", 16,  "8K", 186.672,    0.372, 0.2,       57,    0.3,23894 , 0, 0, 0,95, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.458,0.660,0.788,4.768,6.048,10.560
,   "write", 16,"128K", 266.105,    4.208, 0.6,      193,    6.6, 2128 , 0, 0, 0, 0, 3, 7,75, 6, 5, 1, 0, 0, 0, 0, 0, 0,12.352,27.520,36.096,90.624,142.336,162.816
),nrow=31)
tm <- t(m)
m <-tm
colnames <- c("name","users","bs","MB","lat","min","max","std","iops"
, "us50","us100","us250","us500","ms1","ms2","ms4","ms10","ms20"
, "ms50","ms100","ms250","ms500","s1","s2","s5"
,"p95_00", "p99_00", "p99_50", "p99_90", "p99_95", "p99_99"
)
colnames(m)=colnames
m <- data.frame(m)
testtype <- "pharos_run3_10"
source("fiogp.r")
m <- NULL 
m <- matrix(c(
"randread",  1,  "8K",   1.469,    5.312, 0.1,      657,   18.1,  188 , 0, 3, 8,45, 1, 0, 3,18,15, 3, 0, 0, 0, 0, 0, 0,18.304,30.336,38.144,321.536,659.456,659.456
,"randread",  8,  "8K",  67.887,    0.914, 0.1,      409,    4.0, 8689 , 0, 1,17,37,38, 0, 0, 2, 1, 0, 0, 0, 0, 0, 0, 0,0.668,13.888,19.072,36.608,45.312,138.240
,"randread", 16,  "8K", 165.156,    0.749, 0.1,      348,    3.4,21140 , 0, 0,50,20,25, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0,0.676,11.840,17.280,35.072,46.336,102.912
,"randread", 32,  "8K", 411.685,    0.594, 0.1,      494,    3.3,52695 , 0, 0,40,55, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,0.458,10.688,17.024,35.584,48.384,114.176
,"randread", 64,  "8K", 205.561,    2.341, 0.1,      524,    5.7,26311 , 0, 0, 1, 8,23,40,18, 3, 2, 1, 0, 0, 0, 0, 0, 0,8.256,23.168,30.848,57.088,81.408,187.392
,    "read",  1,  "8K",   2.937,    2.655, 0.1,     2386,   52.7,  375 , 0, 0, 6,27,60, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,0.884,22.912,49.408,374.784,1990.656,2375.680
,    "read",  1, "32K",  15.261,    2.043, 0.2,      224,    6.4,  488 , 0, 0, 1,18,63, 2, 2, 7, 2, 1, 0, 0, 0, 0, 0, 0,8.896,25.216,34.048,66.048,86.528,224.256
,    "read",  1,"128K",  39.305,    3.175, 0.5,      114,    6.7,  314 , 0, 0, 0, 0,71, 9, 1, 8, 6, 2, 0, 0, 0, 0, 0, 0,15.296,33.536,41.216,63.232,88.576,114.176
,    "read",  1,  "1M",  25.848,   38.670, 3.0,      171,   21.3,   25 , 0, 0, 0, 0, 0, 0, 8, 1, 3,66,19, 1, 0, 0, 0, 0,67.000,121.000,161.000,172.000,172.000,172.000
,    "read",  8,  "1M", 120.235,   41.450, 3.0,      213,   26.7,  120 , 0, 0, 0, 0, 0, 0, 4, 4, 9,50,27, 3, 0, 0, 0, 0,90.000,129.000,155.000,182.000,215.000,215.000
,    "read", 16,  "1M", 226.690,   39.366, 0.3,      297,   39.4,  226 , 0, 0, 0, 2, 5, 0, 2, 7,30,20,24, 7, 0, 0, 0, 0,112.128,179.200,216.064,261.120,276.480,296.960
,    "read", 32,  "1M", 406.063,   42.852, 0.2,      452,   46.8,  406 , 0, 0, 0, 6,13, 0, 2, 7, 9,24,25, 9, 0, 0, 0, 0,129.536,220.160,259.072,325.632,337.920,452.608
,    "read", 64,  "1M", 521.026,   59.233, 0.2,      595,   56.5,  521 , 0, 0, 0, 2, 9, 1, 1, 5, 9,22,29,17, 1, 0, 0, 0,158.720,257.024,317.440,444.416,544.768,593.920
,   "write",  1,  "1K",   1.972,    0.491, 0.1,       17,    0.2, 2019 , 0, 0, 8,28,63, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.612,0.716,0.804,1.176,1.416,5.024
,   "write",  1,  "8K",  14.877,    0.521, 0.2,        2,    0.1, 1904 , 0, 0, 5,28,66, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.644,0.716,0.796,1.020,1.160,1.864
,   "write",  1,"128K", 121.675,    1.023, 0.6,       28,    0.4,  973 , 0, 0, 0, 0,64,34, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,1.144,1.368,2.384,3.504,4.704,29.056
,   "write",  4,  "1K",   7.305,    0.397, 0.1,      363,    2.1, 7480 , 0, 0,55,25,18, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.644,0.860,5.856,14.272,23.680,55.552
,   "write",  4,  "8K",  51.876,    0.448, 0.2,        9,    0.2, 6640 , 0, 0, 3,59,36, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.700,0.804,0.940,2.384,2.864,5.216
,   "write",  4,"128K", 236.816,    1.578, 0.6,      218,    2.8, 1894 , 0, 0, 0, 0, 6,90, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,1.800,4.128,6.112,11.328,12.224,218.112
,   "write", 16,  "1K",  17.938,    0.485, 0.1,      277,    1.9,18368 , 0, 0,10,82, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.604,5.024,6.752,19.584,24.192,68.096
,   "write", 16,  "8K", 187.616,    0.371, 0.2,       20,    0.2,24014 , 0, 0, 0,96, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.450,0.684,1.004,2.640,3.824,9.024
,   "write", 16,"128K", 298.900,    3.740, 0.6,      431,    8.9, 2391 , 0, 0, 0, 0, 0, 2,89, 3, 2, 1, 0, 0, 0, 0, 0, 0,6.688,21.120,30.080,89.600,129.536,415.744
),nrow=31)
tm <- t(m)
m <-tm
colnames <- c("name","users","bs","MB","lat","min","max","std","iops"
, "us50","us100","us250","us500","ms1","ms2","ms4","ms10","ms20"
, "ms50","ms100","ms250","ms500","s1","s2","s5"
,"p95_00", "p99_00", "p99_50", "p99_90", "p99_95", "p99_99"
)
colnames(m)=colnames
m <- data.frame(m)
testtype <- "pharos_run4_10"
source("fiogp.r")
m <- NULL 
m <- matrix(c(
"randread",  1,  "8K",   2.050,    3.805, 0.1,      156,    7.2,  262 , 0, 2, 8,43,12, 0, 3,16,12, 1, 0, 0, 0, 0, 0, 0,15.808,23.168,34.048,108.032,124.416,156.672
,"randread",  8,  "8K",  72.986,    0.850, 0.1,      300,    3.0, 9342 , 0, 1,19,34,39, 0, 0, 2, 1, 0, 0, 0, 0, 0, 0, 0,1.688,10.944,14.784,27.264,37.632,116.224
,"randread", 16,  "8K", 186.437,    0.663, 0.1,      215,    2.4,23863 , 0, 0,57,19,18, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0,0.684,10.432,13.760,25.728,32.640,77.312
,"randread", 32,  "8K", 407.595,    0.595, 0.1,      276,    2.5,52172 , 0, 0,36,58, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,0.494,10.560,15.296,29.312,36.608,77.312
,"randread", 64,  "8K", 227.209,    2.160, 0.1,      262,    4.7,29082 , 0, 0, 1, 8,26,40,15, 3, 2, 1, 0, 0, 0, 0, 0, 0,7.520,21.120,28.800,56.576,76.288,136.192
,    "read",  1,  "8K",   0.733,   10.641, 0.2,     3175,  137.8,   93 , 0, 0, 7,28,58, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,1.368,201.728,407.552,3162.112,3162.112,3162.112
,    "read",  1, "32K",   8.763,    3.561, 0.2,      168,   11.5,  280 , 0, 0, 0,18,59, 3, 2, 7, 2, 2, 1, 0, 0, 0, 0, 0,16.768,63.232,76.288,138.240,166.912,168.960
,    "read",  1,"128K",  16.788,    7.439, 0.5,      177,   15.5,  134 , 0, 0, 0, 0,48,19, 3, 7, 9, 8, 2, 0, 0, 0, 0, 0,35.584,75.264,104.960,124.416,177.152,177.152
,    "read",  1,  "1M",  45.391,   22.020, 3.0,      118,   25.4,   45 , 0, 0, 0, 0, 0, 0,53, 5, 1,23,15, 1, 0, 0, 0, 0,71.000,101.000,112.000,120.000,120.000,120.000
,    "read",  8,  "1M", 113.433,   43.900, 3.0,      227,   34.4,  113 , 0, 0, 0, 0, 0, 0,20, 7, 1,27,37, 5, 0, 0, 0, 0,102.000,145.000,159.000,227.000,227.000,227.000
,    "read", 16,  "1M", 192.812,   46.190, 3.0,      303,   35.6,  192 , 0, 0, 0, 0, 0, 0, 0, 4,27,28,30, 7, 0, 0, 0, 0,114.000,163.000,190.000,269.000,306.000,306.000
,    "read", 32,  "1M", 364.012,   48.165, 0.2,      614,   50.1,  364 , 0, 0, 0, 7,13, 0, 3, 4, 9,17,29,13, 0, 0, 0, 0,136.192,191.488,224.256,387.072,593.920,618.496
,    "read", 64,  "1M", 501.237,   62.426, 0.3,      381,   55.0,  501 , 0, 0, 0, 3, 7, 3, 3, 4, 5,23,24,23, 0, 0, 0, 0,162.816,228.352,268.288,309.248,313.344,382.976
,   "write",  1,  "1K",   1.945,    0.498, 0.1,      174,    1.3, 1992 , 0, 0, 8,27,63, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.604,0.756,0.820,1.160,1.736,19.840
,   "write",  1,  "8K",  14.696,    0.528, 0.2,       18,    0.2, 1881 , 0, 0, 5,27,66, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.652,0.860,1.080,1.720,1.880,4.320
,   "write",  1,"128K", 125.037,    0.996, 0.6,       12,    0.2, 1000 , 0, 0, 0, 0,77,22, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,1.112,1.480,2.064,2.928,3.536,6.432
,   "write",  4,  "1K",   5.938,    0.488, 0.1,      273,    2.3, 6080 , 0, 0,42,30,25, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.684,1.112,7.008,29.824,41.216,80.384
,   "write",  4,  "8K",  51.233,    0.454, 0.2,       11,    0.2, 6557 , 0, 0, 3,59,35, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.716,0.868,1.112,2.640,4.320,8.160
,   "write",  4,"128K", 217.346,    1.717, 0.6,     1562,   19.3, 1738 , 0, 0, 0, 0, 6,92, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,1.784,2.832,3.728,6.112,7.520,1531.904
,   "write", 16,  "1K",  15.325,    0.564, 0.2,     1012,    5.3,15693 , 0, 0, 9,80, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.668,5.728,8.512,25.984,36.608,154.624
,   "write", 16,  "8K", 187.948,    0.370, 0.2,       13,    0.2,24057 , 0, 0, 0,95, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.470,0.796,1.144,2.480,2.928,4.896
,   "write", 16,"128K", 314.433,    3.554, 0.6,      366,    9.4, 2515 , 0, 0, 0, 0, 0, 1,93, 2, 1, 0, 0, 0, 0, 0, 0, 0,3.632,14.656,24.704,160.768,272.384,325.632
),nrow=31)
tm <- t(m)
m <-tm
colnames <- c("name","users","bs","MB","lat","min","max","std","iops"
, "us50","us100","us250","us500","ms1","ms2","ms4","ms10","ms20"
, "ms50","ms100","ms250","ms500","s1","s2","s5"
,"p95_00", "p99_00", "p99_50", "p99_90", "p99_95", "p99_99"
)
colnames(m)=colnames
m <- data.frame(m)
testtype <- "pharos_run5_10"
source("fiogp.r")
m <- NULL 
m <- matrix(c(
"randread",  1,  "8K",   1.462,    5.336, 0.1,      174,   10.3,  187 , 0, 1, 9,39, 5, 0, 3,21,15, 2, 0, 0, 0, 0, 0, 0,17.792,38.656,58.624,154.624,175.104,175.104
,"randread",  8,  "8K",  66.371,    0.935, 0.1,      749,    5.8, 8495 , 0, 1,17,40,35, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0,0.596,15.040,21.120,39.168,57.088,228.352
,"randread", 16,  "8K", 154.730,    0.799, 0.1,      535,    4.3,19805 , 0, 0,46,21,28, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0,0.684,12.608,18.816,35.584,48.896,177.152
,"randread", 32,  "8K", 401.934,    0.612, 0.1,      459,    3.6,51447 , 0, 0,40,55, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,0.450,11.200,18.560,35.584,48.384,136.192
,"randread", 64,  "8K", 181.153,    2.698, 0.1,      602,    6.8,23187 , 0, 0, 1, 5,19,39,25, 3, 3, 1, 0, 0, 0, 0, 0, 0,9.408,25.728,35.072,78.336,99.840,234.496
,    "read",  1,  "8K",   2.201,    3.543, 0.2,     3547,   74.1,  281 , 0, 0, 4,24,65, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,1.272,22.912,59.136,782.336,1417.216,3555.328
,    "read",  1, "32K",  17.151,    1.818, 0.2,       71,    3.9,  548 , 0, 0, 0,17,62, 3, 3, 8, 2, 0, 0, 0, 0, 0, 0, 0,8.384,19.328,24.960,42.240,51.456,72.192
,    "read",  1,"128K",  42.391,    2.944, 0.5,       99,    6.3,  339 , 0, 0, 0, 0,72, 9, 1, 7, 5, 2, 0, 0, 0, 0, 0, 0,14.144,28.544,34.560,71.168,75.264,99.840
,    "read",  1,  "1M",  27.733,   36.040, 3.0,      149,   23.1,   27 , 0, 0, 0, 0, 0, 0,20, 0, 1,55,20, 1, 0, 0, 0, 0,70.000,124.000,133.000,151.000,151.000,151.000
,    "read",  8,  "1M", 103.626,   48.050, 3.0,      252,   31.1,  103 , 0, 0, 0, 0, 0, 0, 9, 3, 1,46,34, 4, 0, 0, 0, 0,101.000,167.000,182.000,245.000,253.000,253.000
,    "read", 16,  "1M", 191.642,   46.710, 3.0,      347,   36.0,  191 , 0, 0, 0, 0, 0, 0, 0, 5,28,22,34, 7, 0, 0, 0, 0,112.000,165.000,188.000,251.000,347.000,347.000
,    "read", 32,  "1M", 283.249,   61.225, 0.2,      888,   54.1,  283 , 0, 0, 0, 1, 2, 0, 0, 3,11,28,32,16, 0, 0, 0, 0,156.672,238.592,264.192,423.936,569.344,888.832
,    "read", 64,  "1M", 475.907,   69.765, 0.3,      428,   53.5,  475 , 0, 0, 0, 2, 4, 0, 1, 3, 7,15,37,24, 0, 0, 0, 0,164.864,234.496,259.072,329.728,387.072,428.032
,   "write",  1,  "1K",   1.897,    0.511, 0.1,      102,    1.2, 1943 , 0, 0, 9,27,63, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.620,0.756,0.884,1.480,2.224,87.552
,   "write",  1,  "8K",  15.090,    0.514, 0.2,        2,    0.1, 1931 , 0, 0, 5,28,66, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.636,0.700,0.756,1.004,1.288,2.320
,   "write",  1,"128K", 125.487,    0.992, 0.6,       10,    0.2, 1003 , 0, 0, 0, 0,80,19, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,1.080,1.400,2.096,3.120,5.472,9.920
,   "write",  4,  "1K",   6.697,    0.434, 0.1,      658,    3.5, 6858 , 0, 0,53,24,20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.644,1.064,6.816,15.424,22.912,123.392
,   "write",  4,  "8K",  52.138,    0.446, 0.2,        7,    0.2, 6673 , 0, 0, 4,56,38, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.692,0.764,0.844,1.320,1.592,2.704
,   "write",  4,"128K", 238.237,    1.568, 0.6,      474,    5.8, 1905 , 0, 0, 0, 0, 5,92, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,1.800,3.024,3.888,6.112,7.392,464.896
,   "write", 16,  "1K",  15.595,    0.559, 0.2,      727,    4.7,15969 , 0, 0,10,79, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.684,5.792,9.280,22.656,30.848,134.144
,   "write", 16,  "8K", 182.429,    0.381, 0.2,       56,    0.2,23350 , 0, 0, 1,93, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.506,1.012,1.304,2.384,2.928,3.856
,   "write", 16,"128K", 338.762,    3.298, 0.7,      264,    5.0, 2710 , 0, 0, 0, 0, 0, 2,92, 1, 1, 0, 0, 0, 0, 0, 0, 0,3.568,15.680,20.608,46.848,74.240,226.304
),nrow=31)
tm <- t(m)
m <-tm
colnames <- c("name","users","bs","MB","lat","min","max","std","iops"
, "us50","us100","us250","us500","ms1","ms2","ms4","ms10","ms20"
, "ms50","ms100","ms250","ms500","s1","s2","s5"
,"p95_00", "p99_00", "p99_50", "p99_90", "p99_95", "p99_99"
)
colnames(m)=colnames
m <- data.frame(m)
testtype <- "pharos_run6_10"
source("fiogp.r")
m <- NULL 
m <- matrix(c(
"randread",  1,  "8K",   1.843,    4.233, 0.1,      226,    8.8,  235 , 0, 3, 9,41, 7, 0, 3,20,12, 1, 0, 0, 0, 0, 0, 0,15.040,28.032,32.640,104.960,197.632,226.304
,"randread",  8,  "8K",  71.137,    0.872, 0.1,      180,    3.0, 9105 , 0, 1,18,35,38, 0, 0, 3, 1, 0, 0, 0, 0, 0, 0, 0,2.864,10.944,15.424,29.056,39.168,114.176
,"randread", 16,  "8K", 172.693,    0.712, 0.1,      190,    2.7,22104 , 0, 0,53,20,22, 0, 0, 2, 1, 0, 0, 0, 0, 0, 0, 0,0.700,10.688,14.784,28.544,37.632,94.720
,"randread", 32,  "8K", 393.801,    0.620, 0.1,      420,    3.3,50406 , 0, 0,40,54, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,0.470,11.456,18.304,36.096,47.872,117.248
,"randread", 64,  "8K", 169.485,    2.878, 0.1,      451,    6.6,21694 , 0, 0, 0, 4,16,39,29, 3, 3, 1, 0, 0, 0, 0, 0, 0,10.048,27.776,38.144,73.216,95.744,211.968
,    "read",  1,  "8K",   2.135,    3.654, 0.2,     3573,   75.7,  273 , 0, 0, 4,24,65, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.988,34.560,66.048,1155.072,1171.456,3588.096
,    "read",  1, "32K",  12.436,    2.508, 0.2,      224,    8.4,  397 , 0, 0, 0,19,62, 4, 2, 5, 2, 2, 0, 0, 0, 0, 0, 0,11.328,39.168,51.968,105.984,150.528,224.256
,    "read",  1,"128K",  35.400,    3.526, 0.5,      160,    9.0,  283 , 0, 0, 0, 0,67,14, 4, 4, 3, 4, 0, 0, 0, 0, 0, 0,19.328,43.264,57.088,92.672,142.336,160.768
,    "read",  1,  "1M",  41.562,   24.050, 3.0,      199,   27.4,   41 , 0, 0, 0, 0, 0, 0,43,12, 1,24,17, 1, 0, 0, 0, 0,71.000,106.000,125.000,200.000,200.000,200.000
,    "read",  8,  "1M", 110.170,   45.250, 3.0,      410,   40.5,  110 , 0, 0, 0, 0, 0, 0,16, 9, 2,31,32, 6, 0, 0, 0, 0,111.000,169.000,227.000,408.000,412.000,412.000
,    "read", 16,  "1M", 189.731,   46.960, 3.0,      286,   35.7,  189 , 0, 0, 0, 0, 0, 0, 0, 6,26,25,33, 8, 0, 0, 0, 0,113.000,161.000,176.000,227.000,285.000,285.000
,    "read", 32,  "1M", 250.856,   69.380, 3.0,      859,   61.0,  250 , 0, 0, 0, 0, 0, 0, 0, 1,11,31,34,18, 1, 0, 0, 0,172.000,306.000,404.000,486.000,523.000,857.000
,    "read", 64,  "1M", 437.010,   73.050, 0.3,      829,   66.3,  437 , 0, 0, 0, 3, 6, 0, 0, 2, 4,27,26,25, 1, 0, 0, 0,193.536,284.672,354.304,501.760,569.344,831.488
,   "write",  1,  "1K",   1.904,    0.509, 0.1,      250,    1.9, 1950 , 0, 0, 8,27,63, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.612,0.804,0.868,1.400,4.896,68.096
,   "write",  1,  "8K",  14.811,    0.524, 0.2,        6,    0.2, 1895 , 0, 0, 5,28,66, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.652,0.796,0.884,1.272,1.848,5.408
,   "write",  1,"128K", 124.574,    0.999, 0.6,       13,    0.3,  996 , 0, 0, 0, 0,78,20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,1.096,1.336,1.656,4.576,11.584,13.632
,   "write",  4,  "1K",   5.492,    0.522, 0.2,      571,    4.4, 5624 , 0, 0,41,28,28, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.700,1.012,6.816,29.312,47.360,195.584
,   "write",  4,  "8K",  50.330,    0.462, 0.2,      373,    1.5, 6442 , 0, 0, 3,57,38, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.708,0.820,0.956,1.688,1.992,6.112
,   "write",  4,"128K", 245.479,    1.522, 0.7,      156,    1.9, 1963 , 0, 0, 0, 0, 7,90, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,1.800,2.544,5.856,14.144,21.376,136.192
,   "write", 16,  "1K",  15.624,    0.558, 0.2,      712,    3.7,15999 , 0, 0,10,78, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.692,5.856,9.408,23.424,32.128,112.128
,   "write", 16,  "8K", 187.814,    0.370, 0.2,       16,    0.2,24040 , 0, 0, 0,95, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.466,0.644,0.780,1.800,2.992,12.352
,   "write", 16,"128K", 294.773,    3.800, 0.8,      280,    6.9, 2358 , 0, 0, 0, 0, 0, 0,94, 1, 1, 1, 0, 0, 0, 0, 0, 0,4.000,31.000,50.000,100.000,130.000,200.000
),nrow=31)
tm <- t(m)
m <-tm
colnames <- c("name","users","bs","MB","lat","min","max","std","iops"
, "us50","us100","us250","us500","ms1","ms2","ms4","ms10","ms20"
, "ms50","ms100","ms250","ms500","s1","s2","s5"
,"p95_00", "p99_00", "p99_50", "p99_90", "p99_95", "p99_99"
)
colnames(m)=colnames
m <- data.frame(m)
testtype <- "pharos_run7_10"
source("fiogp.r")
m <- NULL 
m <- matrix(c(
"randread",  1,  "8K",   1.591,    4.903, 0.1,      162,    9.5,  203 , 0, 1, 8,46, 7, 0, 2,12,17, 3, 0, 0, 0, 0, 0, 0,18.304,35.584,44.288,115.200,158.720,162.816
,"randread",  8,  "8K",  64.988,    0.955, 0.1,      340,    4.4, 8318 , 0, 2,17,41,35, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0,0.596,17.024,26.240,55.552,75.264,162.816
,"randread", 16,  "8K", 150.642,    0.821, 0.1,      390,    3.9,19282 , 0, 0,43,21,30, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0,0.668,14.144,20.864,41.216,52.992,126.464
,"randread", 32,  "8K", 395.789,    0.621, 0.1,      322,    3.3,50661 , 0, 0,45,51, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.438,12.224,20.352,44.288,56.064,103.936
,"randread", 64,  "8K", 154.435,    3.164, 0.1,      386,    6.8,19767 , 0, 0, 1, 4,12,35,36, 3, 3, 2, 0, 0, 0, 0, 0, 0,10.944,32.128,42.752,78.336,107.008,189.440
,    "read",  1,  "8K",   2.046,    3.813, 0.1,     4180,   91.4,  261 , 0, 0, 6,24,63, 0, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0,1.112,11.072,27.520,561.152,1941.504,4177.920
,    "read",  1, "32K",  33.526,    0.928, 0.2,      105,    2.9, 1072 , 0, 0, 1,24,70, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,0.844,10.432,16.192,40.192,49.920,100.864
,    "read",  1,"128K",  14.170,    8.814, 0.6,      524,   17.9,  113 , 0, 0, 0, 0, 8,29, 4,28,18, 9, 0, 0, 0, 0, 0, 0,25.472,42.240,57.088,78.336,528.384,528.384
,    "read",  1,  "1M",  42.504,   23.510, 2.0,      260,   26.2,   42 , 0, 0, 0, 0, 0, 0,47, 4, 1,31,14, 0, 0, 0, 0, 0,65.000,90.000,125.000,262.000,262.000,262.000
,    "read",  8,  "1M", 137.862,   36.110, 3.0,      257,   31.9,  137 , 0, 0, 0, 0, 0, 0, 8,26, 5,29,26, 3, 0, 0, 0, 0,93.000,131.000,153.000,239.000,258.000,258.000
,    "read", 16,  "1M", 186.708,   47.600, 3.0,      410,   39.1,  186 , 0, 0, 0, 0, 0, 0, 4,14, 8,33,31, 8, 0, 0, 0, 0,116.000,186.000,225.000,334.000,412.000,412.000
,    "read", 32,  "1M", 304.786,   57.491, 0.3,      669,   60.5,  304 , 0, 0, 0, 1, 2, 0, 5,14, 9,20,28,15, 1, 0, 0, 0,166.912,296.960,333.824,411.648,561.152,667.648
,    "read", 64,  "1M", 474.663,   63.482, 0.3,      664,   58.5,  474 , 0, 0, 0, 2, 4, 0, 1, 6, 6,29,25,20, 1, 0, 0, 0,168.960,272.384,309.248,378.880,552.960,667.648
,   "write",  1,  "1K",   1.919,    0.505, 0.1,      182,    1.4, 1965 , 0, 0, 9,27,62, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.612,0.732,0.860,3.152,7.712,32.640
,   "write",  1,  "8K",  14.836,    0.523, 0.2,        9,    0.2, 1899 , 0, 0, 4,27,67, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.644,0.740,0.852,1.704,2.832,8.512
,   "write",  1,"128K", 121.612,    1.024, 0.6,       23,    0.4,  972 , 0, 0, 0, 0,65,34, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,1.160,1.352,2.320,5.728,6.816,23.424
,   "write",  4,  "1K",   6.369,    0.456, 0.1,      175,    1.7, 6522 , 0, 0,43,30,24, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.692,1.528,6.624,22.400,30.336,55.552
,   "write",  4,  "8K",  50.981,    0.456, 0.2,       11,    0.2, 6525 , 0, 0, 3,57,38, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.708,0.812,1.032,2.160,2.928,6.304
,   "write",  4,"128K", 247.900,    1.507, 0.7,        9,    0.4, 1983 , 0, 0, 0, 0, 6,91, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,1.768,2.896,3.920,6.496,7.776,9.664
,   "write", 16,  "1K",  15.751,    0.552, 0.2,     1031,    3.8,16129 , 0, 0, 8,81, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.692,5.792,9.280,25.984,33.024,92.672
,   "write", 16,  "8K", 186.877,    0.372, 0.2,       11,    0.2,23920 , 0, 0, 0,95, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.466,0.724,1.176,2.352,3.120,5.856
,   "write", 16,"128K", 224.045,    4.993, 0.7,     1896,   42.7, 1792 , 0, 0, 0, 0, 0, 1,89, 4, 2, 1, 0, 0, 0, 0, 0, 0,9.920,30.080,49.920,80.384,1892.352,1892.352
),nrow=31)
tm <- t(m)
m <-tm
colnames <- c("name","users","bs","MB","lat","min","max","std","iops"
, "us50","us100","us250","us500","ms1","ms2","ms4","ms10","ms20"
, "ms50","ms100","ms250","ms500","s1","s2","s5"
,"p95_00", "p99_00", "p99_50", "p99_90", "p99_95", "p99_99"
)
colnames(m)=colnames
m <- data.frame(m)
testtype <- "pharos_run8_10"
source("fiogp.r")
m <- NULL 
m <- matrix(c(
"randread",  1,  "8K",   1.322,    5.898, 0.1,      226,   10.8,  169 , 0, 1, 6,32,11, 0, 3,20,19, 2, 0, 0, 0, 0, 0, 0,18.560,37.632,55.040,132.096,226.304,226.304
,"randread",  8,  "8K",  67.222,    0.923, 0.1,      348,    4.0, 8604 , 0, 1,17,40,35, 0, 0, 2, 1, 0, 0, 0, 0, 0, 0, 0,0.636,14.272,18.560,31.104,49.408,171.008
,"randread", 16,  "8K", 150.690,    0.821, 0.1,      299,    3.5,19288 , 0, 0,44,20,30, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0,0.668,14.528,19.584,36.096,48.384,121.344
,"randread", 32,  "8K", 396.380,    0.619, 0.1,      369,    3.5,50736 , 0, 0,44,52, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.438,12.608,19.584,37.120,49.408,122.368
,"randread", 64,  "8K", 159.228,    3.058, 0.1,      472,    7.6,20381 , 0, 0, 1, 4,13,39,31, 3, 3, 1, 0, 0, 0, 0, 0, 0,10.944,29.056,39.168,79.360,116.224,284.672
,    "read",  1,  "8K",  15.827,    0.490, 0.1,      123,    0.9, 2025 , 0, 0, 9,30,60, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.604,0.740,0.948,2.064,2.384,9.024
,    "read",  1, "32K",  22.101,    1.410, 0.2,      188,    5.0,  707 , 0, 0, 1,23,67, 1, 0, 2, 1, 0, 0, 0, 0, 0, 0, 0,5.216,22.656,30.848,62.720,83.456,187.392
,    "read",  1,"128K",  21.193,    5.892, 0.5,      309,   13.2,  169 , 0, 0, 0, 0,45,22, 3,11, 9, 6, 0, 0, 0, 0, 0, 0,25.984,47.360,71.168,148.480,309.248,309.248
,    "read",  1,  "1M",  37.502,   26.650, 3.0,      611,   48.0,   37 , 0, 0, 0, 0, 0, 0,27,21, 5,30,12, 1, 0, 0, 0, 0,70.000,159.000,562.000,611.000,611.000,611.000
,    "read",  8,  "1M", 130.131,   38.290, 3.0,      743,   38.8,  130 , 0, 0, 0, 0, 0, 0, 0,10,33,24,24, 5, 0, 0, 0, 0,103.000,155.000,186.000,233.000,742.000,742.000
,    "read", 16,  "1M", 172.526,   51.670, 3.0,      470,   49.3,  172 , 0, 0, 0, 0, 0, 0, 0, 3,33,23,25,12, 0, 0, 0, 0,145.000,245.000,289.000,351.000,469.000,469.000
,    "read", 32,  "1M", 262.734,   66.960, 3.0,      592,   67.6,  262 , 0, 0, 0, 0, 0, 0, 2, 9,15,20,30,17, 2, 0, 0, 0,200.000,343.000,375.000,478.000,510.000,594.000
,    "read", 64,  "1M", 445.157,   74.656, 0.2,      812,   82.9,  445 , 0, 0, 0, 3, 9, 0, 0, 1, 5,25,27,21, 3, 0, 0, 0,222.208,428.032,518.144,708.608,724.992,815.104
,   "write",  1,  "1K",   1.961,    0.494, 0.1,       38,    0.5, 2008 , 0, 0, 8,28,62, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.604,0.700,0.756,2.024,4.960,37.120
,   "write",  1,  "8K",  15.075,    0.514, 0.2,        6,    0.2, 1929 , 0, 0, 5,29,64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.636,0.716,0.812,1.480,1.880,4.768
,   "write",  1,"128K", 125.837,    0.989, 0.6,        9,    0.2, 1006 , 0, 0, 0, 0,80,19, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,1.080,1.320,1.912,2.832,4.128,4.832
,   "write",  4,  "1K",   7.809,    0.367, 0.1,      243,    1.4, 7996 , 0, 0,60,19,19, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.628,0.780,3.376,10.816,17.024,37.632
,   "write",  4,  "8K",  54.311,    0.428, 0.2,        7,    0.2, 6951 , 0, 0, 3,69,26, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.684,0.756,0.828,1.496,1.976,3.760
,   "write",  4,"128K", 106.776,    3.504, 0.7,     2880,   54.1,  854 , 0, 0, 0, 0, 6,82, 1, 4, 2, 1, 0, 0, 0, 0, 0, 0,9.920,20.096,30.080,70.144,89.600,2867.200
,   "write", 16,  "1K",  17.339,    0.501, 0.2,      379,    2.4,17755 , 0, 0,11,81, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.636,5.664,7.264,17.280,23.424,69.120
,   "write", 16,  "8K", 169.463,    0.372, 0.2,     1998,    4.1,21691 , 0, 0, 0,96, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0.450,0.604,0.724,1.384,1.992,2.960
,   "write", 16,"128K", 125.462,    8.923, 0.7,     4589,  137.1, 1003 , 0, 0, 0, 0, 0, 1,87, 3, 4, 3, 0, 0, 0, 0, 0, 0,19.840,40.192,60.160,120.320,4554.752,4554.752
),nrow=31)
tm <- t(m)
m <-tm
colnames <- c("name","users","bs","MB","lat","min","max","std","iops"
, "us50","us100","us250","us500","ms1","ms2","ms4","ms10","ms20"
, "ms50","ms100","ms250","ms500","s1","s2","s5"
,"p95_00", "p99_00", "p99_50", "p99_90", "p99_95", "p99_99"
)
colnames(m)=colnames
m <- data.frame(m)
testtype <- "pharos_run9_10"
source("fiogp.r")
