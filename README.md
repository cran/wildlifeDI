---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# wildlifeDI

[![](https://cranlogs.r-pkg.org/badges/wildlifeDI)](https://cran.r-project.org/package=wildlifeDI)

The wildlifeDI package facilitates the calculation of indices of dynamic interaction for wildlife telemetry data. There are also functions for more advanced contact analysis. For more information on the methods used within see the documentation but also see:

Long, J.A., Nelson, T.A., Webb, S.L., Gee, K.L. (2014) A critical examination of indices of dynamic interaction for wildlife telemetry studies. Journal of Animal Ecology. 83(5):1216-1233.

Long, J.A., Webb, S.L., Harju, S.M., Gee, K.L. (2022) Analyzing contacts and behavior from high frequency tracking data using the wildlifeDI R package. Geographical Analysis. 54(3):648-663.

## Installation

You can install the latest (under development version) of wildlifeDI from github with:


```r
devtools::install_github("jedalong/wildlifeDI")
#> 
#>   There is a binary version available but the source version is later:
#>          binary source needs_compilation
#> classInt  0.4-7  0.4-8              TRUE
#> 
#> package 'wk' successfully unpacked and MD5 sums checked
#> package 'proxy' successfully unpacked and MD5 sums checked
#> package 'e1071' successfully unpacked and MD5 sums checked
#> package 'filehash' successfully unpacked and MD5 sums checked
#> package 'sp' successfully unpacked and MD5 sums checked
#> package 'pixmap' successfully unpacked and MD5 sums checked
#> package 'units' successfully unpacked and MD5 sums checked
#> package 's2' successfully unpacked and MD5 sums checked
#> package 'DBI' successfully unpacked and MD5 sums checked
#> package 'tidyselect' successfully unpacked and MD5 sums checked
#> package 'generics' successfully unpacked and MD5 sums checked
#> package 'CircStats' successfully unpacked and MD5 sums checked
#> package 'adehabitatMA' successfully unpacked and MD5 sums checked
#> package 'ade4' successfully unpacked and MD5 sums checked
#> package 'sf' successfully unpacked and MD5 sums checked
#> package 'dplyr' successfully unpacked and MD5 sums checked
#> package 'adehabitatLT' successfully unpacked and MD5 sums checked
#> 
#> The downloaded binary packages are in
#> 	C:\Users\jlong83\AppData\Local\Temp\Rtmp6RfjRK\downloaded_packages
#>          checking for file 'C:\Users\jlong83\AppData\Local\Temp\Rtmp6RfjRK\remotes4fe01fe44f7e\jedalong-wildlifeDI-af7b5ab/DESCRIPTION' ...     checking for file 'C:\Users\jlong83\AppData\Local\Temp\Rtmp6RfjRK\remotes4fe01fe44f7e\jedalong-wildlifeDI-af7b5ab/DESCRIPTION' ...   ✔  checking for file 'C:\Users\jlong83\AppData\Local\Temp\Rtmp6RfjRK\remotes4fe01fe44f7e\jedalong-wildlifeDI-af7b5ab/DESCRIPTION'
#>       ─  preparing 'wildlifeDI':
#>    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   ✔  checking DESCRIPTION meta-information
#>       ─  checking for LF line-endings in source and make files and shell scripts
#>       ─  checking for empty or unneeded directories
#>   ─  looking to see if a 'data/datalist' file should be added
#>      NB: this package now depends on R (>=        NB: this package now depends on R (>= 3.5.0)
#>        WARNING: Added dependency on R >= 3.5.0 because serialized objects in
#>      serialize/load version 3 cannot be read in older versions of R.
#>      File(s) containing such objects:
#>        'wildlifeDI/data/mockhunt.RData'
#> ─  building 'wildlifeDI_0.5.0.tar.gz'
#>      
#> 
```

The version that can currently be downloaded from CRAN is wildlifeDI v0.4.1: 


```r
install.packages('wildlifeDI')
```

## Example

A small dataset is provided to demonstrate the functionality of the package.


```r
library(wildlifeDI)
#> Error in library(wildlifeDI): there is no package called 'wildlifeDI'
data(deer)
deer
#> [[1]]
#>          x       y                date   dx   dy       dist   dt     R2n    abs.angle   rel.angle
#> 1   737870 3851479 2005-03-07 19:03:00   21   12  24.186773  840       0  0.519146114          NA
#> 2   737891 3851491 2005-03-07 19:17:00    1  -30  30.016662  960     585 -1.537475331 -2.05662145
#> 3   737892 3851461 2005-03-07 19:33:00   37   50  62.201286  840     808  0.933725998  2.47120133
#> 4   737929 3851511 2005-03-07 19:47:00   -5  -57  57.218878  900    4505 -1.658291668 -2.59201767
#> 5   737924 3851454 2005-03-07 20:02:00  215  -38 218.332316  900    3541 -0.174937521  1.48335415
#> 6   738139 3851416 2005-03-07 20:17:00  -26    9  27.513633  960   76330  2.808348352  2.98328587
#> 7   738113 3851425 2005-03-07 20:33:00  -17  -22  27.802878  840   61965 -2.228684932  1.24615202
#> 8   738096 3851403 2005-03-07 20:47:00    8   -1   8.062258  900   56852 -0.124354995  2.10432994
#> 9   738104 3851402 2005-03-07 21:02:00   -5    7   8.602325 2700   60685  2.191045813  2.31540081
#> 10  738099 3851409 2005-03-07 21:47:00    0   -6   6.000000  900   57341 -1.570796327  2.52134317
#> 11  738099 3851403 2005-03-07 22:02:00   -2    4   4.472136  900   58217  2.034443936 -2.67794504
#> 12  738097 3851407 2005-03-07 22:17:00    2    0   2.000000  900   56713  0.000000000 -2.03444394
#> 13  738099 3851407 2005-03-07 22:32:00    4    1   4.123106  900   57625  0.244978663  0.24497866
#> 14  738103 3851408 2005-03-07 22:47:00   -7    2   7.280110  900   59330  2.863292995  2.61831433
#> 15  738096 3851410 2005-03-07 23:02:00    8   -6  10.000000  900   55837 -0.643501109  2.77639120
#> 16  738104 3851404 2005-03-07 23:17:00   -8   -3   8.544004  900   60381 -2.782821983 -2.13932087
#> 17  738096 3851401 2005-03-07 23:32:00  -11    3  11.401754  900   57160  2.875340604 -0.62502272
#> 18  738085 3851404 2005-03-07 23:47:00  -69 -157 171.493440  900   51850 -1.984876216  1.42296849
#> 19  738016 3851247 2005-03-08 00:02:00  -12  -18  21.633308  900   75140 -2.158798930 -0.17392271
#> 20  738004 3851229 2005-03-08 00:17:00  -12  -32  34.176015  900   80456 -1.929566997  0.22923193
#> 21  737992 3851197 2005-03-08 00:32:00  -27 -142 144.544111  960   94408 -1.758694208  0.17087279
#> 22  737965 3851055 2005-03-08 00:48:00   25  -34  42.201896  900  188801 -0.936773607  0.82192060
#> 23  737990 3851021 2005-03-08 01:03:00    8  -17  18.788294  840  224164 -1.130953744 -0.19418014
#> 24  737998 3851004 2005-03-08 01:17:00  -26    7  26.925824  900  242009  2.878597922 -2.27363364
#> 25  737972 3851011 2005-03-08 01:32:00   41    1  41.012193  960  229428  0.024385409 -2.85421251
#> 26  738013 3851012 2005-03-08 01:48:00  -21  -24  31.890437  840  238538 -2.289626326 -2.31401174
#> 27  737992 3850988 2005-03-08 02:02:00   77  -57  95.801879  900  255965 -0.637238143  1.65238818
#> 28  738069 3850931 2005-03-08 02:17:00   27    4  27.294688  900  339905  0.147078355  0.78431650
#> 29  738096 3850935 2005-03-08 02:32:00    4  -14  14.560220  900  347012 -1.292496668 -1.43957502
#> 30  738100 3850921 2005-03-08 02:47:00   10   39  40.261644  960  364264  1.319793640  2.61229031
#> 31  738110 3850960 2005-03-08 03:03:00  249   -1 249.002008  840  326961 -0.004016043 -1.32380968
#> 32  738359 3850959 2005-03-08 03:17:00   63  121 136.418474  900  509521  1.090756743  1.09477279
#> 33  738422 3851080 2005-03-08 03:32:00  159    6 159.113167  900  463905  0.037717952 -1.05303879
#> 34  738581 3851086 2005-03-08 03:47:00   -8  -11  13.601471  900  659970 -2.199592613 -2.23731057
#> 35  738573 3851075 2005-03-08 04:02:00   -1    0   1.000000  900  657425  3.141592654 -0.94200004
#> 36  738572 3851075 2005-03-08 04:17:00   -3   -3   4.242641  900  656020 -2.356194490  0.78539816
#> 37  738569 3851072 2005-03-08 04:32:00    3   -1   3.162278  900  654250 -0.321750554  2.03444394
#> 38  738572 3851071 2005-03-08 04:47:00   -5    0   5.000000  900  659268  3.141592654 -2.81984210
#> 39  738567 3851071 2005-03-08 05:02:00   -3    2   3.605551  900  652273  2.553590050 -0.58800260
#> 40  738564 3851073 2005-03-08 05:17:00    9    1   9.055385  900  646472  0.110657221 -2.44293283
#> 41  738573 3851074 2005-03-08 05:32:00   -8   -1   8.062258  900  658234 -3.017237659 -3.12789488
#> 42  738565 3851073 2005-03-08 05:47:00    3   -1   3.162278  900  647861 -0.321750554  2.69548710
#> 43  738568 3851072 2005-03-08 06:02:00    2    3   3.605551  900  652853  0.982793723  1.30454428
#> 44  738570 3851075 2005-03-08 06:17:00    9    5  10.295630  900  653216  0.507098504 -0.47569522
#> 45  738579 3851080 2005-03-08 06:32:00   -7   -7   9.899495  900  661882 -2.356194490 -2.86329299
#> 46  738572 3851073 2005-03-08 06:47:00   30  -70  76.157731  960  657640 -1.165904541  1.19028995
#> 47  738602 3851003 2005-03-08 07:03:00   18 -160 161.009317  840  762400 -1.458767364 -0.29286282
#> 48  738620 3850843 2005-03-08 07:17:00   87 -112 141.820309  900  966996 -0.910371587  0.54839578
#> 49  738707 3850731 2005-03-08 07:32:00   64  -26  69.079664  900 1260073 -0.385882669  0.52448892
#> 50  738771 3850705 2005-03-08 07:47:00  204 -130 241.900806  900 1410877 -0.567363333 -0.18148066
#> 51  738975 3850575 2005-03-08 08:02:00   -7   82  82.298238  960 2038241  1.655955719  2.22331905
#> 52  738968 3850657 2005-03-08 08:18:00  402  -17 402.359292  900 1881288 -0.042263376 -1.69821910
#> 53  739370 3850640 2005-03-08 08:33:00  111 -260 282.703024  840 2953921 -1.167297944 -1.12503457
#> 54  739481 3850380 2005-03-08 08:47:00 -567   -4 567.014109 1800 3803122 -3.134538097 -1.96724015
#> 55  738914 3850376 2005-03-08 09:17:00 -420  -26 420.803992  900 2306545 -3.079766787  0.05477131
#> 56  738494 3850350 2005-03-08 09:32:00  -96  102 140.071410  960 1664017  2.325900730 -0.87751779
#> 57  738398 3850452 2005-03-08 09:48:00   -9   57  57.706152  840 1333513  1.727398204 -0.59850253
#> 58  738389 3850509 2005-03-08 10:02:00    7  -43  43.566042  960 1210261 -1.409421216 -3.13681942
#> 59  738396 3850466 2005-03-08 10:18:00 -187  -29 189.235303 2700 1302845 -2.987738022 -1.57831681
#> 60  738209 3850437 2005-03-08 11:03:00   15  192 192.585046  900 1200685  1.492829693 -1.80261759
#> 61  738224 3850629 2005-03-08 11:18:00   10  -53  53.935146  900  847816 -1.384309425 -2.87713912
#> 62  738234 3850576 2005-03-08 11:33:00  -15   34  37.161808  840  947905  1.986288423 -2.91258746
#> 63  738219 3850610 2005-03-08 11:47:00    1   -7   7.071068  960  876962 -1.428899272  2.86799761
#> 64  738220 3850603 2005-03-08 12:03:00    2    2   2.828427  840  889876  0.785398163  2.21429744
#> 65  738222 3850605 2005-03-08 12:17:00    5   -2   5.385165  900  887780 -0.380506377 -1.16590454
#> 66  738227 3850603 2005-03-08 12:32:00  -10   -2  10.198039  900  894825 -2.944197094 -2.56369072
#> 67  738217 3850601 2005-03-08 12:47:00    1    5   5.099020  960  891293  1.373400767 -1.96558745
#> 68  738218 3850606 2005-03-08 13:03:00   11  -40  41.484937  840  883233 -1.302430116 -2.67583088
#> 69  738229 3850566 2005-03-08 13:17:00   21   -7  22.135944  900  962450 -0.321750554  0.98067956
#> 70  738250 3850559 2005-03-08 13:32:00  -31    8  32.015621  900  990800  2.889038378 -3.07239637
#> 71  738219 3850567 2005-03-08 13:47:00   -1    7   7.071068  960  953545  1.712693381 -1.17634500
#> 72  738218 3850574 2005-03-08 14:03:00    3   -1   3.162278  840  940129 -0.321750554 -2.03444394
#> 73  738221 3850573 2005-03-08 14:17:00   62   59  85.586214  960  944037  0.760609853  1.08236041
#> 74  738283 3850632 2005-03-08 14:33:00    9  -56  56.718604  840  887978 -1.411444686 -2.17205454
#> 75  738292 3850576 2005-03-08 14:47:00    1   39  39.012818  960  993493  1.545160918  2.95660560
#> 76  738293 3850615 2005-03-08 15:03:00    1   -4   4.123106  900  925425 -1.325817664 -2.87097858
#> 77  738294 3850611 2005-03-08 15:18:00    5   -8   9.433981  960  933200 -1.012197011  0.31362065
#> 78  738299 3850603 2005-03-08 15:34:00   -3    8   8.544004  780  951417  1.929566997  2.94176401
#> 79  738296 3850611 2005-03-08 15:47:00  -23   -6  23.769729  900  934900 -2.886410263  1.46720805
#> 80  738273 3850605 2005-03-08 16:02:00    9   -9  12.727922  900  926285 -0.785398163  2.10101210
#> 81  738282 3850596 2005-03-08 16:17:00    1  -87  87.005747  900  949433 -1.559302580 -0.77390442
#> 82  738283 3850509 2005-03-08 16:32:00   36  -38  52.345009  900 1111469 -0.812418613  0.74688397
#> 83  738319 3850471 2005-03-08 16:47:00  -42 -119 126.194295  900 1217665 -1.910088941 -1.09767033
#> 84  738277 3850352 2005-03-08 17:02:00  -30   12  32.310989  900 1435778  2.761086276 -1.61201009
#> 85  738247 3850364 2005-03-08 17:17:00   83  -12  83.862983  900 1385354 -0.143583394 -2.90466967
#> 86  738330 3850352 2005-03-08 17:32:00  -13   16  20.615528  900 1481729  2.253112882  2.39669628
#> 87  738317 3850368 2005-03-08 17:47:00    4    1   4.123106  900 1434130  0.244978663 -2.00813422
#> 88  738321 3850369 2005-03-08 18:02:00   -5   -5   7.071068  900 1435501 -2.356194490 -2.60117315
#> 89  738316 3850364 2005-03-08 18:17:00  -20   -3  20.223748  900 1442141 -2.992702706 -0.63650822
#> 90  738296 3850361 2005-03-08 18:32:00   61  -26  66.309879  900 1431400 -0.402911591  2.58979112
#> 91  738357 3850335 2005-03-08 18:47:00  -89   32  94.578010  960 1545905  2.796435004 -3.08383871
#> 92  738268 3850367 2005-03-08 19:03:00    2   55  55.036352  840 1394948  1.534448706 -1.26198630
#> 93  738270 3850422 2005-03-08 19:17:00   37  -32  48.918299  900 1277249 -0.713060833 -2.24750954
#> 94  738307 3850390 2005-03-08 19:32:00  112   99 149.482440  900 1376890  0.723864570  1.43692540
#> 95  738419 3850489 2005-03-08 19:47:00  279  155 319.164534  900 1281501  0.507098504 -0.21676607
#> 96  738698 3850644 2005-03-08 20:02:00   80   66 103.711137  900 1382809  0.689800045  0.18270154
#> 97  738778 3850710 2005-03-08 20:17:00  -11  -37  38.600518  900 1415825 -1.859771737 -2.54957178
#> 98  738767 3850673 2005-03-08 20:32:00    2    0   2.000000  900 1454245  0.000000000  1.85977174
#> 99  738769 3850673 2005-03-08 20:47:00   -4   -6   7.211103  900 1457837 -2.158798930 -2.15879893
#> 100 738765 3850667 2005-03-08 21:02:00    4   -1   4.123106  900 1460369 -0.244978663  1.91382027
#>  [ reached 'max' / getOption("max.print") -- omitted 451 rows ]
#> 
#> [[2]]
#>          x       y                date   dx   dy       dist  dt     R2n   abs.angle   rel.angle
#> 1   739311 3851055 2005-03-07 19:02:00  -13    1  13.038405 900       0  3.06482076          NA
#> 2   739298 3851056 2005-03-07 19:17:00  -13   75  76.118329 960     170  1.74242440 -1.32239637
#> 3   739285 3851131 2005-03-07 19:33:00  302    5 302.041388 840    6452  0.01655478 -1.72586962
#> 4   739587 3851136 2005-03-07 19:47:00   30   19  35.510562 900   82737  0.56456939  0.54801461
#> 5   739617 3851155 2005-03-07 20:02:00    1    2   2.236068 900  103636  1.10714872  0.54257932
#> 6   739618 3851157 2005-03-07 20:17:00    4   -5   6.403124 900  104653 -0.89605538 -2.00320410
#> 7   739622 3851152 2005-03-07 20:32:00   -1    0   1.000000 900  106130  3.14159265 -2.24553727
#> 8   739621 3851152 2005-03-07 20:47:00   -2    2   2.828427 900  105509  2.35619449 -0.78539816
#> 9   739619 3851154 2005-03-07 21:02:00   -1   -5   5.099020 900  104665 -1.76819189  2.15879893
#> 10  739618 3851149 2005-03-07 21:17:00   18    3  18.248288 900  103085  0.16514868  1.93334056
#> 11  739636 3851152 2005-03-07 21:32:00  -35   14  37.696154 900  115034  2.76108628  2.59593760
#> 12  739601 3851166 2005-03-07 21:47:00 -266  -20 266.750820 900   96421 -3.06654589  0.45555314
#> 13  739335 3851146 2005-03-07 22:02:00 -394   -1 394.001269 900    8857 -3.13905459 -0.07250870
#> 14  738941 3851145 2005-03-07 22:17:00  -96  578 585.918083 900  145000  1.73538384 -1.40874688
#> 15  738845 3851723 2005-03-07 22:32:00   -3   80  80.056230 960  663380  1.60827876 -0.12710507
#> 16  738842 3851803 2005-03-07 22:48:00   -9   -1   9.055385 840  779465 -3.03093543  1.64397111
#> 17  738833 3851802 2005-03-07 23:02:00  -50   17  52.810984 900  786493  2.81385415 -0.43839573
#> 18  738783 3851819 2005-03-07 23:17:00   52  -15  54.120237 900  862480 -0.28083772 -3.09469187
#> 19  738835 3851804 2005-03-07 23:32:00    1   37  37.013511 960  787577  1.54377588  1.82461360
#> 20  738836 3851841 2005-03-07 23:48:00    3  -30  30.149627 840  843421 -1.47112767 -3.01490355
#> 21  738839 3851811 2005-03-08 00:02:00   17  -15  22.671568 900  794320 -0.72297935  0.74814832
#> 22  738856 3851796 2005-03-08 00:17:00   -4  121 121.066098 900  756106  1.60384214  2.32682150
#> 23  738852 3851917 2005-03-08 00:32:00  431   60 435.156294 900  953725  0.13832216 -1.46551998
#> 24  739283 3851977 2005-03-08 00:47:00  350    5 350.035712 900  850868  0.01428474 -0.12403742
#> 25  739633 3851982 2005-03-08 01:02:00 -306   -4 306.026143 900  963013 -3.12852150  3.14037906
#> 26  739327 3851978 2005-03-08 01:17:00    8    3   8.544004 960  852185  0.35877067 -2.79589313
#> 27  739335 3851981 2005-03-08 01:33:00  226    0 226.000000 840  858052  0.00000000 -0.35877067
#> 28  739561 3851981 2005-03-08 01:47:00    4    0   4.000000 900  919976  0.00000000  0.00000000
#> 29  739565 3851981 2005-03-08 02:02:00   19   40  44.283180 900  921992  1.12734799  1.12734799
#> 30  739584 3852021 2005-03-08 02:17:00   -4    3   5.000000 900 1007685  2.49809154  1.37074355
#> 31  739580 3852024 2005-03-08 02:32:00    6   -2   6.324555 900 1011322 -0.32175055 -2.81984210
#> 32  739586 3852022 2005-03-08 02:47:00    8   -8  11.313708 900 1010714 -0.78539816 -0.46364761
#> 33  739594 3852014 2005-03-08 03:02:00   -3    3   4.242641 900  999770  2.35619449  3.14159265
#> 34  739591 3852017 2005-03-08 03:17:00   -9    1   9.055385 900 1003844  3.03093543  0.67474094
#> 35  739582 3852018 2005-03-08 03:32:00    1   11  11.045361 900 1000810  1.48013644 -1.55079899
#> 36  739583 3852029 2005-03-08 03:47:00    1  -15  15.033296 960 1022660 -1.50422816 -2.98436460
#> 37  739584 3852014 2005-03-08 04:03:00   -5    1   5.099020 840  994210  2.94419709 -1.83476005
#> 38  739579 3852015 2005-03-08 04:17:00    0    8   8.000000 960  993424  1.57079633 -1.37340077
#> 39  739579 3852023 2005-03-08 04:33:00    5   -2   5.385165 840 1008848 -0.38050638 -1.95130270
#> 40  739584 3852021 2005-03-08 04:47:00    0   -1   1.000000 900 1007685 -1.57079633 -1.19028995
#> 41  739584 3852020 2005-03-08 05:02:00   -6    4   7.211103 900 1005754  2.55359005 -2.15879893
#> 42  739578 3852024 2005-03-08 05:17:00    6   -6   8.485281 900 1010250 -0.78539816  2.94419709
#> 43  739584 3852018 2005-03-08 05:32:00   -3    4   5.000000 960 1001898  2.21429744  2.99969560
#> 44  739581 3852022 2005-03-08 05:48:00   -3   17  17.262677 840 1007989  1.74546853 -0.46882891
#> 45  739578 3852039 2005-03-08 06:02:00    3  -16  16.278821 900 1039545 -1.38544838 -3.13091690
#> 46  739581 3852023 2005-03-08 06:17:00    1   -4   4.123106 900 1009924 -1.32581766  0.05963071
#> 47  739582 3852019 2005-03-08 06:32:00    1    2   2.236068 900 1002737  1.10714872  2.43296638
#> 48  739583 3852021 2005-03-08 06:47:00    1   -5   5.099020 960 1007140 -1.37340077 -2.48054948
#> 49  739584 3852016 2005-03-08 07:03:00 -310  -38 312.320348 840  998050 -3.01962050 -1.64621973
#> 50  739274 3851978 2005-03-08 07:17:00 -399   90 409.024449 900  853298  2.91974154 -0.34382327
#> 51  738875 3852068 2005-03-08 07:32:00 -334   19 334.539983 900 1216265  3.08476767  0.16502613
#> 52  738541 3852087 2005-03-08 07:47:00  -64  -26  69.079664 900 1657924 -2.75570998  0.44270765
#> 53  738477 3852061 2005-03-08 08:02:00 -229  -39 232.297223 900 1707592 -2.97290542 -0.21719544
#> 54  738248 3852022 2005-03-08 08:17:00 -368  -90 378.845615 900 2065058 -2.90173558  0.07116984
#> 55  737880 3851932 2005-03-08 08:32:00 -244 -227 333.264159 960 2816890 -2.39227225  0.50946334
#> 56  737636 3851705 2005-03-08 08:48:00  -49   47  67.896981 840 3228125  2.37702481 -1.51388825
#> 57  737587 3851752 2005-03-08 09:02:00   73 -208 220.438200 960 3457985 -1.23326516  2.67289534
#> 58  737660 3851544 2005-03-08 09:18:00   81 -132 154.870914 840 2964922 -1.02041038  0.21285478
#> 59  737741 3851412 2005-03-08 09:32:00  105  -26 108.171161 960 2592349 -0.24273651  0.77767386
#> 60  737846 3851386 2005-03-08 09:48:00   -3  -85  85.052925 900 2255786 -1.60607580 -1.36333929
#> 61  737843 3851301 2005-03-08 10:03:00   55   24  60.008333 900 2215540  0.41145624  2.01753204
#> 62  737898 3851325 2005-03-08 10:18:00  111  -46 120.154068 840 2069469 -0.39287051 -0.80432675
#> 63  738009 3851279 2005-03-08 10:32:00   91 -296 309.672408 960 1745380 -1.27253481 -0.87966430
#> 64  738100 3850983 2005-03-08 10:48:00   35  -72  80.056230 900 1471705 -1.11832144  0.15421337
#> 65  738135 3850911 2005-03-08 11:03:00   20  -26  32.802439 900 1403712 -0.91510070  0.20322074
#> 66  738155 3850885 2005-03-08 11:18:00   15  -53  55.081757 900 1365236 -1.29499043 -0.37988973
#> 67  738170 3850832 2005-03-08 11:33:00  -22  -13  25.553865 900 1351610 -2.60788446 -1.31289403
#> 68  738148 3850819 2005-03-08 11:48:00   24   25  34.655447 900 1408265  0.80580349 -2.86949735
#> 69  738172 3850844 2005-03-08 12:03:00    1    7   7.071068 840 1341842  1.42889927  0.62309578
#> 70  738173 3850851 2005-03-08 12:17:00  -23   53  57.775427 900 1336660  1.98023354  0.55133426
#> 71  738150 3850904 2005-03-08 12:32:00   26  -89  92.720009 900 1370722 -1.28657081  3.01638096
#> 72  738176 3850815 2005-03-08 12:47:00    8  -12  14.422205 900 1345825 -0.98279372  0.30377709
#> 73  738184 3850803 2005-03-08 13:02:00   -6   -3   6.708204 960 1333633 -2.67794504 -1.69515132
#> 74  738178 3850800 2005-03-08 13:18:00    3    2   3.605551 840 1348714  0.58800260 -3.01723766
#> 75  738181 3850802 2005-03-08 13:32:00    4    1   4.123106 900 1340909  0.24497866 -0.34302394
#> 76  738185 3850803 2005-03-08 13:47:00   30  -20  36.055513 960 1331380 -0.58800260 -0.83298127
#> 77  738215 3850783 2005-03-08 14:03:00   46  -19  49.769469 900 1275200 -0.39169994  0.19630266
#> 78  738261 3850764 2005-03-08 14:18:00  137   19 138.311243 900 1187181  0.13780710  0.52950704
#> 79  738398 3850783 2005-03-08 14:33:00  207    9 207.195560 900  907553  0.04345090 -0.09435620
#> 80  738605 3850792 2005-03-08 14:48:00   17  126 127.141653 840  567605  1.43668554  1.39323465
#> 81  738622 3850918 2005-03-08 15:02:00    7  -39  39.623226 900  493490 -1.39320016 -2.82988570
#> 82  738629 3850879 2005-03-08 15:17:00   -2   -2   2.828427 900  496100 -2.35619449 -0.96299433
#> 83  738627 3850877 2005-03-08 15:32:00   61  -26  66.309879 960  499540 -0.40291159  1.95328290
#> 84  738688 3850851 2005-03-08 15:48:00  -73   56  92.005435 840  429745  2.48722241  2.89013400
#> 85  738615 3850907 2005-03-08 16:02:00   15    1  15.033296 900  506320  0.06656816 -2.42065424
#> 86  738630 3850908 2005-03-08 16:17:00   -3   -3   4.242641 960  485370 -2.35619449 -2.42276265
#> 87  738627 3850905 2005-03-08 16:33:00    2   10  10.198039 840  490356  1.37340077 -2.55359005
#> 88  738629 3850915 2005-03-08 16:47:00   -7   -8  10.630146 900  484724 -2.28962633  2.62015821
#> 89  738622 3850907 2005-03-08 17:02:00   10   -8  12.806248 900  496625 -0.67474094  1.61488538
#> 90  738632 3850899 2005-03-08 17:17:00   -2    2   2.828427 900  485377  2.35619449  3.03093543
#> 91  738630 3850901 2005-03-08 17:32:00    4   25  25.317978 900  487477  1.41214106 -0.94405343
#> 92  738634 3850926 2005-03-08 17:47:00   -4  -15  15.524175 900  474970 -1.83139872  3.03964552
#> 93  738630 3850911 2005-03-08 18:02:00  146  -49 154.003247 960  484497 -0.32380394  1.50759478
#> 94  738776 3850862 2005-03-08 18:18:00   -4 -154 154.051939 840  323474 -1.59676451 -1.27296057
#> 95  738772 3850708 2005-03-08 18:32:00    2   -1   2.236068 900  410930 -0.46364761  1.13311691
#> 96  738774 3850707 2005-03-08 18:47:00    9    8  12.041595 900  409473  0.72664234  1.19028995
#> 97  738783 3850715 2005-03-08 19:02:00   81   25  84.770278 960  394384  0.29936623 -0.42727611
#> 98  738864 3850740 2005-03-08 19:18:00  158  190 247.111311 900  299034  0.87709432  0.57772809
#> 99  739022 3850930 2005-03-08 19:33:00  140  147 203.000000 840   99146  0.80978357 -0.06731075
#> 100 739162 3851077 2005-03-08 19:47:00  109  -19 110.643572 900   22685 -0.17257796 -0.98236153
#>  [ reached 'max' / getOption("max.print") -- omitted 467 rows ]
#> 
#> attr(,"class")
#> [1] "ltraj" "list" 
#> attr(,"typeII")
#> [1] TRUE
#> attr(,"regular")
#> [1] FALSE
#> attr(,"proj4string")
#> An object of class "CRS"
#> Slot "projargs":
#> [1] "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
```

Different tests of dynamic interaction can be straightforwardly computed. Most require a distance threshold (dc) to define when two animals are proximal. Similarly, most methods require a time threshold (tc) to define when two fixes are simultaneous. 

Here we use dc = 50 and tc = 8 mins (8*60).

A few examples are given below:


```r
dc <- 50
tc <- 8*60
#Proximity Analysis
Prox(deer[1],deer[2])
#> Error in Prox(deer[1], deer[2]): could not find function "Prox"
#Doncaster's test
Don(deer[1],deer[2],tc=tc,dc=dc,plot=F)
#> Error in Don(deer[1], deer[2], tc = tc, dc = dc, plot = F): could not find function "Don"
#Kenward's Coefficient of Sociality
Cs(deer[1],deer[2],tc=tc)
#> Error in Cs(deer[1], deer[2], tc = tc): could not find function "Cs"
#Shirabe's correlation coefficient
Cr(deer[1],deer[2],tc=tc)
#> Error in Cr(deer[1], deer[2], tc = tc): could not find function "Cr"
#Dynamic Interaction index
DI(deer[1],deer[2],tc=tc)
#> Error in DI(deer[1], deer[2], tc = tc): could not find function "DI"
#Benhamou's interaction statistic
IAB(deer[1],deer[2],tc=tc,dc=dc)
#> Error in IAB(deer[1], deer[2], tc = tc, dc = dc): could not find function "IAB"
```

For much more detailed information on the package please see the documentation and the vignette.

--- END --- 
