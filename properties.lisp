(in-package :hvac)


(defparameter +water-properties+ '((source . "NIST: http://webbook.nist.gov/chemistry/fluid/")
                                   (legend . (:temperature "[C]"
                                              :pressure "[bar]"
                                              :density "[kg/m3]"
                                              :volume "[m3/kg]"
                                              :internal-energy "[kJ/kg]"
                                              :enthalpy "[kJ/kg]"
                                              :entropy "[J/g*K]"
                                              :isochoric-specific-heat "[J/g*K]"
                                              :isobaric-specific-heat "[J/g*K]"
                                              :sound-speed "[m/s]"
                                              :joule-thomson "[K/bar]"
                                              :viscosity "[Pa*s]"
                                              :thermal-conductivity "[W/m*K]"
                                              :phase "[--]"))
                                   (data . #3a (((1.0000 1.0000 999.90 0.0010001 4.1774 4.2774 0.015265 4.2148 4.2161 1407.4 -0.024045 0.0017309 0.56297 liquid)
                                                 (1.5000 1.0000 999.92 0.0010001 6.2851 6.3851 0.022946 4.2136 4.2145 1409.8 -0.023997 0.0017017 0.56392 liquid)
                                                 (2.0000 1.0000 999.94 0.0010001 8.3920 8.4920 0.030610 4.2124 4.2130 1412.2 -0.023950 0.0016734 0.56487 liquid)
                                                 (2.5000 1.0000 999.96 0.0010000 10.498 10.598 0.038258 4.2112 4.2116 1414.6 -0.023903 0.0016458 0.56582 liquid)
                                                 (3.0000 1.0000 999.97 0.0010000 12.604 12.704 0.045889 4.2100 4.2102 1417.0 -0.023857 0.0016189 0.56677 liquid)
                                                 (3.5000 1.0000 999.97 0.0010000 14.708 14.808 0.053504 4.2088 4.2088 1419.3 -0.023811 0.0015927 0.56772 liquid)
                                                 (4.0000 1.0000 999.97 0.0010000 16.812 16.912 0.061103 4.2075 4.2075 1421.6 -0.023765 0.0015672 0.56867 liquid)
                                                 (4.5000 1.0000 999.97 0.0010000 18.916 19.016 0.068685 4.2062 4.2063 1423.9 -0.023720 0.0015424 0.56962 liquid)
                                                 (5.0000 1.0000 999.97 0.0010000 21.019 21.119 0.076252 4.2049 4.2050 1426.2 -0.023676 0.0015181 0.57057 liquid)
                                                 (5.5000 1.0000 999.96 0.0010000 23.121 23.221 0.083803 4.2036 4.2039 1428.4 -0.023631 0.0014945 0.57152 liquid)
                                                 (6.0000 1.0000 999.94 0.0010001 25.223 25.323 0.091339 4.2022 4.2028 1430.6 -0.023588 0.0014714 0.57247 liquid)
                                                 (6.5000 1.0000 999.92 0.0010001 27.324 27.424 0.098859 4.2008 4.2017 1432.8 -0.023544 0.0014490 0.57342 liquid)
                                                 (7.0000 1.0000 999.90 0.0010001 29.424 29.524 0.10636 4.1994 4.2006 1434.9 -0.023501 0.0014270 0.57437 liquid)
                                                 (7.5000 1.0000 999.88 0.0010001 31.524 31.624 0.11385 4.1980 4.1996 1437.0 -0.023458 0.0014056 0.57532 liquid)
                                                 (8.0000 1.0000 999.85 0.0010001 33.624 33.724 0.12133 4.1965 4.1987 1439.1 -0.023416 0.0013847 0.57627 liquid)
                                                 (8.5000 1.0000 999.82 0.0010002 35.723 35.823 0.12879 4.1951 4.1977 1441.2 -0.023374 0.0013643 0.57721 liquid)
                                                 (9.0000 1.0000 999.78 0.0010002 37.822 37.922 0.13623 4.1936 4.1969 1443.3 -0.023333 0.0013444 0.57816 liquid)
                                                 (9.5000 1.0000 999.74 0.0010003 39.920 40.020 0.14366 4.1921 4.1960 1445.3 -0.023291 0.0013249 0.57910 liquid)
                                                 (10.000 1.0000 999.70 0.0010003 42.018 42.118 0.15108 4.1906 4.1952 1447.3 -0.023250 0.0013059 0.58005 liquid)
                                                 (10.500 1.0000 999.66 0.0010003 44.115 44.215 0.15848 4.1890 4.1944 1449.2 -0.023210 0.0012873 0.58099 liquid)
                                                 (11.000 1.0000 999.61 0.0010004 46.212 46.312 0.16586 4.1875 4.1936 1451.2 -0.023170 0.0012691 0.58193 liquid)
                                                 (11.500 1.0000 999.56 0.0010004 48.309 48.409 0.17324 4.1859 4.1929 1453.1 -0.023130 0.0012514 0.58287 liquid)
                                                 (12.000 1.0000 999.50 0.0010005 50.405 50.505 0.18059 4.1843 4.1922 1455.0 -0.023090 0.0012340 0.58380 liquid)
                                                 (12.500 1.0000 999.44 0.0010006 52.501 52.601 0.18794 4.1827 4.1915 1456.9 -0.023051 0.0012170 0.58474 liquid)
                                                 (13.000 1.0000 999.38 0.0010006 54.596 54.696 0.19527 4.1811 4.1908 1458.7 -0.023011 0.0012004 0.58567 liquid)
                                                 (13.500 1.0000 999.31 0.0010007 56.692 56.792 0.20258 4.1794 4.1902 1460.6 -0.022973 0.0011842 0.58660 liquid)
                                                 (14.000 1.0000 999.25 0.0010008 58.786 58.887 0.20989 4.1778 4.1896 1462.4 -0.022934 0.0011683 0.58753 liquid)
                                                 (14.500 1.0000 999.18 0.0010008 60.881 60.981 0.21717 4.1761 4.1890 1464.2 -0.022896 0.0011528 0.58846 liquid)
                                                 (15.000 1.0000 999.10 0.0010009 62.975 63.076 0.22445 4.1744 4.1885 1465.9 -0.022858 0.0011375 0.58938 liquid)
                                                 (15.500 1.0000 999.03 0.0010010 65.070 65.170 0.23171 4.1727 4.1879 1467.7 -0.022820 0.0011227 0.59030 liquid)
                                                 (16.000 1.0000 998.95 0.0010011 67.163 67.263 0.23896 4.1710 4.1874 1469.4 -0.022783 0.0011081 0.59122 liquid)
                                                 (16.500 1.0000 998.86 0.0010011 69.257 69.357 0.24619 4.1693 4.1869 1471.1 -0.022746 0.0010938 0.59214 liquid)
                                                 (17.000 1.0000 998.78 0.0010012 71.350 71.450 0.25341 4.1675 4.1865 1472.8 -0.022709 0.0010798 0.59305 liquid)
                                                 (17.500 1.0000 998.69 0.0010013 73.443 73.544 0.26062 4.1657 4.1860 1474.4 -0.022672 0.0010661 0.59396 liquid)
                                                 (18.000 1.0000 998.60 0.0010014 75.536 75.636 0.26781 4.1640 4.1856 1476.0 -0.022635 0.0010527 0.59487 liquid)
                                                 (18.500 1.0000 998.50 0.0010015 77.629 77.729 0.27500 4.1622 4.1852 1477.6 -0.022599 0.0010395 0.59577 liquid)
                                                 (19.000 1.0000 998.41 0.0010016 79.721 79.822 0.28216 4.1603 4.1848 1479.2 -0.022563 0.0010266 0.59667 liquid)
                                                 (19.500 1.0000 998.31 0.0010017 81.814 81.914 0.28932 4.1585 4.1844 1480.8 -0.022527 0.0010140 0.59757 liquid)
                                                 (20.000 1.0000 998.21 0.0010018 83.906 84.006 0.29646 4.1567 4.1841 1482.3 -0.022492 0.0010016 0.59846 liquid)
                                                 (20.500 1.0000 998.10 0.0010019 85.998 86.098 0.30359 4.1548 4.1837 1483.9 -0.022456 0.00098946 0.59935 liquid)
                                                 (21.000 1.0000 997.99 0.0010020 88.090 88.190 0.31071 4.1530 4.1834 1485.4 -0.022421 0.00097755 0.60024 liquid)
                                                 (21.500 1.0000 997.89 0.0010021 90.181 90.281 0.31782 4.1511 4.1831 1486.9 -0.022386 0.00096588 0.60112 liquid)
                                                 (22.000 1.0000 997.77 0.0010022 92.273 92.373 0.32491 4.1492 4.1828 1488.3 -0.022351 0.00095442 0.60200 liquid)
                                                 (22.500 1.0000 997.66 0.0010023 94.364 94.464 0.33199 4.1473 4.1825 1489.8 -0.022317 0.00094319 0.60288 liquid)
                                                 (23.000 1.0000 997.54 0.0010025 96.455 96.555 0.33905 4.1454 4.1822 1491.2 -0.022282 0.00093216 0.60375 liquid)
                                                 (23.500 1.0000 997.42 0.0010026 98.546 98.646 0.34611 4.1434 4.1820 1492.6 -0.022248 0.00092135 0.60462 liquid)
                                                 (24.000 1.0000 997.30 0.0010027 100.64 100.74 0.35315 4.1415 4.1818 1494.0 -0.022214 0.00091073 0.60548 liquid)
                                                 (24.500 1.0000 997.17 0.0010028 102.73 102.83 0.36018 4.1395 4.1815 1495.4 -0.022180 0.00090031 0.60634 liquid)
                                                 (25.000 1.0000 997.05 0.0010030 104.82 104.92 0.36720 4.1376 4.1813 1496.7 -0.022147 0.00089008 0.60719 liquid)
                                                 (25.500 1.0000 996.92 0.0010031 106.91 107.01 0.37421 4.1356 4.1811 1498.0 -0.022113 0.00088004 0.60805 liquid)
                                                 (26.000 1.0000 996.79 0.0010032 109.00 109.10 0.38120 4.1336 4.1809 1499.3 -0.022080 0.00087018 0.60889 liquid)
                                                 (26.500 1.0000 996.65 0.0010034 111.09 111.19 0.38818 4.1316 4.1808 1500.6 -0.022047 0.00086050 0.60973 liquid)
                                                 (27.000 1.0000 996.52 0.0010035 113.18 113.28 0.39515 4.1296 4.1806 1501.9 -0.022014 0.00085099 0.61057 liquid)
                                                 (27.500 1.0000 996.38 0.0010036 115.27 115.37 0.40211 4.1275 4.1804 1503.2 -0.021981 0.00084165 0.61141 liquid)
                                                 (28.000 1.0000 996.24 0.0010038 117.36 117.46 0.40906 4.1255 4.1803 1504.4 -0.021948 0.00083248 0.61223 liquid)
                                                 (28.500 1.0000 996.09 0.0010039 119.45 119.55 0.41599 4.1235 4.1802 1505.6 -0.021916 0.00082346 0.61306 liquid)
                                                 (29.000 1.0000 995.95 0.0010041 121.54 121.64 0.42291 4.1214 4.1800 1506.8 -0.021883 0.00081460 0.61388 liquid)
                                                 (29.500 1.0000 995.80 0.0010042 123.63 123.73 0.42983 4.1193 4.1799 1508.0 -0.021851 0.00080590 0.61469 liquid)
                                                 (30.000 1.0000 995.65 0.0010044 125.72 125.82 0.43673 4.1172 4.1798 1509.2 -0.021819 0.00079735 0.61550 liquid)
                                                 (30.500 1.0000 995.50 0.0010045 127.81 127.91 0.44361 4.1151 4.1797 1510.3 -0.021787 0.00078894 0.61631 liquid)
                                                 (31.000 1.0000 995.34 0.0010047 129.90 130.00 0.45049 4.1130 4.1796 1511.4 -0.021755 0.00078067 0.61711 liquid)
                                                 (31.500 1.0000 995.19 0.0010048 131.99 132.09 0.45736 4.1109 4.1796 1512.5 -0.021724 0.00077255 0.61790 liquid)
                                                 (32.000 1.0000 995.03 0.0010050 134.08 134.18 0.46421 4.1088 4.1795 1513.6 -0.021692 0.00076456 0.61869 liquid)
                                                 (32.500 1.0000 994.87 0.0010052 136.17 136.27 0.47105 4.1066 4.1794 1514.7 -0.021661 0.00075670 0.61948 liquid)
                                                 (33.000 1.0000 994.70 0.0010053 138.26 138.36 0.47788 4.1045 4.1794 1515.8 -0.021630 0.00074898 0.62026 liquid)
                                                 (33.500 1.0000 994.54 0.0010055 140.35 140.45 0.48470 4.1023 4.1793 1516.8 -0.021598 0.00074138 0.62103 liquid)
                                                 (34.000 1.0000 994.37 0.0010057 142.44 142.54 0.49151 4.1002 4.1793 1517.8 -0.021567 0.00073390 0.62180 liquid)
                                                 (34.500 1.0000 994.20 0.0010058 144.53 144.63 0.49831 4.0980 4.1793 1518.8 -0.021536 0.00072655 0.62257 liquid)
                                                 (35.000 1.0000 994.03 0.0010060 146.62 146.72 0.50510 4.0958 4.1793 1519.8 -0.021506 0.00071932 0.62332 liquid)
                                                 (35.500 1.0000 993.86 0.0010062 148.71 148.81 0.51187 4.0936 4.1792 1520.8 -0.021475 0.00071220 0.62408 liquid)
                                                 (36.000 1.0000 993.68 0.0010064 150.80 150.90 0.51864 4.0914 4.1792 1521.8 -0.021444 0.00070519 0.62483 liquid)
                                                 (36.500 1.0000 993.51 0.0010065 152.89 152.99 0.52539 4.0892 4.1792 1522.7 -0.021414 0.00069830 0.62557 liquid)
                                                 (37.000 1.0000 993.33 0.0010067 154.98 155.08 0.53213 4.0870 4.1792 1523.7 -0.021384 0.00069152 0.62631 liquid)
                                                 (37.500 1.0000 993.15 0.0010069 157.07 157.17 0.53887 4.0847 4.1793 1524.6 -0.021353 0.00068484 0.62704 liquid)
                                                 (38.000 1.0000 992.97 0.0010071 159.16 159.26 0.54559 4.0825 4.1793 1525.5 -0.021323 0.00067827 0.62777 liquid)
                                                 (38.500 1.0000 992.78 0.0010073 161.25 161.35 0.55230 4.0802 4.1793 1526.3 -0.021293 0.00067180 0.62849 liquid)
                                                 (39.000 1.0000 992.59 0.0010075 163.33 163.44 0.55900 4.0780 4.1793 1527.2 -0.021263 0.00066543 0.62921 liquid)
                                                 (39.500 1.0000 992.41 0.0010077 165.42 165.53 0.56569 4.0757 4.1794 1528.1 -0.021233 0.00065915 0.62992 liquid)
                                                 (40.000 1.0000 992.22 0.0010078 167.51 167.62 0.57237 4.0734 4.1794 1528.9 -0.021204 0.00065298 0.63063 liquid)
                                                 (40.500 1.0000 992.02 0.0010080 169.60 169.70 0.57903 4.0711 4.1795 1529.7 -0.021174 0.00064689 0.63133 liquid)
                                                 (41.000 1.0000 991.83 0.0010082 171.69 171.79 0.58569 4.0688 4.1795 1530.5 -0.021144 0.00064090 0.63202 liquid)
                                                 (41.500 1.0000 991.63 0.0010084 173.78 173.88 0.59234 4.0665 4.1796 1531.3 -0.021115 0.00063500 0.63271 liquid)
                                                 (42.000 1.0000 991.44 0.0010086 175.87 175.97 0.59897 4.0642 4.1796 1532.1 -0.021085 0.00062919 0.63340 liquid)
                                                 (42.500 1.0000 991.24 0.0010088 177.96 178.06 0.60560 4.0619 4.1797 1532.9 -0.021056 0.00062346 0.63407 liquid)
                                                 (43.000 1.0000 991.04 0.0010090 180.05 180.15 0.61222 4.0596 4.1798 1533.6 -0.021027 0.00061782 0.63475 liquid)
                                                 (43.500 1.0000 990.83 0.0010093 182.14 182.24 0.61882 4.0572 4.1799 1534.3 -0.020998 0.00061226 0.63542 liquid)
                                                 (44.000 1.0000 990.63 0.0010095 184.23 184.33 0.62542 4.0549 4.1800 1535.1 -0.020969 0.00060678 0.63608 liquid)
                                                 (44.500 1.0000 990.42 0.0010097 186.32 186.42 0.63200 4.0525 4.1800 1535.8 -0.020940 0.00060139 0.63673 liquid)
                                                 (45.000 1.0000 990.21 0.0010099 188.41 188.51 0.63858 4.0502 4.1801 1536.4 -0.020911 0.00059607 0.63739 liquid)
                                                 (45.500 1.0000 990.00 0.0010101 190.50 190.60 0.64514 4.0478 4.1802 1537.1 -0.020882 0.00059082 0.63803 liquid)
                                                 (46.000 1.0000 989.79 0.0010103 192.59 192.69 0.65169 4.0454 4.1803 1537.8 -0.020853 0.00058565 0.63867 liquid)
                                                 (46.500 1.0000 989.58 0.0010105 194.68 194.78 0.65824 4.0430 4.1805 1538.4 -0.020824 0.00058056 0.63931 liquid)
                                                 (47.000 1.0000 989.36 0.0010108 196.77 196.87 0.66477 4.0407 4.1806 1539.1 -0.020795 0.00057554 0.63993 liquid)
                                                 (47.500 1.0000 989.14 0.0010110 198.86 198.96 0.67130 4.0383 4.1807 1539.7 -0.020767 0.00057059 0.64056 liquid)
                                                 (48.000 1.0000 988.93 0.0010112 200.95 201.06 0.67781 4.0359 4.1808 1540.3 -0.020738 0.00056571 0.64118 liquid)
                                                 (48.500 1.0000 988.71 0.0010114 203.04 203.15 0.68431 4.0334 4.1809 1540.9 -0.020710 0.00056089 0.64179 liquid)
                                                 (49.000 1.0000 988.48 0.0010117 205.13 205.24 0.69081 4.0310 4.1811 1541.5 -0.020681 0.00055615 0.64240 liquid)
                                                 (49.500 1.0000 988.26 0.0010119 207.23 207.33 0.69729 4.0286 4.1812 1542.0 -0.020653 0.00055146 0.64300 liquid)
                                                 (50.000 1.0000 988.03 0.0010121 209.32 209.42 0.70377 4.0262 4.1813 1542.6 -0.020625 0.00054685 0.64359 liquid)
                                                 (50.500 1.0000 987.81 0.0010123 211.41 211.51 0.71023 4.0238 4.1815 1543.1 -0.020596 0.00054229 0.64418 liquid)
                                                 (51.000 1.0000 987.58 0.0010126 213.50 213.60 0.71669 4.0213 4.1816 1543.6 -0.020568 0.00053780 0.64477 liquid)
                                                 (51.500 1.0000 987.35 0.0010128 215.59 215.69 0.72313 4.0189 4.1818 1544.2 -0.020540 0.00053337 0.64535 liquid)
                                                 (52.000 1.0000 987.12 0.0010131 217.68 217.78 0.72957 4.0164 4.1819 1544.7 -0.020512 0.00052900 0.64592 liquid)
                                                 (52.500 1.0000 986.88 0.0010133 219.77 219.87 0.73599 4.0140 4.1821 1545.1 -0.020484 0.00052469 0.64649 liquid)
                                                 (53.000 1.0000 986.65 0.0010135 221.86 221.96 0.74241 4.0115 4.1823 1545.6 -0.020456 0.00052044 0.64706 liquid)
                                                 (53.500 1.0000 986.41 0.0010138 223.95 224.05 0.74882 4.0090 4.1824 1546.1 -0.020428 0.00051624 0.64761 liquid)
                                                 (54.000 1.0000 986.17 0.0010140 226.04 226.15 0.75522 4.0066 4.1826 1546.5 -0.020400 0.00051210 0.64817 liquid)
                                                 (54.500 1.0000 985.93 0.0010143 228.14 228.24 0.76160 4.0041 4.1828 1547.0 -0.020372 0.00050801 0.64871 liquid)
                                                 (55.000 1.0000 985.69 0.0010145 230.23 230.33 0.76798 4.0016 4.1830 1547.4 -0.020344 0.00050398 0.64926 liquid)
                                                 (55.500 1.0000 985.45 0.0010148 232.32 232.42 0.77435 3.9991 4.1831 1547.8 -0.020316 0.00050000 0.64979 liquid)
                                                 (56.000 1.0000 985.21 0.0010150 234.41 234.51 0.78071 3.9966 4.1833 1548.2 -0.020288 0.00049607 0.65032 liquid)
                                                 (56.500 1.0000 984.96 0.0010153 236.50 236.60 0.78706 3.9941 4.1835 1548.6 -0.020261 0.00049219 0.65085 liquid)
                                                 (57.000 1.0000 984.71 0.0010155 238.59 238.69 0.79340 3.9916 4.1837 1549.0 -0.020233 0.00048836 0.65137 liquid)
                                                 (57.500 1.0000 984.46 0.0010158 240.68 240.79 0.79973 3.9891 4.1839 1549.3 -0.020205 0.00048458 0.65189 liquid)
                                                 (58.000 1.0000 984.21 0.0010160 242.78 242.88 0.80605 3.9866 4.1841 1549.7 -0.020177 0.00048085 0.65240 liquid)
                                                 (58.500 1.0000 983.96 0.0010163 244.87 244.97 0.81237 3.9841 4.1843 1550.0 -0.020150 0.00047717 0.65290 liquid)
                                                 (59.000 1.0000 983.71 0.0010166 246.96 247.06 0.81867 3.9815 4.1845 1550.3 -0.020122 0.00047353 0.65340 liquid)
                                                 (59.500 1.0000 983.45 0.0010168 249.05 249.16 0.82497 3.9790 4.1847 1550.7 -0.020094 0.00046994 0.65390 liquid)
                                                 (60.000 1.0000 983.20 0.0010171 251.15 251.25 0.83125 3.9765 4.1850 1551.0 -0.020067 0.00046640 0.65439 liquid)
                                                 (60.500 1.0000 982.94 0.0010174 253.24 253.34 0.83753 3.9739 4.1852 1551.3 -0.020039 0.00046289 0.65487 liquid)
                                                 (61.000 1.0000 982.68 0.0010176 255.33 255.43 0.84379 3.9714 4.1854 1551.5 -0.020012 0.00045944 0.65535 liquid)
                                                 (61.500 1.0000 982.42 0.0010179 257.42 257.53 0.85005 3.9689 4.1856 1551.8 -0.019984 0.00045602 0.65582 liquid)
                                                 (62.000 1.0000 982.15 0.0010182 259.52 259.62 0.85630 3.9663 4.1859 1552.1 -0.019957 0.00045265 0.65629 liquid)
                                                 (62.500 1.0000 981.89 0.0010184 261.61 261.71 0.86254 3.9638 4.1861 1552.3 -0.019929 0.00044932 0.65676 liquid)
                                                 (63.000 1.0000 981.63 0.0010187 263.70 263.80 0.86877 3.9612 4.1863 1552.6 -0.019902 0.00044603 0.65721 liquid)
                                                 (63.500 1.0000 981.36 0.0010190 265.80 265.90 0.87500 3.9586 4.1866 1552.8 -0.019874 0.00044278 0.65767 liquid)
                                                 (64.000 1.0000 981.09 0.0010193 267.89 267.99 0.88121 3.9561 4.1868 1553.0 -0.019847 0.00043957 0.65812 liquid)
                                                 (64.500 1.0000 980.82 0.0010196 269.98 270.08 0.88741 3.9535 4.1871 1553.2 -0.019819 0.00043639 0.65856 liquid)
                                                 (65.000 1.0000 980.55 0.0010198 272.08 272.18 0.89361 3.9509 4.1873 1553.4 -0.019792 0.00043326 0.65900 liquid)
                                                 (65.500 1.0000 980.28 0.0010201 274.17 274.27 0.89980 3.9484 4.1876 1553.6 -0.019764 0.00043016 0.65943 liquid)
                                                 (66.000 1.0000 980.00 0.0010204 276.26 276.37 0.90598 3.9458 4.1878 1553.8 -0.019737 0.00042710 0.65986 liquid)
                                                 (66.500 1.0000 979.73 0.0010207 278.36 278.46 0.91215 3.9432 4.1881 1553.9 -0.019710 0.00042408 0.66029 liquid)
                                                 (67.000 1.0000 979.45 0.0010210 280.45 280.55 0.91831 3.9406 4.1884 1554.1 -0.019682 0.00042109 0.66071 liquid)
                                                 (67.500 1.0000 979.17 0.0010213 282.55 282.65 0.92446 3.9381 4.1886 1554.2 -0.019655 0.00041814 0.66112 liquid)
                                                 (68.000 1.0000 978.90 0.0010216 284.64 284.74 0.93060 3.9355 4.1889 1554.3 -0.019627 0.00041522 0.66153 liquid)
                                                 (68.500 1.0000 978.61 0.0010219 286.73 286.84 0.93674 3.9329 4.1892 1554.5 -0.019600 0.00041234 0.66194 liquid)
                                                 (69.000 1.0000 978.33 0.0010221 288.83 288.93 0.94286 3.9303 4.1895 1554.6 -0.019572 0.00040949 0.66234 liquid)
                                                 (69.500 1.0000 978.05 0.0010224 290.92 291.03 0.94898 3.9277 4.1898 1554.7 -0.019545 0.00040668 0.66273 liquid)
                                                 (70.000 1.0000 977.76 0.0010227 293.02 293.12 0.95509 3.9251 4.1901 1554.7 -0.019518 0.00040389 0.66313 liquid)
                                                 (70.500 1.0000 977.48 0.0010230 295.11 295.22 0.96119 3.9225 4.1904 1554.8 -0.019490 0.00040114 0.66351 liquid)
                                                 (71.000 1.0000 977.19 0.0010233 297.21 297.31 0.96729 3.9199 4.1907 1554.9 -0.019463 0.00039842 0.66389 liquid)
                                                 (71.500 1.0000 976.90 0.0010236 299.30 299.41 0.97337 3.9173 4.1910 1554.9 -0.019435 0.00039573 0.66427 liquid)
                                                 (72.000 1.0000 976.61 0.0010239 301.40 301.50 0.97945 3.9147 4.1913 1555.0 -0.019408 0.00039307 0.66465 liquid)
                                                 (72.500 1.0000 976.32 0.0010243 303.50 303.60 0.98551 3.9121 4.1916 1555.0 -0.019380 0.00039044 0.66501 liquid)
                                                 (73.000 1.0000 976.03 0.0010246 305.59 305.69 0.99157 3.9095 4.1919 1555.1 -0.019353 0.00038785 0.66538 liquid)
                                                 (73.500 1.0000 975.73 0.0010249 307.69 307.79 0.99762 3.9069 4.1922 1555.1 -0.019325 0.00038528 0.66574 liquid)
                                                 (74.000 1.0000 975.44 0.0010252 309.78 309.89 1.0037 3.9043 4.1925 1555.1 -0.019298 0.00038274 0.66609 liquid)
                                                 (74.500 1.0000 975.14 0.0010255 311.88 311.98 1.0097 3.9017 4.1929 1555.1 -0.019270 0.00038022 0.66644 liquid)
                                                 (75.000 1.0000 974.84 0.0010258 313.98 314.08 1.0157 3.8990 4.1932 1555.1 -0.019243 0.00037774 0.66679 liquid)
                                                 (75.500 1.0000 974.54 0.0010261 316.07 316.18 1.0217 3.8964 4.1935 1555.0 -0.019215 0.00037528 0.66713 liquid)
                                                 (76.000 1.0000 974.24 0.0010264 318.17 318.27 1.0278 3.8938 4.1939 1555.0 -0.019188 0.00037285 0.66747 liquid)
                                                 (76.500 1.0000 973.94 0.0010268 320.27 320.37 1.0338 3.8912 4.1942 1555.0 -0.019160 0.00037045 0.66780 liquid)
                                                 (77.000 1.0000 973.64 0.0010271 322.36 322.47 1.0397 3.8886 4.1946 1554.9 -0.019132 0.00036807 0.66813 liquid)
                                                 (77.500 1.0000 973.33 0.0010274 324.46 324.56 1.0457 3.8860 4.1949 1554.9 -0.019105 0.00036572 0.66845 liquid)
                                                 (78.000 1.0000 973.03 0.0010277 326.56 326.66 1.0517 3.8833 4.1953 1554.8 -0.019077 0.00036340 0.66877 liquid)
                                                 (78.500 1.0000 972.72 0.0010280 328.66 328.76 1.0577 3.8807 4.1956 1554.7 -0.019049 0.00036110 0.66909 liquid)
                                                 (79.000 1.0000 972.41 0.0010284 330.75 330.86 1.0636 3.8781 4.1960 1554.6 -0.019022 0.00035882 0.66940 liquid)
                                                 (79.500 1.0000 972.10 0.0010287 332.85 332.96 1.0696 3.8755 4.1964 1554.5 -0.018994 0.00035657 0.66971 liquid)
                                                 (80.000 1.0000 971.79 0.0010290 334.95 335.05 1.0755 3.8728 4.1968 1554.4 -0.018966 0.00035435 0.67001 liquid)
                                                 (80.500 1.0000 971.48 0.0010294 337.05 337.15 1.0815 3.8702 4.1971 1554.3 -0.018938 0.00035215 0.67031 liquid)
                                                 (81.000 1.0000 971.16 0.0010297 339.15 339.25 1.0874 3.8676 4.1975 1554.2 -0.018910 0.00034997 0.67061 liquid)
                                                 (81.500 1.0000 970.85 0.0010300 341.25 341.35 1.0933 3.8650 4.1979 1554.1 -0.018882 0.00034781 0.67090 liquid)
                                                 (82.000 1.0000 970.53 0.0010304 343.35 343.45 1.0992 3.8623 4.1983 1553.9 -0.018855 0.00034568 0.67119 liquid)
                                                 (82.500 1.0000 970.22 0.0010307 345.45 345.55 1.1052 3.8597 4.1987 1553.8 -0.018827 0.00034357 0.67147 liquid)
                                                 (83.000 1.0000 969.90 0.0010310 347.54 347.65 1.1111 3.8571 4.1991 1553.6 -0.018799 0.00034148 0.67175 liquid)
                                                 (83.500 1.0000 969.58 0.0010314 349.64 349.75 1.1169 3.8545 4.1995 1553.4 -0.018771 0.00033941 0.67203 liquid)
                                                 (84.000 1.0000 969.26 0.0010317 351.74 351.85 1.1228 3.8518 4.1999 1553.3 -0.018743 0.00033737 0.67230 liquid)
                                                 (84.500 1.0000 968.93 0.0010321 353.84 353.95 1.1287 3.8492 4.2003 1553.1 -0.018714 0.00033534 0.67257 liquid)
                                                 (85.000 1.0000 968.61 0.0010324 355.94 356.05 1.1346 3.8466 4.2007 1552.9 -0.018686 0.00033334 0.67283 liquid)
                                                 (85.500 1.0000 968.29 0.0010328 358.04 358.15 1.1404 3.8440 4.2012 1552.7 -0.018658 0.00033136 0.67309 liquid)
                                                 (86.000 1.0000 967.96 0.0010331 360.15 360.25 1.1463 3.8413 4.2016 1552.5 -0.018630 0.00032940 0.67335 liquid)
                                                 (86.500 1.0000 967.63 0.0010334 362.25 362.35 1.1521 3.8387 4.2020 1552.2 -0.018602 0.00032746 0.67360 liquid)
                                                 (87.000 1.0000 967.30 0.0010338 364.35 364.45 1.1580 3.8361 4.2025 1552.0 -0.018573 0.00032553 0.67385 liquid)
                                                 (87.500 1.0000 966.98 0.0010342 366.45 366.55 1.1638 3.8335 4.2029 1551.8 -0.018545 0.00032363 0.67410 liquid)
                                                 (88.000 1.0000 966.64 0.0010345 368.55 368.65 1.1696 3.8308 4.2034 1551.5 -0.018517 0.00032175 0.67434 liquid)
                                                 (88.500 1.0000 966.31 0.0010349 370.65 370.76 1.1754 3.8282 4.2038 1551.3 -0.018488 0.00031989 0.67458 liquid)
                                                 (89.000 1.0000 965.98 0.0010352 372.75 372.86 1.1812 3.8256 4.2043 1551.0 -0.018460 0.00031804 0.67481 liquid)
                                                 (89.500 1.0000 965.64 0.0010356 374.86 374.96 1.1870 3.8230 4.2047 1550.7 -0.018431 0.00031622 0.67504 liquid)
                                                 (90.000 1.0000 965.31 0.0010359 376.96 377.06 1.1928 3.8204 4.2052 1550.4 -0.018403 0.00031441 0.67527 liquid)
                                                 (90.500 1.0000 964.97 0.0010363 379.06 379.17 1.1986 3.8177 4.2057 1550.2 -0.018374 0.00031262 0.67549 liquid)
                                                 (91.000 1.0000 964.63 0.0010367 381.16 381.27 1.2044 3.8151 4.2062 1549.9 -0.018345 0.00031085 0.67571 liquid)
                                                 (91.500 1.0000 964.29 0.0010370 383.27 383.37 1.2102 3.8125 4.2066 1549.6 -0.018317 0.00030909 0.67593 liquid)
                                                 (92.000 1.0000 963.95 0.0010374 385.37 385.47 1.2159 3.8099 4.2071 1549.2 -0.018288 0.00030735 0.67614 liquid)
                                                 (92.500 1.0000 963.61 0.0010378 387.47 387.58 1.2217 3.8073 4.2076 1548.9 -0.018259 0.00030563 0.67635 liquid)
                                                 (93.000 1.0000 963.27 0.0010381 389.58 389.68 1.2275 3.8046 4.2081 1548.6 -0.018230 0.00030393 0.67656 liquid)
                                                 (93.500 1.0000 962.93 0.0010385 391.68 391.79 1.2332 3.8020 4.2086 1548.3 -0.018201 0.00030224 0.67676 liquid)
                                                 (94.000 1.0000 962.58 0.0010389 393.79 393.89 1.2389 3.7994 4.2091 1547.9 -0.018172 0.00030057 0.67696 liquid)
                                                 (94.500 1.0000 962.23 0.0010392 395.89 396.00 1.2447 3.7968 4.2097 1547.6 -0.018143 0.00029892 0.67715 liquid)
                                                 (95.000 1.0000 961.89 0.0010396 398.00 398.10 1.2504 3.7942 4.2102 1547.2 -0.018114 0.00029728 0.67735 liquid)
                                                 (95.500 1.0000 961.54 0.0010400 400.10 400.21 1.2561 3.7916 4.2107 1546.8 -0.018085 0.00029566 0.67754 liquid)
                                                 (96.000 1.0000 961.19 0.0010404 402.21 402.31 1.2618 3.7890 4.2112 1546.5 -0.018055 0.00029405 0.67772 liquid)
                                                 (96.500 1.0000 960.84 0.0010408 404.31 404.42 1.2675 3.7864 4.2118 1546.1 -0.018026 0.00029246 0.67790 liquid)
                                                 (97.000 1.0000 960.49 0.0010411 406.42 406.52 1.2732 3.7838 4.2123 1545.7 -0.017997 0.00029089 0.67808 liquid)
                                                 (97.500 1.0000 960.13 0.0010415 408.53 408.63 1.2789 3.7812 4.2129 1545.3 -0.017967 0.00028933 0.67826 liquid)
                                                 (98.000 1.0000 959.78 0.0010419 410.63 410.74 1.2846 3.7786 4.2134 1544.9 -0.017938 0.00028778 0.67843 liquid)
                                                 (98.500 1.0000 959.42 0.0010423 412.74 412.84 1.2902 3.7760 4.2140 1544.4 -0.017908 0.00028625 0.67860 liquid)
                                                 (99.000 1.0000 959.07 0.0010427 414.85 414.95 1.2959 3.7734 4.2145 1544.0 -0.017879 0.00028473 0.67877 liquid)
                                                 (99.500 1.0000 958.71 0.0010431 416.95 417.06 1.3016 3.7708 4.2151 1543.6 -0.017849 0.00028323 0.67893 liquid)
                                                 (99.606 1.0000 958.63 0.0010432 417.40 417.50 1.3028 3.7702 4.2152 1543.5 -0.017843 0.00028291 0.67897 liquid)
                                                 (99.606 1.0000 0.59034 1.6939 2505.6 2674.9 7.3588 1.5548 2.0784 471.99 6.7038 1.2256e-05 0.025053 vapor)
                                                 (100.00 1.0000 0.58967 1.6959 2506.2 2675.8 7.3610 1.5535 2.0766 472.28 6.6638 1.2270e-05 0.025079 vapor))))))