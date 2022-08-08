Lab1
================
2022-08-03

``` r
# cargar librerias para R
library(readxl)
library(writexl)
library(readr)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6     ✔ dplyr   1.0.9
    ## ✔ tibble  3.1.8     ✔ stringr 1.4.0
    ## ✔ tidyr   1.2.0     ✔ forcats 0.5.1
    ## ✔ purrr   0.3.4     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(tidytext)
```

``` r
# lectura de archivos
enero <- readxl::read_excel('Datos/01-2018.xlsx')
febrero <- readxl::read_excel('Datos/02-2018.xlsx')
marzo <- readxl::read_excel('Datos/03-2018.xlsx')
abril <- readxl::read_excel('Datos/04-2018.xlsx')
mayo <- readxl::read_excel('Datos/05-2018.xlsx')
junio <- readxl::read_excel('Datos/06-2018.xlsx')
julio <- readxl::read_excel('Datos/07-2018.xlsx')
agosto <- readxl::read_excel('Datos/08-2018.xlsx')
```

    ## New names:
    ## • `` -> `...10`

``` r
septiembre <- readxl::read_excel('Datos/09-2018.xlsx')
octubre <- readxl::read_excel('Datos/10-2018.xlsx')
noviembre <- readxl::read_excel('Datos/11-2018.xlsx')
```

``` r
# se agrega columna de la fecha segun cada archivo
enero$Fecha <- '01-2018'
febrero$Fecha <- '02-2018'
marzo$Fecha <- '03-2018'
abril$Fecha <- '04-2018'
mayo$Fecha <- '05-2018'
junio$Fecha <- '06-2018'
julio$Fecha <- '07-2018'
agosto$Fecha <- '08-2018'
septiembre$Fecha <- '09-2018'
octubre$Fecha <- '10-2018'
noviembre$Fecha <- '11-2018'
```

``` r
# limpieza de datos, solo las columnas que pide
keeps <- c("COD_VIAJE", "CLIENTE", "UBICACION", "CANTIDAD", "PILOTO", "Q", "CREDITO", "UNIDAD", "Fecha")
enero <- enero[keeps]
febrero <- febrero[keeps]
marzo <- marzo[keeps]
abril <- abril[keeps]
mayo <- mayo[keeps]
junio <- junio[keeps]
julio <- julio[keeps]
agosto <- agosto[keeps]
septiembre <- septiembre[keeps]
octubre <- octubre[keeps]
noviembre <- noviembre[keeps]
```

``` r
# unificacion de datos
df_list <- list(enero, febrero, marzo, abril, mayo, junio,
                julio, agosto, septiembre, octubre, noviembre)

final <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list) 
final
```

    ##      COD_VIAJE                                            CLIENTE UBICACION
    ## 1     10000001            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 2     10000002                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 3     10000003           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 4     10000004                                TAQUERIA EL CHINITO     76002
    ## 5     10000005      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 6     10000006                            UBIQUO LABS |||FALTANTE     76001
    ## 7     10000007      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 8     10000008                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 9     10000009                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 10    10000010      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 11    10000011                EL GALLO NEGRO / Despacho a cliente     76001
    ## 12    10000012 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 13    10000013                          POLLO PINULITO|||FALTANTE     76001
    ## 14    10000014                EL GALLO NEGRO / Despacho a cliente     76001
    ## 15    10000015                                TAQUERIA EL CHINITO     76001
    ## 16    10000016            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 17    10000017        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 18    10000018           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 19    10000019            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 20    10000020                                        UBIQUO LABS     76001
    ## 21    10000021           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 22    10000022                                     BAR LA OFICINA     76001
    ## 23    10000023                            UBIQUO LABS |||FALTANTE     76001
    ## 24    10000024           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 25    10000025           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 26    10000026      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 27    10000027            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 28    10000028        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 29    10000029            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 30    10000030                                TAQUERIA EL CHINITO     76002
    ## 31    10000031            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 32    10000032                                TAQUERIA EL CHINITO     76002
    ## 33    10000033                     EL PINCHE OBELISCO |||Faltante     76002
    ## 34    10000034                                        UBIQUO LABS     76001
    ## 35    10000035 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 36    10000036                            UBIQUO LABS |||FALTANTE     76002
    ## 37    10000037                     EL PINCHE OBELISCO |||Faltante     76001
    ## 38    10000038        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 39    10000039            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 40    10000040                                TAQUERIA EL CHINITO     76001
    ## 41    10000041 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 42    10000042                            UBIQUO LABS |||FALTANTE     76002
    ## 43    10000043           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 44    10000044            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 45    10000045            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 46    10000046            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 47    10000047                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 48    10000048        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 49    10000049                  POLLO PINULITO/Despacho a cliente     76002
    ## 50    10000050           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 51    10000051                              HOSPITAL LAS AMERICAS     76001
    ## 52    10000052                                     BAR LA OFICINA     76002
    ## 53    10000053                EL GALLO NEGRO / Despacho a cliente     76001
    ## 54    10000054            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 55    10000055                  POLLO PINULITO/Despacho a cliente     76002
    ## 56    10000056                                     BAR LA OFICINA     76002
    ## 57    10000057                     EL PINCHE OBELISCO |||Faltante     76002
    ## 58    10000058                                        UBIQUO LABS     76001
    ## 59    10000059           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 60    10000060                                     BAR LA OFICINA     76002
    ## 61    10000061                                     BAR LA OFICINA     76002
    ## 62    10000062                                TAQUERIA EL CHINITO     76001
    ## 63    10000063 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 64    10000064                          POLLO PINULITO|||FALTANTE     76002
    ## 65    10000065                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 66    10000066                                     BAR LA OFICINA     76001
    ## 67    10000067                                        UBIQUO LABS     76002
    ## 68    10000068                                TAQUERIA EL CHINITO     76002
    ## 69    10000069                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 70    10000070                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 71    10000071                     EL PINCHE OBELISCO |||Faltante     76002
    ## 72    10000072                                     BAR LA OFICINA     76002
    ## 73    10000073                     EL PINCHE OBELISCO |||Faltante     76002
    ## 74    10000074                     EL PINCHE OBELISCO |||Faltante     76001
    ## 75    10000075                                     BAR LA OFICINA     76001
    ## 76    10000076                EL GALLO NEGRO / Despacho a cliente     76002
    ## 77    10000077            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 78    10000078        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 79    10000079 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 80    10000080                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 81    10000081            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 82    10000082 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 83    10000083            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 84    10000084            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 85    10000085            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 86    10000086                     EL PINCHE OBELISCO |||Faltante     76001
    ## 87    10000087 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 88    10000088                EL GALLO NEGRO / Despacho a cliente     76001
    ## 89    10000089            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 90    10000090                          POLLO PINULITO|||FALTANTE     76002
    ## 91    10000091 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 92    10000092        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 93    10000093                                     BAR LA OFICINA     76002
    ## 94    10000094                  POLLO PINULITO/Despacho a cliente     76002
    ## 95    10000095           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 96    10000096      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 97    10000097                                        UBIQUO LABS     76001
    ## 98    10000098                  POLLO PINULITO/Despacho a cliente     76002
    ## 99    10000099                     EL PINCHE OBELISCO |||Faltante     76001
    ## 100   10000100                                     BAR LA OFICINA     76001
    ## 101   10000101                     EL PINCHE OBELISCO |||Faltante     76001
    ## 102   10000102                          POLLO PINULITO|||FALTANTE     76001
    ## 103   10000103                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 104   10000104            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 105   10000105                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 106   10000106        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 107   10000107 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 108   10000108 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 109   10000109                                        UBIQUO LABS     76001
    ## 110   10000110                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 111   10000111      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 112   10000112            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 113   10000113                                TAQUERIA EL CHINITO     76001
    ## 114   10000114           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 115   10000115                              HOSPITAL LAS AMERICAS     76001
    ## 116   10000116                EL GALLO NEGRO / Despacho a cliente     76001
    ## 117   10000117           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 118   10000118            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 119   10000119            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 120   10000120                                TAQUERIA EL CHINITO     76002
    ## 121   10000121                                        UBIQUO LABS     76001
    ## 122   10000122                     EL PINCHE OBELISCO |||Faltante     76001
    ## 123   10000123 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 124   10000124                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 125   10000125                          POLLO PINULITO|||FALTANTE     76002
    ## 126   10000126                  POLLO PINULITO/Despacho a cliente     76002
    ## 127   10000127                                TAQUERIA EL CHINITO     76001
    ## 128   10000128                  POLLO PINULITO/Despacho a cliente     76001
    ## 129   10000129            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 130   10000130 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 131   10000131                                     BAR LA OFICINA     76002
    ## 132   10000132                     EL PINCHE OBELISCO |||Faltante     76002
    ## 133   10000133                          POLLO PINULITO|||FALTANTE     76002
    ## 134   10000134 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 135   10000135                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 136   10000136                     EL PINCHE OBELISCO |||Faltante     76001
    ## 137   10000137            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 138   10000138                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 139   10000139            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 140   10000140                                     BAR LA OFICINA     76001
    ## 141   10000141                     EL PINCHE OBELISCO |||Faltante     76002
    ## 142   10000142        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 143   10000143        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 144   10000144        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 145   10000145 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 146   10000146                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 147   10000147                     EL PINCHE OBELISCO |||Faltante     76001
    ## 148   10000148                                        UBIQUO LABS     76001
    ## 149   10000149                                TAQUERIA EL CHINITO     76001
    ## 150   10000150            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 151   10000151      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 152   10000152                                TAQUERIA EL CHINITO     76001
    ## 153   10000153        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 154   10000154 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 155   10000155                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 156   10000156            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 157   10000157                  POLLO PINULITO/Despacho a cliente     76001
    ## 158   10000158                     EL PINCHE OBELISCO |||Faltante     76001
    ## 159   10000159                                     BAR LA OFICINA     76001
    ## 160   10000160            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 161   10000161        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 162   10000162                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 163   10000163            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 164   10000164                EL GALLO NEGRO / Despacho a cliente     76002
    ## 165   10000165                                TAQUERIA EL CHINITO     76001
    ## 166   10000166                                     BAR LA OFICINA     76001
    ## 167   10000167        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 168   10000168                     EL PINCHE OBELISCO |||Faltante     76001
    ## 169   10000169 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 170   10000170                            UBIQUO LABS |||FALTANTE     76001
    ## 171   10000171                  POLLO PINULITO/Despacho a cliente     76001
    ## 172   10000172                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 173   10000173        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 174   10000174                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 175   10000175                                TAQUERIA EL CHINITO     76002
    ## 176   10000176                                TAQUERIA EL CHINITO     76002
    ## 177   10000177                            UBIQUO LABS |||FALTANTE     76002
    ## 178   10000178        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 179   10000179                                TAQUERIA EL CHINITO     76002
    ## 180   10000180                                TAQUERIA EL CHINITO     76002
    ## 181   10000181                          POLLO PINULITO|||FALTANTE     76002
    ## 182   10000182            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 183   10000183            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 184   10000184                                     BAR LA OFICINA     76002
    ## 185   10000185           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 186   10000186                                        UBIQUO LABS     76002
    ## 187   10000187                                        UBIQUO LABS     76001
    ## 188   10000188            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 189   10000189                                TAQUERIA EL CHINITO     76001
    ## 190   10000190        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 191   10000191                                TAQUERIA EL CHINITO     76002
    ## 192   10000192            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 193   10000193        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 194   10000194                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 195   10000195                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 196   10000196           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 197   10000197                          POLLO PINULITO|||FALTANTE     76002
    ## 198   10000198                          POLLO PINULITO|||FALTANTE     76002
    ## 199   10000199                                TAQUERIA EL CHINITO     76001
    ## 200   10000200                     EL PINCHE OBELISCO |||Faltante     76001
    ## 201   10000201                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 202   10000202            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 203   10000203                     EL PINCHE OBELISCO |||Faltante     76002
    ## 204   10000204                  POLLO PINULITO/Despacho a cliente     76001
    ## 205   10000205            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 206   10000206        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 207   10000207                  POLLO PINULITO/Despacho a cliente     76002
    ## 208   10000208            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 209   10000209                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 210   10000210            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 211   10000211      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 212   10000212      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 213   10000213           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 214   10000214                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 215   10000215           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 216   10000216                                TAQUERIA EL CHINITO     76001
    ## 217   10000217           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 218   10000218                          POLLO PINULITO|||FALTANTE     76001
    ## 219   10000219                                        UBIQUO LABS     76002
    ## 220   10000220                                     BAR LA OFICINA     76002
    ## 221   10000221                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 222   10000222                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 223   10000223      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 224   10000224                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 225   10000225 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 226   10000226            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 227   10000227                EL GALLO NEGRO / Despacho a cliente     76001
    ## 228   10000228           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 229   10000229                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 230   10000230           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 231   10000231                                     BAR LA OFICINA     76001
    ## 232   10000232            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 233   10000233                                        UBIQUO LABS     76001
    ## 234   10000234            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 235   10000235        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 236   10000236                                     BAR LA OFICINA     76001
    ## 237   10000237            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 238   10000238                                        UBIQUO LABS     76002
    ## 239   10000239        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 240   10000240                     EL PINCHE OBELISCO |||Faltante     76001
    ## 241   10000241                                     BAR LA OFICINA     76001
    ## 242   10000242                                TAQUERIA EL CHINITO     76001
    ## 243   10000243            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 244   10000244                EL GALLO NEGRO / Despacho a cliente     76001
    ## 245   10000245                                TAQUERIA EL CHINITO     76001
    ## 246   10000246                            UBIQUO LABS |||FALTANTE     76002
    ## 247   10000247           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 248   10000248            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 249   10000249      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 250   10000250                          POLLO PINULITO|||FALTANTE     76001
    ## 251   10000251                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 252   10000252      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 253   10000253      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 254   10000254 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 255   10000255                          POLLO PINULITO|||FALTANTE     76001
    ## 256   10000256 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 257   10000257                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 258   10000258            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 259   10000259                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 260   10000260                          POLLO PINULITO|||FALTANTE     76001
    ## 261   10000261                                        UBIQUO LABS     76002
    ## 262   10000262            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 263   10000263                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 264   10000264                     EL PINCHE OBELISCO |||Faltante     76001
    ## 265   10000265            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 266   10000266            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 267   10000267           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 268   10000268                            UBIQUO LABS |||FALTANTE     76001
    ## 269   10000269 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 270   10000270 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 271   10000271 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 272   10000272            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 273   10000273                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 274   10000274                EL GALLO NEGRO / Despacho a cliente     76002
    ## 275   10000275                          POLLO PINULITO|||FALTANTE     76001
    ## 276   10000276                                     BAR LA OFICINA     76002
    ## 277   10000277            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 278   10000278      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 279   10000279                EL GALLO NEGRO / Despacho a cliente     76002
    ## 280   10000280                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 281   10000281                                TAQUERIA EL CHINITO     76002
    ## 282   10000282                                        UBIQUO LABS     76002
    ## 283   10000283                  POLLO PINULITO/Despacho a cliente     76001
    ## 284   10000284                                TAQUERIA EL CHINITO     76001
    ## 285   10000285            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 286   10000286        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 287   10000287            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 288   10000288           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 289   10000289            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 290   10000290                                TAQUERIA EL CHINITO     76002
    ## 291   10000291                            UBIQUO LABS |||FALTANTE     76001
    ## 292   10000292      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 293   10000293                          POLLO PINULITO|||FALTANTE     76002
    ## 294   10000294                          POLLO PINULITO|||FALTANTE     76001
    ## 295   10000295                EL GALLO NEGRO / Despacho a cliente     76002
    ## 296   10000296            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 297   10000297                     EL PINCHE OBELISCO |||Faltante     76001
    ## 298   10000298            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 299   10000299            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 300   10000300                EL GALLO NEGRO / Despacho a cliente     76002
    ## 301   10000301        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 302   10000302                  POLLO PINULITO/Despacho a cliente     76001
    ## 303   10000303                                        UBIQUO LABS     76002
    ## 304   10000304                            UBIQUO LABS |||FALTANTE     76001
    ## 305   10000305                            UBIQUO LABS |||FALTANTE     76002
    ## 306   10000306            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 307   10000307            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 308   10000308            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 309   10000309                            UBIQUO LABS |||FALTANTE     76002
    ## 310   10000310      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 311   10000311                EL GALLO NEGRO / Despacho a cliente     76001
    ## 312   10000312                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 313   10000313           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 314   10000314                            UBIQUO LABS |||FALTANTE     76002
    ## 315   10000315                                        UBIQUO LABS     76001
    ## 316   10000316                                TAQUERIA EL CHINITO     76001
    ## 317   10000317                                        UBIQUO LABS     76001
    ## 318   10000318           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 319   10000319           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 320   10000320                              HOSPITAL LAS AMERICAS     76002
    ## 321   10000321                            UBIQUO LABS |||FALTANTE     76002
    ## 322   10000322            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 323   10000323                              HOSPITAL LAS AMERICAS     76002
    ## 324   10000324 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 325   10000325           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 326   10000326            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 327   10000327                              HOSPITAL LAS AMERICAS     76002
    ## 328   10000328                                TAQUERIA EL CHINITO     76002
    ## 329   10000329            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 330   10000330                                        UBIQUO LABS     76002
    ## 331   10000331                                TAQUERIA EL CHINITO     76002
    ## 332   10000332                                     BAR LA OFICINA     76001
    ## 333   10000333                            UBIQUO LABS |||FALTANTE     76001
    ## 334   10000334            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 335   10000335           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 336   10000336                                     BAR LA OFICINA     76001
    ## 337   10000337                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 338   10000338                  POLLO PINULITO/Despacho a cliente     76002
    ## 339   10000339                                TAQUERIA EL CHINITO     76002
    ## 340   10000340      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 341   10000341                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 342   10000342                              HOSPITAL LAS AMERICAS     76002
    ## 343   10000343            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 344   10000344                     EL PINCHE OBELISCO |||Faltante     76002
    ## 345   10000345                  POLLO PINULITO/Despacho a cliente     76002
    ## 346   10000346                                     BAR LA OFICINA     76002
    ## 347   10000347            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 348   10000348                              HOSPITAL LAS AMERICAS     76001
    ## 349   10000349                          POLLO PINULITO|||FALTANTE     76002
    ## 350   10000350                EL GALLO NEGRO / Despacho a cliente     76002
    ## 351   10000351                  POLLO PINULITO/Despacho a cliente     76001
    ## 352   10000352            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 353   10000353                EL GALLO NEGRO / Despacho a cliente     76001
    ## 354   10000354                                TAQUERIA EL CHINITO     76002
    ## 355   10000355            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 356   10000356 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 357   10000357                EL GALLO NEGRO / Despacho a cliente     76001
    ## 358   10000358                                        UBIQUO LABS     76002
    ## 359   10000359           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 360   10000360            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 361   10000361                                     BAR LA OFICINA     76002
    ## 362   10000362                EL GALLO NEGRO / Despacho a cliente     76002
    ## 363   10000363                              HOSPITAL LAS AMERICAS     76001
    ## 364   10000364                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 365   10000365                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 366   10000366            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 367   10000367                            UBIQUO LABS |||FALTANTE     76002
    ## 368   10000368                            UBIQUO LABS |||FALTANTE     76001
    ## 369   10000369                              HOSPITAL LAS AMERICAS     76001
    ## 370   10000370        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 371   10000371                              HOSPITAL LAS AMERICAS     76002
    ## 372   10000372            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 373   10000373                     EL PINCHE OBELISCO |||Faltante     76002
    ## 374   10000374                                        UBIQUO LABS     76002
    ## 375   10000375           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 376   10000376                                     BAR LA OFICINA     76002
    ## 377   10000377            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 378   10000378                                        UBIQUO LABS     76002
    ## 379   10000379                          POLLO PINULITO|||FALTANTE     76002
    ## 380   10000380                     EL PINCHE OBELISCO |||Faltante     76001
    ## 381   10000381                            UBIQUO LABS |||FALTANTE     76001
    ## 382   10000382            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 383   10000383            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 384   10000384 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 385   10000385            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 386   10000386                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 387   10000387                            UBIQUO LABS |||FALTANTE     76002
    ## 388   10000388 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 389   10000389        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 390   10000390                     EL PINCHE OBELISCO |||Faltante     76001
    ## 391   10000391 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 392   10000392                     EL PINCHE OBELISCO |||Faltante     76001
    ## 393   10000393            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 394   10000394                            UBIQUO LABS |||FALTANTE     76001
    ## 395   10000395      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 396   10000396            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 397   10000397            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 398   10000398 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 399   10000399                                     BAR LA OFICINA     76002
    ## 400   10000400                  POLLO PINULITO/Despacho a cliente     76002
    ## 401   10000401        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 402   10000402                                TAQUERIA EL CHINITO     76001
    ## 403   10000403                  POLLO PINULITO/Despacho a cliente     76002
    ## 404   10000404                              HOSPITAL LAS AMERICAS     76002
    ## 405   10000405 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 406   10000406                EL GALLO NEGRO / Despacho a cliente     76002
    ## 407   10000407                     EL PINCHE OBELISCO |||Faltante     76002
    ## 408   10000408 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 409   10000409        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 410   10000410                  POLLO PINULITO/Despacho a cliente     76001
    ## 411   10000411            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 412   10000412        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 413   10000413                            UBIQUO LABS |||FALTANTE     76002
    ## 414   10000414                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 415   10000415                                     BAR LA OFICINA     76002
    ## 416   10000416                                     BAR LA OFICINA     76001
    ## 417   10000417                          POLLO PINULITO|||FALTANTE     76001
    ## 418   10000418            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 419   10000419                              HOSPITAL LAS AMERICAS     76002
    ## 420   10000420                            UBIQUO LABS |||FALTANTE     76001
    ## 421   10000421 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 422   10000422                          POLLO PINULITO|||FALTANTE     76002
    ## 423   10000423                                TAQUERIA EL CHINITO     76001
    ## 424   10000424                                     BAR LA OFICINA     76001
    ## 425   10000425                EL GALLO NEGRO / Despacho a cliente     76002
    ## 426   10000426      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 427   10000427                                        UBIQUO LABS     76001
    ## 428   10000428                          POLLO PINULITO|||FALTANTE     76001
    ## 429   10000429                                     BAR LA OFICINA     76002
    ## 430   10000430 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 431   10000431        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 432   10000432                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 433   10000433      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 434   10000434           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 435   10000435                EL GALLO NEGRO / Despacho a cliente     76002
    ## 436   10000436                                TAQUERIA EL CHINITO     76001
    ## 437   10000437                                     BAR LA OFICINA     76002
    ## 438   10000438        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 439   10000439                  POLLO PINULITO/Despacho a cliente     76002
    ## 440   10000440 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 441   10000441 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 442   10000442                EL GALLO NEGRO / Despacho a cliente     76001
    ## 443   10000443      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 444   10000444 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 445   10000445                                     BAR LA OFICINA     76001
    ## 446   10000446                                TAQUERIA EL CHINITO     76002
    ## 447   10000447                                TAQUERIA EL CHINITO     76002
    ## 448   10000448        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 449   10000449                                     BAR LA OFICINA     76001
    ## 450   10000450           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 451   10000451            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 452   10000452                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 453   10000453                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 454   10000454      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 455   10000455            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 456   10000456                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 457   10000457           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 458   10000458      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 459   10000459                     EL PINCHE OBELISCO |||Faltante     76001
    ## 460   10000460                     EL PINCHE OBELISCO |||Faltante     76002
    ## 461   10000461 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 462   10000462                     EL PINCHE OBELISCO |||Faltante     76001
    ## 463   10000463                          POLLO PINULITO|||FALTANTE     76001
    ## 464   10000464                                        UBIQUO LABS     76002
    ## 465   10000465                          POLLO PINULITO|||FALTANTE     76002
    ## 466   10000466                                TAQUERIA EL CHINITO     76001
    ## 467   10000467                                        UBIQUO LABS     76001
    ## 468   10000468                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 469   10000469           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 470   10000470                                        UBIQUO LABS     76001
    ## 471   10000471                              HOSPITAL LAS AMERICAS     76001
    ## 472   10000472                                        UBIQUO LABS     76002
    ## 473   10000473        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 474   10000474            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 475   10000475                                        UBIQUO LABS     76002
    ## 476   10000476                EL GALLO NEGRO / Despacho a cliente     76001
    ## 477   10000477                EL GALLO NEGRO / Despacho a cliente     76001
    ## 478   10000478            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 479   10000479            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 480   10000480                EL GALLO NEGRO / Despacho a cliente     76001
    ## 481   10000481                     EL PINCHE OBELISCO |||Faltante     76002
    ## 482   10000482            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 483   10000483      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 484   10000484            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 485   10000485                  POLLO PINULITO/Despacho a cliente     76001
    ## 486   10000486            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 487   10000487            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 488   10000488                            UBIQUO LABS |||FALTANTE     76001
    ## 489   10000489                EL GALLO NEGRO / Despacho a cliente     76002
    ## 490   10000490                  POLLO PINULITO/Despacho a cliente     76001
    ## 491   10000491            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 492   10000492                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 493   10000493           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 494   10000494            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 495   10000495                EL GALLO NEGRO / Despacho a cliente     76002
    ## 496   10000496                                     BAR LA OFICINA     76002
    ## 497   10000497                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 498   10000498                                        UBIQUO LABS     76002
    ## 499   10000499            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 500   10000500                          POLLO PINULITO|||FALTANTE     76002
    ## 501   10000501                     EL PINCHE OBELISCO |||Faltante     76002
    ## 502   10000502            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 503   10000503        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 504   10000504                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 505   10000505                            UBIQUO LABS |||FALTANTE     76002
    ## 506   10000506                                     BAR LA OFICINA     76002
    ## 507   10000507           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 508   10000508                EL GALLO NEGRO / Despacho a cliente     76002
    ## 509   10000509                     EL PINCHE OBELISCO |||Faltante     76002
    ## 510   10000510                EL GALLO NEGRO / Despacho a cliente     76002
    ## 511   10000511                            UBIQUO LABS |||FALTANTE     76001
    ## 512   10000512                                TAQUERIA EL CHINITO     76001
    ## 513   10000513           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 514   10000514                            UBIQUO LABS |||FALTANTE     76002
    ## 515   10000515            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 516   10000516            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 517   10000517                              HOSPITAL LAS AMERICAS     76002
    ## 518   10000518           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 519   10000519                            UBIQUO LABS |||FALTANTE     76001
    ## 520   10000520            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 521   10000521                                     BAR LA OFICINA     76001
    ## 522   10000522                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 523   10000523            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 524   10000524                                     BAR LA OFICINA     76001
    ## 525   10000525            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 526   10000526            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 527   10000527            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 528   10000528                          POLLO PINULITO|||FALTANTE     76002
    ## 529   10000529                                        UBIQUO LABS     76001
    ## 530   10000530      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 531   10000531                            UBIQUO LABS |||FALTANTE     76002
    ## 532   10000532                            UBIQUO LABS |||FALTANTE     76002
    ## 533   10000533                              HOSPITAL LAS AMERICAS     76001
    ## 534   10000534                     EL PINCHE OBELISCO |||Faltante     76001
    ## 535   10000535                            UBIQUO LABS |||FALTANTE     76001
    ## 536   10000536                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 537   10000537                          POLLO PINULITO|||FALTANTE     76001
    ## 538   10000538            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 539   10000539 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 540   10000540 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 541   10000541                                     BAR LA OFICINA     76001
    ## 542   10000542                  POLLO PINULITO/Despacho a cliente     76001
    ## 543   10000543                EL GALLO NEGRO / Despacho a cliente     76002
    ## 544   10000544      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 545   10000545                          POLLO PINULITO|||FALTANTE     76002
    ## 546   10000546                                        UBIQUO LABS     76002
    ## 547   10000547                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 548   10000548                                     BAR LA OFICINA     76002
    ## 549   10000549            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 550   10000550                                     BAR LA OFICINA     76002
    ## 551   10000551                     EL PINCHE OBELISCO |||Faltante     76001
    ## 552   10000552                     EL PINCHE OBELISCO |||Faltante     76002
    ## 553   10000553                              HOSPITAL LAS AMERICAS     76002
    ## 554   10000554                EL GALLO NEGRO / Despacho a cliente     76001
    ## 555   10000555                                TAQUERIA EL CHINITO     76002
    ## 556   10000556 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 557   10000557                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 558   10000558            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 559   10000559           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 560   10000560                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 561   10000561        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 562   10000562        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 563   10000563            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 564   10000564                          POLLO PINULITO|||FALTANTE     76001
    ## 565   10000565                EL GALLO NEGRO / Despacho a cliente     76001
    ## 566   10000566                EL GALLO NEGRO / Despacho a cliente     76002
    ## 567   10000567                                TAQUERIA EL CHINITO     76001
    ## 568   10000568                  POLLO PINULITO/Despacho a cliente     76002
    ## 569   10000569                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 570   10000570 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 571   10000571                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 572   10000572                                TAQUERIA EL CHINITO     76001
    ## 573   10000573                EL GALLO NEGRO / Despacho a cliente     76001
    ## 574   10000574        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 575   10000575                                        UBIQUO LABS     76001
    ## 576   10000576                                        UBIQUO LABS     76001
    ## 577   10000577                                TAQUERIA EL CHINITO     76002
    ## 578   10000578 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 579   10000579                          POLLO PINULITO|||FALTANTE     76001
    ## 580   10000580                            UBIQUO LABS |||FALTANTE     76002
    ## 581   10000581      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 582   10000582      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 583   10000583                              HOSPITAL LAS AMERICAS     76002
    ## 584   10000584                            UBIQUO LABS |||FALTANTE     76002
    ## 585   10000585 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 586   10000586            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 587   10000587                     EL PINCHE OBELISCO |||Faltante     76002
    ## 588   10000588                                     BAR LA OFICINA     76001
    ## 589   10000589            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 590   10000590           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 591   10000591            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 592   10000592                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 593   10000593                            UBIQUO LABS |||FALTANTE     76001
    ## 594   10000594                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 595   10000595                                TAQUERIA EL CHINITO     76002
    ## 596   10000596                                        UBIQUO LABS     76001
    ## 597   10000597                  POLLO PINULITO/Despacho a cliente     76002
    ## 598   10000598                     EL PINCHE OBELISCO |||Faltante     76002
    ## 599   10000599                              HOSPITAL LAS AMERICAS     76002
    ## 600   10000600                  POLLO PINULITO/Despacho a cliente     76002
    ## 601   10000601                     EL PINCHE OBELISCO |||Faltante     76001
    ## 602   10000602      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 603   10000603                     EL PINCHE OBELISCO |||Faltante     76001
    ## 604   10000604                                        UBIQUO LABS     76001
    ## 605   10000605                            UBIQUO LABS |||FALTANTE     76001
    ## 606   10000606 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 607   10000607                                     BAR LA OFICINA     76001
    ## 608   10000608                              HOSPITAL LAS AMERICAS     76002
    ## 609   10000609                              HOSPITAL LAS AMERICAS     76001
    ## 610   10000610                                TAQUERIA EL CHINITO     76001
    ## 611   10000611            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 612   10000612        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 613   10000613                  POLLO PINULITO/Despacho a cliente     76001
    ## 614   10000614            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 615   10000615        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 616   10000616                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 617   10000617                            UBIQUO LABS |||FALTANTE     76002
    ## 618   10000618                                        UBIQUO LABS     76001
    ## 619   10000619                                     BAR LA OFICINA     76001
    ## 620   10000620                                     BAR LA OFICINA     76002
    ## 621   10000621                          POLLO PINULITO|||FALTANTE     76002
    ## 622   10000622                  POLLO PINULITO/Despacho a cliente     76002
    ## 623   10000623           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 624   10000624                                TAQUERIA EL CHINITO     76002
    ## 625   10000625                  POLLO PINULITO/Despacho a cliente     76002
    ## 626   10000626                            UBIQUO LABS |||FALTANTE     76001
    ## 627   10000627                                        UBIQUO LABS     76001
    ## 628   10000628      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 629   10000629            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 630   10000630 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 631   10000631        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 632   10000632 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 633   10000633                          POLLO PINULITO|||FALTANTE     76001
    ## 634   10000634                  POLLO PINULITO/Despacho a cliente     76002
    ## 635   10000635 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 636   10000636            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 637   10000637                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 638   10000638                  POLLO PINULITO/Despacho a cliente     76001
    ## 639   10000639 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 640   10000640            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 641   10000641                                     BAR LA OFICINA     76002
    ## 642   10000642                EL GALLO NEGRO / Despacho a cliente     76001
    ## 643   10000643                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 644   10000644                                TAQUERIA EL CHINITO     76001
    ## 645   10000645      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 646   10000646                                        UBIQUO LABS     76002
    ## 647   10000647                                     BAR LA OFICINA     76002
    ## 648   10000648                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 649   10000649            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 650   10000650                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 651   10000651                                        UBIQUO LABS     76002
    ## 652   10000652                            UBIQUO LABS |||FALTANTE     76001
    ## 653   10000653                                        UBIQUO LABS     76002
    ## 654   10000654            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 655   10000655            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 656   10000656                  POLLO PINULITO/Despacho a cliente     76002
    ## 657   10000657            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 658   10000658                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 659   10000659            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 660   10000660            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 661   10000661        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 662   10000662            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 663   10000663                EL GALLO NEGRO / Despacho a cliente     76002
    ## 664   10000664                                TAQUERIA EL CHINITO     76002
    ## 665   10000665                                        UBIQUO LABS     76002
    ## 666   10000666        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 667   10000667                EL GALLO NEGRO / Despacho a cliente     76001
    ## 668   10000668                                        UBIQUO LABS     76002
    ## 669   10000669 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 670   10000670                     EL PINCHE OBELISCO |||Faltante     76002
    ## 671   10000671                     EL PINCHE OBELISCO |||Faltante     76002
    ## 672   10000672                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 673   10000673                                TAQUERIA EL CHINITO     76002
    ## 674   10000674 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 675   10000675                     EL PINCHE OBELISCO |||Faltante     76002
    ## 676   10000676        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 677   10000677                              HOSPITAL LAS AMERICAS     76002
    ## 678   10000678                EL GALLO NEGRO / Despacho a cliente     76002
    ## 679   10000679 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 680   10000680            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 681   10000681                                        UBIQUO LABS     76001
    ## 682   10000682                                        UBIQUO LABS     76002
    ## 683   10000683      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 684   10000684            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 685   10000685                     EL PINCHE OBELISCO |||Faltante     76001
    ## 686   10000686                  POLLO PINULITO/Despacho a cliente     76001
    ## 687   10000687                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 688   10000688            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 689   10000689        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 690   10000690                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 691   10000691                EL GALLO NEGRO / Despacho a cliente     76002
    ## 692   10000692                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 693   10000693                                        UBIQUO LABS     76002
    ## 694   10000694                EL GALLO NEGRO / Despacho a cliente     76002
    ## 695   10000695            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 696   10000696                     EL PINCHE OBELISCO |||Faltante     76001
    ## 697   10000697            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 698   10000698            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 699   10000699            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 700   10000700            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 701   10000701            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 702   10000702                  POLLO PINULITO/Despacho a cliente     76002
    ## 703   10000703                  POLLO PINULITO/Despacho a cliente     76002
    ## 704   10000704 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 705   10000705                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 706   10000706                                     BAR LA OFICINA     76002
    ## 707   10000707                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 708   10000708                EL GALLO NEGRO / Despacho a cliente     76002
    ## 709   10000709                  POLLO PINULITO/Despacho a cliente     76001
    ## 710   10000710                                     BAR LA OFICINA     76001
    ## 711   10000711                                        UBIQUO LABS     76001
    ## 712   10000712            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 713   10000713                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 714   10000714                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 715   10000715      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 716   10000716      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 717   10000717                                     BAR LA OFICINA     76002
    ## 718   10000718        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 719   10000719           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 720   10000720                                     BAR LA OFICINA     76002
    ## 721   10000721      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 722   10000722        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 723   10000723           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 724   10000724 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 725   10000725                          POLLO PINULITO|||FALTANTE     76002
    ## 726   10000726                                TAQUERIA EL CHINITO     76001
    ## 727   10000727        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 728   10000728                                        UBIQUO LABS     76002
    ## 729   10000729      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 730   10000730                                        UBIQUO LABS     76001
    ## 731   10000731                     EL PINCHE OBELISCO |||Faltante     76001
    ## 732   10000732                                        UBIQUO LABS     76001
    ## 733   10000733                EL GALLO NEGRO / Despacho a cliente     76001
    ## 734   10000734                                        UBIQUO LABS     76001
    ## 735   10000735                  POLLO PINULITO/Despacho a cliente     76002
    ## 736   10000736                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 737   10000737        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 738   10000738                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 739   10000739            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 740   10000740            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 741   10000741      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 742   10000742 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 743   10000743                          POLLO PINULITO|||FALTANTE     76001
    ## 744   10000744                            UBIQUO LABS |||FALTANTE     76001
    ## 745   10000745            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 746   10000746                                     BAR LA OFICINA     76002
    ## 747   10000747            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 748   10000748 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 749   10000749                              HOSPITAL LAS AMERICAS     76002
    ## 750   10000750                            UBIQUO LABS |||FALTANTE     76001
    ## 751   10000751                  POLLO PINULITO/Despacho a cliente     76001
    ## 752   10000752      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 753   10000753 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 754   10000754                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 755   10000755                                TAQUERIA EL CHINITO     76002
    ## 756   10000756                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 757   10000757                                     BAR LA OFICINA     76002
    ## 758   10000758            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 759   10000759                            UBIQUO LABS |||FALTANTE     76002
    ## 760   10000760                EL GALLO NEGRO / Despacho a cliente     76002
    ## 761   10000761                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 762   10000762            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 763   10000763                                     BAR LA OFICINA     76002
    ## 764   10000764                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 765   10000765 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 766   10000766                EL GALLO NEGRO / Despacho a cliente     76002
    ## 767   10000767                     EL PINCHE OBELISCO |||Faltante     76001
    ## 768   10000768 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 769   10000769                     EL PINCHE OBELISCO |||Faltante     76002
    ## 770   10000770            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 771   10000771                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 772   10000772        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 773   10000773                                     BAR LA OFICINA     76002
    ## 774   10000774                                     BAR LA OFICINA     76001
    ## 775   10000775                     EL PINCHE OBELISCO |||Faltante     76001
    ## 776   10000776            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 777   10000777                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 778   10000778      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 779   10000779        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 780   10000780                EL GALLO NEGRO / Despacho a cliente     76002
    ## 781   10000781                                        UBIQUO LABS     76001
    ## 782   10000782                            UBIQUO LABS |||FALTANTE     76001
    ## 783   10000783                  POLLO PINULITO/Despacho a cliente     76002
    ## 784   10000784            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 785   10000785                EL GALLO NEGRO / Despacho a cliente     76001
    ## 786   10000786                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 787   10000787        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 788   10000788                              HOSPITAL LAS AMERICAS     76002
    ## 789   10000789                            UBIQUO LABS |||FALTANTE     76001
    ## 790   10000790                  POLLO PINULITO/Despacho a cliente     76002
    ## 791   10000791 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 792   10000792                              HOSPITAL LAS AMERICAS     76001
    ## 793   10000793           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 794   10000794                            UBIQUO LABS |||FALTANTE     76001
    ## 795   10000795                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 796   10000796                              HOSPITAL LAS AMERICAS     76002
    ## 797   10000797                              HOSPITAL LAS AMERICAS     76001
    ## 798   10000798           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 799   10000799                EL GALLO NEGRO / Despacho a cliente     76002
    ## 800   10000800                EL GALLO NEGRO / Despacho a cliente     76002
    ## 801   10000801                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 802   10000802                EL GALLO NEGRO / Despacho a cliente     76002
    ## 803   10000803           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 804   10000804                                TAQUERIA EL CHINITO     76002
    ## 805   10000805 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 806   10000806                            UBIQUO LABS |||FALTANTE     76002
    ## 807   10000807                              HOSPITAL LAS AMERICAS     76002
    ## 808   10000808           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 809   10000809                EL GALLO NEGRO / Despacho a cliente     76001
    ## 810   10000810            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 811   10000811                                TAQUERIA EL CHINITO     76001
    ## 812   10000812                     EL PINCHE OBELISCO |||Faltante     76001
    ## 813   10000813                                     BAR LA OFICINA     76002
    ## 814   10000814           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 815   10000815                EL GALLO NEGRO / Despacho a cliente     76002
    ## 816   10000816                  POLLO PINULITO/Despacho a cliente     76002
    ## 817   10000817                EL GALLO NEGRO / Despacho a cliente     76001
    ## 818   10000818      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 819   10000819        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 820   10000820                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 821   10000821                     EL PINCHE OBELISCO |||Faltante     76001
    ## 822   10000822           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 823   10000823                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 824   10000824                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 825   10000825                                TAQUERIA EL CHINITO     76002
    ## 826   10000826 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 827   10000827           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 828   10000828                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 829   10000829                  POLLO PINULITO/Despacho a cliente     76002
    ## 830   10000830            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 831   10000831                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 832   10000832      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 833   10000833                     EL PINCHE OBELISCO |||Faltante     76001
    ## 834   10000834                            UBIQUO LABS |||FALTANTE     76001
    ## 835   10000835      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 836   10000836           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 837   10000837                     EL PINCHE OBELISCO |||Faltante     76001
    ## 838   10000838                                TAQUERIA EL CHINITO     76001
    ## 839   10000839                EL GALLO NEGRO / Despacho a cliente     76001
    ## 840   10000840      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 841   10000841                                     BAR LA OFICINA     76001
    ## 842   10000842                            UBIQUO LABS |||FALTANTE     76002
    ## 843   10000843                                        UBIQUO LABS     76002
    ## 844   10000844            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 845   10000845        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 846   10000846                     EL PINCHE OBELISCO |||Faltante     76001
    ## 847   10000847                     EL PINCHE OBELISCO |||Faltante     76001
    ## 848   10000848                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 849   10000849           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 850   10000850      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 851   10000851           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 852   10000852                                        UBIQUO LABS     76001
    ## 853   10000853                              HOSPITAL LAS AMERICAS     76002
    ## 854   10000854            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 855   10000855 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 856   10000856      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 857   10000857        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 858   10000858            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 859   10000859                          POLLO PINULITO|||FALTANTE     76001
    ## 860   10000860      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 861   10000861                     EL PINCHE OBELISCO |||Faltante     76002
    ## 862   10000862        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 863   10000863                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 864   10000864                     EL PINCHE OBELISCO |||Faltante     76002
    ## 865   10000865                EL GALLO NEGRO / Despacho a cliente     76001
    ## 866   10000866 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 867   10000867                EL GALLO NEGRO / Despacho a cliente     76001
    ## 868   10000868           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 869   10000869            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 870   10000870        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 871   10000871                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 872   10000872            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 873   10000873           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 874   10000874                                TAQUERIA EL CHINITO     76002
    ## 875   10000875                                        UBIQUO LABS     76001
    ## 876   10000876                                        UBIQUO LABS     76002
    ## 877   10000877                          POLLO PINULITO|||FALTANTE     76001
    ## 878   10000878                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 879   10000879                            UBIQUO LABS |||FALTANTE     76001
    ## 880   10000880            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 881   10000881                                        UBIQUO LABS     76001
    ## 882   10000882                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 883   10000883                                        UBIQUO LABS     76002
    ## 884   10000884                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 885   10000885 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 886   10000886                                        UBIQUO LABS     76002
    ## 887   10000887                                     BAR LA OFICINA     76001
    ## 888   10000888                          POLLO PINULITO|||FALTANTE     76002
    ## 889   10000889                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 890   10000890            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 891   10000891                                TAQUERIA EL CHINITO     76001
    ## 892   10000892                                TAQUERIA EL CHINITO     76002
    ## 893   10000893                          POLLO PINULITO|||FALTANTE     76002
    ## 894   10000894            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 895   10000895                                TAQUERIA EL CHINITO     76001
    ## 896   10000896        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 897   10000897                            UBIQUO LABS |||FALTANTE     76002
    ## 898   10000898                            UBIQUO LABS |||FALTANTE     76001
    ## 899   10000899                     EL PINCHE OBELISCO |||Faltante     76002
    ## 900   10000900 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 901   10000901      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 902   10000902 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 903   10000903      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 904   10000904                            UBIQUO LABS |||FALTANTE     76002
    ## 905   10000905 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 906   10000906                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 907   10000907            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 908   10000908                                TAQUERIA EL CHINITO     76002
    ## 909   10000909            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 910   10000910                EL GALLO NEGRO / Despacho a cliente     76002
    ## 911   10000911                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 912   10000912                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 913   10000913                                     BAR LA OFICINA     76002
    ## 914   10000914                              HOSPITAL LAS AMERICAS     76001
    ## 915   10000915                                TAQUERIA EL CHINITO     76001
    ## 916   10000916                  POLLO PINULITO/Despacho a cliente     76002
    ## 917   10000917                  POLLO PINULITO/Despacho a cliente     76001
    ## 918   10000918 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 919   10000919                                TAQUERIA EL CHINITO     76001
    ## 920   10000920                          POLLO PINULITO|||FALTANTE     76002
    ## 921   10000921 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 922   10000922      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 923   10000923                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 924   10000924                                     BAR LA OFICINA     76002
    ## 925   10000925            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 926   10000926           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 927   10000927           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 928   10000928            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 929   10000929                  POLLO PINULITO/Despacho a cliente     76002
    ## 930   10000930                                        UBIQUO LABS     76002
    ## 931   10000931           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 932   10000932            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 933   10000933                                TAQUERIA EL CHINITO     76002
    ## 934   10000934                     EL PINCHE OBELISCO |||Faltante     76001
    ## 935   10000935            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 936   10000936                     EL PINCHE OBELISCO |||Faltante     76002
    ## 937   10000937            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 938   10000938                              HOSPITAL LAS AMERICAS     76001
    ## 939   10000939        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 940   10000940                                     BAR LA OFICINA     76002
    ## 941   10000941 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 942   10000942                                     BAR LA OFICINA     76001
    ## 943   10000943                                TAQUERIA EL CHINITO     76001
    ## 944   10000944            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 945   10000945 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 946   10000946 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 947   10000947            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 948   10000948                                TAQUERIA EL CHINITO     76001
    ## 949   10000949                              HOSPITAL LAS AMERICAS     76001
    ## 950   10000950                                        UBIQUO LABS     76001
    ## 951   10000951      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 952   10000952                              HOSPITAL LAS AMERICAS     76001
    ## 953   10000953                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 954   10000954           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 955   10000955                                TAQUERIA EL CHINITO     76002
    ## 956   10000956                  POLLO PINULITO/Despacho a cliente     76001
    ## 957   10000957 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 958   10000958                                TAQUERIA EL CHINITO     76002
    ## 959   10000959        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 960   10000960                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 961   10000961            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 962   10000962                            UBIQUO LABS |||FALTANTE     76002
    ## 963   10000963                                        UBIQUO LABS     76002
    ## 964   10000964                                        UBIQUO LABS     76002
    ## 965   10000965                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 966   10000966 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 967   10000967                                     BAR LA OFICINA     76002
    ## 968   10000968                              HOSPITAL LAS AMERICAS     76002
    ## 969   10000969                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 970   10000970                            UBIQUO LABS |||FALTANTE     76001
    ## 971   10000971                     EL PINCHE OBELISCO |||Faltante     76002
    ## 972   10000972 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 973   10000973                EL GALLO NEGRO / Despacho a cliente     76001
    ## 974   10000974 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 975   10000975                                TAQUERIA EL CHINITO     76002
    ## 976   10000976           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 977   10000977                EL GALLO NEGRO / Despacho a cliente     76001
    ## 978   10000978                                     BAR LA OFICINA     76001
    ## 979   10000979                          POLLO PINULITO|||FALTANTE     76002
    ## 980   10000980                                TAQUERIA EL CHINITO     76002
    ## 981   10000981                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 982   10000982                                        UBIQUO LABS     76002
    ## 983   10000983            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 984   10000984                                        UBIQUO LABS     76001
    ## 985   10000985                  POLLO PINULITO/Despacho a cliente     76002
    ## 986   10000986                          POLLO PINULITO|||FALTANTE     76002
    ## 987   10000987 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 988   10000988                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 989   10000989        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 990   10000990                          POLLO PINULITO|||FALTANTE     76001
    ## 991   10000991                                TAQUERIA EL CHINITO     76002
    ## 992   10000992            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 993   10000993            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 994   10000994                          POLLO PINULITO|||FALTANTE     76001
    ## 995   10000995      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 996   10000996                  POLLO PINULITO/Despacho a cliente     76001
    ## 997   10000997                  POLLO PINULITO/Despacho a cliente     76002
    ## 998   10000998                                TAQUERIA EL CHINITO     76001
    ## 999   10000999                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1000  10001000                EL GALLO NEGRO / Despacho a cliente     76001
    ## 1001  10001001                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 1002  10001002           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1003  10001003            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 1004  10001004                              HOSPITAL LAS AMERICAS     76001
    ## 1005  10001005           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1006  10001006                            UBIQUO LABS |||FALTANTE     76002
    ## 1007  10001007                                     BAR LA OFICINA     76001
    ## 1008  10001008                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 1009  10001009            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1010  10001010            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1011  10001011           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 1012  10001012                                     BAR LA OFICINA     76001
    ## 1013  10001013                                TAQUERIA EL CHINITO     76001
    ## 1014  10001014            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1015  10001015                              HOSPITAL LAS AMERICAS     76002
    ## 1016  10001016                            UBIQUO LABS |||FALTANTE     76001
    ## 1017  10001017        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 1018  10001018                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1019  10001019            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 1020  10001020           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1021  10001021                          POLLO PINULITO|||FALTANTE     76002
    ## 1022  10001022                EL GALLO NEGRO / Despacho a cliente     76001
    ## 1023  10001023            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1024  10001024           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1025  10001025                          POLLO PINULITO|||FALTANTE     76002
    ## 1026  10001026                                        UBIQUO LABS     76001
    ## 1027  10001027            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 1028  10001028                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 1029  10001029                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1030  10001030           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1031  10001031        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 1032  10001032                  POLLO PINULITO/Despacho a cliente     76002
    ## 1033  10001033                          POLLO PINULITO|||FALTANTE     76001
    ## 1034  10001034            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1035  10001035                            UBIQUO LABS |||FALTANTE     76002
    ## 1036  10001036                                TAQUERIA EL CHINITO     76001
    ## 1037  10001037        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1038  10001038        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1039  10001039            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1040  10001040                                        UBIQUO LABS     76001
    ## 1041  10001041                EL GALLO NEGRO / Despacho a cliente     76002
    ## 1042  10001042                                     BAR LA OFICINA     76001
    ## 1043  10001043            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1044  10001044                          POLLO PINULITO|||FALTANTE     76001
    ## 1045  10001045                EL GALLO NEGRO / Despacho a cliente     76002
    ## 1046  10001046                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 1047  10001047                          POLLO PINULITO|||FALTANTE     76001
    ## 1048  10001048                EL GALLO NEGRO / Despacho a cliente     76002
    ## 1049  10001049            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 1050  10001050                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 1051  10001051                          POLLO PINULITO|||FALTANTE     76001
    ## 1052  10001052            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 1053  10001053                              HOSPITAL LAS AMERICAS     76002
    ## 1054  10001054                EL GALLO NEGRO / Despacho a cliente     76002
    ## 1055  10001055                            UBIQUO LABS |||FALTANTE     76002
    ## 1056  10001056                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1057  10001057            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 1058  10001058                          POLLO PINULITO|||FALTANTE     76001
    ## 1059  10001059        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1060  10001060           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1061  10001061                     EL PINCHE OBELISCO |||Faltante     76002
    ## 1062  10001062                            UBIQUO LABS |||FALTANTE     76001
    ## 1063  10001063                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1064  10001064      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 1065  10001065                            UBIQUO LABS |||FALTANTE     76001
    ## 1066  10001066 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1067  10001067                                     BAR LA OFICINA     76002
    ## 1068  10001068           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1069  10001069            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 1070  10001070                                        UBIQUO LABS     76001
    ## 1071  10001071        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1072  10001072                  POLLO PINULITO/Despacho a cliente     76002
    ## 1073  10001073            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1074  10001074                            UBIQUO LABS |||FALTANTE     76002
    ## 1075  10001075           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 1076  10001076                              HOSPITAL LAS AMERICAS     76002
    ## 1077  10001077                  POLLO PINULITO/Despacho a cliente     76002
    ## 1078  10001078                          POLLO PINULITO|||FALTANTE     76002
    ## 1079  10001079                                     BAR LA OFICINA     76002
    ## 1080  10001080            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 1081  10001081 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 1082  10001082                                TAQUERIA EL CHINITO     76002
    ## 1083  10001083                  POLLO PINULITO/Despacho a cliente     76001
    ## 1084  10001084                                     BAR LA OFICINA     76002
    ## 1085  10001085      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1086  10001086                                TAQUERIA EL CHINITO     76001
    ## 1087  10001087                                        UBIQUO LABS     76002
    ## 1088  10001088                                        UBIQUO LABS     76002
    ## 1089  10001089                                     BAR LA OFICINA     76001
    ## 1090  10001090                                     BAR LA OFICINA     76001
    ## 1091  10001091            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 1092  10001092                                        UBIQUO LABS     76002
    ## 1093  10001093                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 1094  10001094                              HOSPITAL LAS AMERICAS     76001
    ## 1095  10001095                                     BAR LA OFICINA     76001
    ## 1096  10001096 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1097  10001097        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1098  10001098      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1099  10001099                            UBIQUO LABS |||FALTANTE     76002
    ## 1100  10001100 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 1101  10001101            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 1102  10001102      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1103  10001103                     EL PINCHE OBELISCO |||Faltante     76002
    ## 1104  10001104           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1105  10001105            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 1106  10001106                                TAQUERIA EL CHINITO     76001
    ## 1107  10001107                                        UBIQUO LABS     76001
    ## 1108  10001108                EL GALLO NEGRO / Despacho a cliente     76001
    ## 1109  10001109            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1110  10001110                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 1111  10001111                          POLLO PINULITO|||FALTANTE     76001
    ## 1112  10001112                          POLLO PINULITO|||FALTANTE     76002
    ## 1113  10001113                                TAQUERIA EL CHINITO     76002
    ## 1114  10001114                     EL PINCHE OBELISCO |||Faltante     76002
    ## 1115  10001115           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1116  10001116                          POLLO PINULITO|||FALTANTE     76002
    ## 1117  10001117                              HOSPITAL LAS AMERICAS     76001
    ## 1118  10001118                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 1119  10001119           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1120  10001120                          POLLO PINULITO|||FALTANTE     76002
    ## 1121  10001121            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1122  10001122                  POLLO PINULITO/Despacho a cliente     76002
    ## 1123  10001123                  POLLO PINULITO/Despacho a cliente     76001
    ## 1124  10001124                  POLLO PINULITO/Despacho a cliente     76002
    ## 1125  10001125                                TAQUERIA EL CHINITO     76001
    ## 1126  10001126                                        UBIQUO LABS     76002
    ## 1127  10001127                EL GALLO NEGRO / Despacho a cliente     76001
    ## 1128  10001128        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 1129  10001129                                TAQUERIA EL CHINITO     76001
    ## 1130  10001130                                        UBIQUO LABS     76002
    ## 1131  10001131                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1132  10001132                                TAQUERIA EL CHINITO     76001
    ## 1133  10001133                          POLLO PINULITO|||FALTANTE     76001
    ## 1134  10001134                                TAQUERIA EL CHINITO     76002
    ## 1135  10001135        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 1136  10001136        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 1137  10001137      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 1138  10001138            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 1139  10001139        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1140  10001140                              HOSPITAL LAS AMERICAS     76001
    ## 1141  10001141                                     BAR LA OFICINA     76002
    ## 1142  10001142                                TAQUERIA EL CHINITO     76001
    ## 1143  10001143                                        UBIQUO LABS     76001
    ## 1144  10001144                EL GALLO NEGRO / Despacho a cliente     76001
    ## 1145  10001145        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 1146  10001146            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1147  10001147 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1148  10001148                                TAQUERIA EL CHINITO     76002
    ## 1149  10001149 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 1150  10001150            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1151  10001151                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 1152  10001152      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 1153  10001153                  POLLO PINULITO/Despacho a cliente     76001
    ## 1154  10001154                            UBIQUO LABS |||FALTANTE     76001
    ## 1155  10001155                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1156  10001156                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1157  10001157                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1158  10001158           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 1159  10001159                     EL PINCHE OBELISCO |||Faltante     76002
    ## 1160  10001160                EL GALLO NEGRO / Despacho a cliente     76001
    ## 1161  10001161                EL GALLO NEGRO / Despacho a cliente     76002
    ## 1162  10001162                              HOSPITAL LAS AMERICAS     76001
    ## 1163  10001163      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1164  10001164                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 1165  10001165            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1166  10001166            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 1167  10001167      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1168  10001168            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1169  10001169                                     BAR LA OFICINA     76001
    ## 1170  10001170                          POLLO PINULITO|||FALTANTE     76001
    ## 1171  10001171                                        UBIQUO LABS     76001
    ## 1172  10001172                                        UBIQUO LABS     76002
    ## 1173  10001173                                        UBIQUO LABS     76001
    ## 1174  10001174                              HOSPITAL LAS AMERICAS     76002
    ## 1175  10001175                     EL PINCHE OBELISCO |||Faltante     76002
    ## 1176  10001176                                     BAR LA OFICINA     76001
    ## 1177  10001177                            UBIQUO LABS |||FALTANTE     76002
    ## 1178  10001178                     EL PINCHE OBELISCO |||Faltante     76002
    ## 1179  10001179 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 1180  10001180                            UBIQUO LABS |||FALTANTE     76001
    ## 1181  10001181            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 1182  10001182                              HOSPITAL LAS AMERICAS     76001
    ## 1183  10001183            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1184  10001184                  POLLO PINULITO/Despacho a cliente     76001
    ## 1185  10001185 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1186  10001186                EL GALLO NEGRO / Despacho a cliente     76001
    ## 1187  10001187           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 1188  10001188        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1189  10001189           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 1190  10001190                                     BAR LA OFICINA     76002
    ## 1191  10001191        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1192  10001192                                        UBIQUO LABS     76002
    ## 1193  10001193      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 1194  10001194            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1195  10001195 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1196  10001196                                TAQUERIA EL CHINITO     76001
    ## 1197  10001197            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1198  10001198            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1199  10001199                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 1200  10001200                              HOSPITAL LAS AMERICAS     76001
    ## 1201  10001201                            UBIQUO LABS |||FALTANTE     76001
    ## 1202  10001202 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1203  10001203                                TAQUERIA EL CHINITO     76002
    ## 1204  10001204            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1205  10001205                            UBIQUO LABS |||FALTANTE     76001
    ## 1206  10001206                     EL PINCHE OBELISCO |||Faltante     76002
    ## 1207  10001207            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1208  10001208                  POLLO PINULITO/Despacho a cliente     76001
    ## 1209  10001209                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 1210  10001210                EL GALLO NEGRO / Despacho a cliente     76002
    ## 1211  10001211            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 1212  10001212                              HOSPITAL LAS AMERICAS     76002
    ## 1213  10001213                                TAQUERIA EL CHINITO     76001
    ## 1214  10001214                          POLLO PINULITO|||FALTANTE     76001
    ## 1215  10001215 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1216  10001216            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1217  10001217           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 1218  10001218                  POLLO PINULITO/Despacho a cliente     76002
    ## 1219  10001219                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 1220  10001220                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1221  10001221                          POLLO PINULITO|||FALTANTE     76001
    ## 1222  10001222                                     BAR LA OFICINA     76002
    ## 1223  10001223                            UBIQUO LABS |||FALTANTE     76002
    ## 1224  10001224                              HOSPITAL LAS AMERICAS     76001
    ## 1225  10001225           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 1226  10001226        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1227  10001227                     EL PINCHE OBELISCO |||Faltante     76002
    ## 1228  10001228      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1229  10001229           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 1230  10001230            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1231  10001231                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 1232  10001232      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 1233  10001233        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 1234  10001234           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 1235  10001235                  POLLO PINULITO/Despacho a cliente     76001
    ## 1236  10001236                            UBIQUO LABS |||FALTANTE     76002
    ## 1237  10001237                                TAQUERIA EL CHINITO     76001
    ## 1238  10001238                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1239  10001239                  POLLO PINULITO/Despacho a cliente     76002
    ## 1240  10001240                EL GALLO NEGRO / Despacho a cliente     76002
    ## 1241  10001241                                        UBIQUO LABS     76001
    ## 1242  10001242                          POLLO PINULITO|||FALTANTE     76001
    ## 1243  10001243            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1244  10001244 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1245  10001245            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1246  10001246                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 1247  10001247                          POLLO PINULITO|||FALTANTE     76002
    ## 1248  10001248                EL GALLO NEGRO / Despacho a cliente     76002
    ## 1249  10001249            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1250  10001250                            UBIQUO LABS |||FALTANTE     76001
    ## 1251  10001251                     EL PINCHE OBELISCO |||Faltante     76002
    ## 1252  10001252            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1253  10001253                                     BAR LA OFICINA     76001
    ## 1254  10001254                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1255  10001255 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1256  10001256                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 1257  10001257            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1258  10001258                              HOSPITAL LAS AMERICAS     76001
    ## 1259  10001259                          POLLO PINULITO|||FALTANTE     76001
    ## 1260  10001260           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1261  10001261                                TAQUERIA EL CHINITO     76002
    ## 1262  10001262                                     BAR LA OFICINA     76001
    ## 1263  10001263                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1264  10001264                  POLLO PINULITO/Despacho a cliente     76002
    ## 1265  10001265        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 1266  10001266            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1267  10001267                                     BAR LA OFICINA     76002
    ## 1268  10001268                            UBIQUO LABS |||FALTANTE     76002
    ## 1269  10001269                EL GALLO NEGRO / Despacho a cliente     76001
    ## 1270  10001270            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1271  10001271                            UBIQUO LABS |||FALTANTE     76002
    ## 1272  10001272                            UBIQUO LABS |||FALTANTE     76002
    ## 1273  10001273        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1274  10001274                     EL PINCHE OBELISCO |||Faltante     76002
    ## 1275  10001275        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1276  10001276                            UBIQUO LABS |||FALTANTE     76001
    ## 1277  10001277                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 1278  10001278                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1279  10001279                  POLLO PINULITO/Despacho a cliente     76001
    ## 1280  10001280                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1281  10001281                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 1282  10001282                                        UBIQUO LABS     76002
    ## 1283  10001283                                TAQUERIA EL CHINITO     76002
    ## 1284  10001284            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 1285  10001285 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1286  10001286                EL GALLO NEGRO / Despacho a cliente     76001
    ## 1287  10001287        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 1288  10001288 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 1289  10001289                              HOSPITAL LAS AMERICAS     76001
    ## 1290  10001290            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 1291  10001291                                TAQUERIA EL CHINITO     76001
    ## 1292  10001292                              HOSPITAL LAS AMERICAS     76002
    ## 1293  10001293                            UBIQUO LABS |||FALTANTE     76001
    ## 1294  10001294                  POLLO PINULITO/Despacho a cliente     76002
    ## 1295  10001295      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1296  10001296                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1297  10001297            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1298  10001298            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 1299  10001299 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 1300  10001300                          POLLO PINULITO|||FALTANTE     76002
    ## 1301  10001301                          POLLO PINULITO|||FALTANTE     76002
    ## 1302  10001302                                TAQUERIA EL CHINITO     76002
    ## 1303  10001303           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1304  10001304                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1305  10001305                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 1306  10001306                  POLLO PINULITO/Despacho a cliente     76001
    ## 1307  10001307                                        UBIQUO LABS     76001
    ## 1308  10001308                  POLLO PINULITO/Despacho a cliente     76002
    ## 1309  10001309           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 1310  10001310      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1311  10001311           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1312  10001312            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1313  10001313                EL GALLO NEGRO / Despacho a cliente     76001
    ## 1314  10001314            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 1315  10001315                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 1316  10001316                  POLLO PINULITO/Despacho a cliente     76002
    ## 1317  10001317                              HOSPITAL LAS AMERICAS     76001
    ## 1318  10001318           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 1319  10001319                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1320  10001320                     EL PINCHE OBELISCO |||Faltante     76002
    ## 1321  10001321            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1322  10001322                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1323  10001323        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 1324  10001324                                        UBIQUO LABS     76002
    ## 1325  10001325            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1326  10001326                            UBIQUO LABS |||FALTANTE     76002
    ## 1327  10001327                                     BAR LA OFICINA     76001
    ## 1328  10001328                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 1329  10001329                  POLLO PINULITO/Despacho a cliente     76002
    ## 1330  10001330                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 1331  10001331 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 1332  10001332                                     BAR LA OFICINA     76001
    ## 1333  10001333           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1334  10001334            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 1335  10001335                                        UBIQUO LABS     76001
    ## 1336  10001336                EL GALLO NEGRO / Despacho a cliente     76002
    ## 1337  10001337                                TAQUERIA EL CHINITO     76002
    ## 1338  10001338           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1339  10001339                          POLLO PINULITO|||FALTANTE     76002
    ## 1340  10001340                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 1341  10001341           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1342  10001342                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1343  10001343                  POLLO PINULITO/Despacho a cliente     76002
    ## 1344  10001344                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1345  10001345                            UBIQUO LABS |||FALTANTE     76002
    ## 1346  10001346                                     BAR LA OFICINA     76001
    ## 1347  10001347            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 1348  10001348        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1349  10001349                EL GALLO NEGRO / Despacho a cliente     76001
    ## 1350  10001350      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1351  10001351                                     BAR LA OFICINA     76001
    ## 1352  10001352                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 1353  10001353      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 1354  10001354      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 1355  10001355                  POLLO PINULITO/Despacho a cliente     76001
    ## 1356  10001356                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1357  10001357                                     BAR LA OFICINA     76001
    ## 1358  10001358                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1359  10001359                              HOSPITAL LAS AMERICAS     76002
    ## 1360  10001360                     EL PINCHE OBELISCO |||Faltante     76002
    ## 1361  10001361                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 1362  10001362                     EL PINCHE OBELISCO |||Faltante     76002
    ## 1363  10001363      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1364  10001364 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1365  10001365 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1366  10001366                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1367  10001367            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 1368  10001368                  POLLO PINULITO/Despacho a cliente     76001
    ## 1369  10001369                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 1370  10001370                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1371  10001371                EL GALLO NEGRO / Despacho a cliente     76002
    ## 1372  10001372            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 1373  10001373 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1374  10001374            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1375  10001375            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 1376  10001376                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 1377  10001377                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 1378  10001378                                     BAR LA OFICINA     76002
    ## 1379  10001379                          POLLO PINULITO|||FALTANTE     76002
    ## 1380  10001380            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1381  10001381                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1382  10001382        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 1383  10001383                                        UBIQUO LABS     76001
    ## 1384  10001384                            UBIQUO LABS |||FALTANTE     76001
    ## 1385  10001385                                     BAR LA OFICINA     76001
    ## 1386  10001386                                        UBIQUO LABS     76002
    ## 1387  10001387            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 1388  10001388 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1389  10001389                                TAQUERIA EL CHINITO     76002
    ## 1390  10001390            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 1391  10001391            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1392  10001392           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1393  10001393                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 1394  10001394                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 1395  10001395                          POLLO PINULITO|||FALTANTE     76001
    ## 1396  10001396      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1397  10001397                EL GALLO NEGRO / Despacho a cliente     76001
    ## 1398  10001398           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1399  10001399        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1400  10001400                            UBIQUO LABS |||FALTANTE     76002
    ## 1401  10001401 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1402  10001402           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1403  10001403                  POLLO PINULITO/Despacho a cliente     76001
    ## 1404  10001404                                     BAR LA OFICINA     76002
    ## 1405  10001405                                TAQUERIA EL CHINITO     76001
    ## 1406  10001406                  POLLO PINULITO/Despacho a cliente     76001
    ## 1407  10001407                              HOSPITAL LAS AMERICAS     76001
    ## 1408  10001408                EL GALLO NEGRO / Despacho a cliente     76001
    ## 1409  10001409           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 1410  10001410                                        UBIQUO LABS     76002
    ## 1411  10001411      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1412  10001412                  POLLO PINULITO/Despacho a cliente     76002
    ## 1413  10001413                     EL PINCHE OBELISCO |||Faltante     76002
    ## 1414  10001414                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 1415  10001415      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 1416  10001416                EL GALLO NEGRO / Despacho a cliente     76001
    ## 1417  10001417           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1418  10001418                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 1419  10001419        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1420  10001420                                        UBIQUO LABS     76002
    ## 1421  10001421                          POLLO PINULITO|||FALTANTE     76002
    ## 1422  10001422           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1423  10001423                          POLLO PINULITO|||FALTANTE     76002
    ## 1424  10001424                          POLLO PINULITO|||FALTANTE     76002
    ## 1425  10001425           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1426  10001426                          POLLO PINULITO|||FALTANTE     76001
    ## 1427  10001427            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 1428  10001428                                     BAR LA OFICINA     76001
    ## 1429  10001429                  POLLO PINULITO/Despacho a cliente     76001
    ## 1430  10001430                                     BAR LA OFICINA     76001
    ## 1431  10001431                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 1432  10001432                          POLLO PINULITO|||FALTANTE     76002
    ## 1433  10001433                            UBIQUO LABS |||FALTANTE     76002
    ## 1434  10001434                  POLLO PINULITO/Despacho a cliente     76001
    ## 1435  10001435                  POLLO PINULITO/Despacho a cliente     76001
    ## 1436  10001436                                TAQUERIA EL CHINITO     76002
    ## 1437  10001437      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1438  10001438                          POLLO PINULITO|||FALTANTE     76001
    ## 1439  10001439                  POLLO PINULITO/Despacho a cliente     76002
    ## 1440  10001440                          POLLO PINULITO|||FALTANTE     76002
    ## 1441  10001441                            UBIQUO LABS |||FALTANTE     76002
    ## 1442  10001442                  POLLO PINULITO/Despacho a cliente     76001
    ## 1443  10001443           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1444  10001444 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1445  10001445                EL GALLO NEGRO / Despacho a cliente     76002
    ## 1446  10001446                            UBIQUO LABS |||FALTANTE     76002
    ## 1447  10001447            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 1448  10001448        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1449  10001449            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1450  10001450            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1451  10001451                                        UBIQUO LABS     76001
    ## 1452  10001452 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 1453  10001453                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 1454  10001454                              HOSPITAL LAS AMERICAS     76002
    ## 1455  10001455                                     BAR LA OFICINA     76002
    ## 1456  10001456                          POLLO PINULITO|||FALTANTE     76002
    ## 1457  10001457            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 1458  10001458                            UBIQUO LABS |||FALTANTE     76001
    ## 1459  10001459      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 1460  10001460            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1461  10001461                EL GALLO NEGRO / Despacho a cliente     76002
    ## 1462  10001462 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1463  10001463            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 1464  10001464                              HOSPITAL LAS AMERICAS     76002
    ## 1465  10001465                EL GALLO NEGRO / Despacho a cliente     76002
    ## 1466  10001466                          POLLO PINULITO|||FALTANTE     76002
    ## 1467  10001467                                        UBIQUO LABS     76001
    ## 1468  10001468            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1469  10001469                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 1470  10001470                            UBIQUO LABS |||FALTANTE     76001
    ## 1471  10001471 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1472  10001472 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1473  10001473                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1474  10001474                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 1475  10001475                              HOSPITAL LAS AMERICAS     76002
    ## 1476  10001476            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 1477  10001477                          POLLO PINULITO|||FALTANTE     76001
    ## 1478  10001478                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1479  10001479                EL GALLO NEGRO / Despacho a cliente     76001
    ## 1480  10001480            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1481  10001481            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1482  10001482            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1483  10001483                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1484  10001484                     EL PINCHE OBELISCO |||Faltante     76002
    ## 1485  10001485            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 1486  10001486      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 1487  10001487           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1488  10001488                EL GALLO NEGRO / Despacho a cliente     76001
    ## 1489  10001489                                     BAR LA OFICINA     76001
    ## 1490  10001490                                        UBIQUO LABS     76001
    ## 1491  10001491                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 1492  10001492                EL GALLO NEGRO / Despacho a cliente     76002
    ## 1493  10001493                  POLLO PINULITO/Despacho a cliente     76001
    ## 1494  10001494                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1495  10001495                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1496  10001496            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 1497  10001497      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1498  10001498                                        UBIQUO LABS     76001
    ## 1499  10001499                  POLLO PINULITO/Despacho a cliente     76001
    ## 1500  10001500                     EL PINCHE OBELISCO |||Faltante     76002
    ## 1501  10001501                              HOSPITAL LAS AMERICAS     76002
    ## 1502  10001502        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 1503  10001503        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1504  10001504        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1505  10001505        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1506  10001506                EL GALLO NEGRO / Despacho a cliente     76002
    ## 1507  10001507      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1508  10001508           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1509  10001509        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1510  10001510      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1511  10001511            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1512  10001512            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 1513  10001513           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1514  10001514                                     BAR LA OFICINA     76001
    ## 1515  10001515      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1516  10001516            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 1517  10001517                                TAQUERIA EL CHINITO     76001
    ## 1518  10001518                EL GALLO NEGRO / Despacho a cliente     76002
    ## 1519  10001519                                        UBIQUO LABS     76002
    ## 1520  10001520                                        UBIQUO LABS     76002
    ## 1521  10001521      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 1522  10001522                              HOSPITAL LAS AMERICAS     76001
    ## 1523  10001523                          POLLO PINULITO|||FALTANTE     76002
    ## 1524  10001524                            UBIQUO LABS |||FALTANTE     76001
    ## 1525  10001525                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1526  10001526            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1527  10001527                          POLLO PINULITO|||FALTANTE     76002
    ## 1528  10001528            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1529  10001529            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1530  10001530                          POLLO PINULITO|||FALTANTE     76002
    ## 1531  10001531      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1532  10001532                                     BAR LA OFICINA     76002
    ## 1533  10001533            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1534  10001534                  POLLO PINULITO/Despacho a cliente     76001
    ## 1535  10001535            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 1536  10001536                            UBIQUO LABS |||FALTANTE     76001
    ## 1537  10001537            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1538  10001538           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 1539  10001539           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 1540  10001540                EL GALLO NEGRO / Despacho a cliente     76001
    ## 1541  10001541            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1542  10001542                  POLLO PINULITO/Despacho a cliente     76001
    ## 1543  10001543      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1544  10001544                                        UBIQUO LABS     76001
    ## 1545  10001545                                        UBIQUO LABS     76001
    ## 1546  10001546            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 1547  10001547                                TAQUERIA EL CHINITO     76001
    ## 1548  10001548                              HOSPITAL LAS AMERICAS     76002
    ## 1549  10001549            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 1550  10001550                          POLLO PINULITO|||FALTANTE     76001
    ## 1551  10001551                                     BAR LA OFICINA     76002
    ## 1552  10001552            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1553  10001553                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 1554  10001554                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1555  10001555 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 1556  10001556            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 1557  10001557           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1558  10001558            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 1559  10001559        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1560  10001560                                TAQUERIA EL CHINITO     76001
    ## 1561  10001561                            UBIQUO LABS |||FALTANTE     76002
    ## 1562  10001562 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 1563  10001563                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 1564  10001564            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1565  10001565                                        UBIQUO LABS     76002
    ## 1566  10001566 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 1567  10001567                EL GALLO NEGRO / Despacho a cliente     76001
    ## 1568  10001568                  POLLO PINULITO/Despacho a cliente     76001
    ## 1569  10001569                                        UBIQUO LABS     76002
    ## 1570  10001570                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 1571  10001571      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 1572  10001572                                        UBIQUO LABS     76001
    ## 1573  10001573           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1574  10001574                                        UBIQUO LABS     76002
    ## 1575  10001575                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 1576  10001576                            UBIQUO LABS |||FALTANTE     76002
    ## 1577  10001577                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 1578  10001578                  POLLO PINULITO/Despacho a cliente     76001
    ## 1579  10001579                            UBIQUO LABS |||FALTANTE     76001
    ## 1580  10001580            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1581  10001581                EL GALLO NEGRO / Despacho a cliente     76002
    ## 1582  10001582            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 1583  10001583                            UBIQUO LABS |||FALTANTE     76001
    ## 1584  10001584                  POLLO PINULITO/Despacho a cliente     76002
    ## 1585  10001585                                     BAR LA OFICINA     76002
    ## 1586  10001586      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 1587  10001587                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1588  10001588                          POLLO PINULITO|||FALTANTE     76001
    ## 1589  10001589           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 1590  10001590                     EL PINCHE OBELISCO |||Faltante     76002
    ## 1591  10001591                  POLLO PINULITO/Despacho a cliente     76002
    ## 1592  10001592            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 1593  10001593            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 1594  10001594                EL GALLO NEGRO / Despacho a cliente     76001
    ## 1595  10001595                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1596  10001596                                     BAR LA OFICINA     76002
    ## 1597  10001597                EL GALLO NEGRO / Despacho a cliente     76002
    ## 1598  10001598                              HOSPITAL LAS AMERICAS     76001
    ## 1599  10001599                  POLLO PINULITO/Despacho a cliente     76002
    ## 1600  10001600            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1601  10001601                  POLLO PINULITO/Despacho a cliente     76002
    ## 1602  10001602                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 1603  10001603                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 1604  10001604                                TAQUERIA EL CHINITO     76002
    ## 1605  10001605                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 1606  10001606 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1607  10001607                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1608  10001608                EL GALLO NEGRO / Despacho a cliente     76001
    ## 1609  10001609                                     BAR LA OFICINA     76002
    ## 1610  10001610            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 1611  10001611            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 1612  10001612                          POLLO PINULITO|||FALTANTE     76001
    ## 1613  10001613                  POLLO PINULITO/Despacho a cliente     76001
    ## 1614  10001614                                     BAR LA OFICINA     76002
    ## 1615  10001615      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 1616  10001616                                        UBIQUO LABS     76001
    ## 1617  10001617                     EL PINCHE OBELISCO |||Faltante     76002
    ## 1618  10001618                            UBIQUO LABS |||FALTANTE     76002
    ## 1619  10001619                              HOSPITAL LAS AMERICAS     76002
    ## 1620  10001620                     EL PINCHE OBELISCO |||Faltante     76002
    ## 1621  10001621        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1622  10001622                                        UBIQUO LABS     76002
    ## 1623  10001623                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1624  10001624                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1625  10001625            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1626  10001626                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1627  10001627                  POLLO PINULITO/Despacho a cliente     76001
    ## 1628  10001628            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1629  10001629                          POLLO PINULITO|||FALTANTE     76002
    ## 1630  10001630                            UBIQUO LABS |||FALTANTE     76002
    ## 1631  10001631                     EL PINCHE OBELISCO |||Faltante     76002
    ## 1632  10001632                            UBIQUO LABS |||FALTANTE     76001
    ## 1633  10001633                              HOSPITAL LAS AMERICAS     76001
    ## 1634  10001634                                        UBIQUO LABS     76001
    ## 1635  10001635                  POLLO PINULITO/Despacho a cliente     76002
    ## 1636  10001636      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 1637  10001637                                TAQUERIA EL CHINITO     76001
    ## 1638  10001638            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1639  10001639                          POLLO PINULITO|||FALTANTE     76001
    ## 1640  10001640                                        UBIQUO LABS     76002
    ## 1641  10001641        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 1642  10001642        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1643  10001643                            UBIQUO LABS |||FALTANTE     76002
    ## 1644  10001644                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 1645  10001645                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1646  10001646                EL GALLO NEGRO / Despacho a cliente     76001
    ## 1647  10001647                  POLLO PINULITO/Despacho a cliente     76001
    ## 1648  10001648            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1649  10001649                          POLLO PINULITO|||FALTANTE     76002
    ## 1650  10001650 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1651  10001651                                     BAR LA OFICINA     76001
    ## 1652  10001652            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 1653  10001653                EL GALLO NEGRO / Despacho a cliente     76002
    ## 1654  10001654 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1655  10001655            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1656  10001656                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1657  10001657 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1658  10001658           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1659  10001659                              HOSPITAL LAS AMERICAS     76002
    ## 1660  10001660            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1661  10001661                                        UBIQUO LABS     76002
    ## 1662  10001662                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1663  10001663            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1664  10001664                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1665  10001665                            UBIQUO LABS |||FALTANTE     76001
    ## 1666  10001666                          POLLO PINULITO|||FALTANTE     76002
    ## 1667  10001667        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1668  10001668                          POLLO PINULITO|||FALTANTE     76001
    ## 1669  10001669                              HOSPITAL LAS AMERICAS     76002
    ## 1670  10001670                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 1671  10001671        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 1672  10001672      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 1673  10001673            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 1674  10001674        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1675  10001675           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1676  10001676                  POLLO PINULITO/Despacho a cliente     76001
    ## 1677  10001677                                TAQUERIA EL CHINITO     76001
    ## 1678  10001678                          POLLO PINULITO|||FALTANTE     76002
    ## 1679  10001679           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1680  10001680                  POLLO PINULITO/Despacho a cliente     76001
    ## 1681  10001681           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 1682  10001682                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1683  10001683        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 1684  10001684            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 1685  10001685            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 1686  10001686            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 1687  10001687           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1688  10001688                                TAQUERIA EL CHINITO     76002
    ## 1689  10001689                                TAQUERIA EL CHINITO     76001
    ## 1690  10001690           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1691  10001691                            UBIQUO LABS |||FALTANTE     76002
    ## 1692  10001692        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1693  10001693            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1694  10001694                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1695  10001695                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1696  10001696                                        UBIQUO LABS     76002
    ## 1697  10001697                                     BAR LA OFICINA     76002
    ## 1698  10001698                  POLLO PINULITO/Despacho a cliente     76001
    ## 1699  10001699                                     BAR LA OFICINA     76002
    ## 1700  10001700                                     BAR LA OFICINA     76001
    ## 1701  10001701           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 1702  10001702                                        UBIQUO LABS     76001
    ## 1703  10001703                                TAQUERIA EL CHINITO     76002
    ## 1704  10001704                EL GALLO NEGRO / Despacho a cliente     76002
    ## 1705  10001705                     EL PINCHE OBELISCO |||Faltante     76002
    ## 1706  10001706                                     BAR LA OFICINA     76002
    ## 1707  10001707            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1708  10001708      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1709  10001709                              HOSPITAL LAS AMERICAS     76002
    ## 1710  10001710                          POLLO PINULITO|||FALTANTE     76002
    ## 1711  10001711                              HOSPITAL LAS AMERICAS     76001
    ## 1712  10001712                          POLLO PINULITO|||FALTANTE     76001
    ## 1713  10001713 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 1714  10001714                          POLLO PINULITO|||FALTANTE     76002
    ## 1715  10001715                                        UBIQUO LABS     76001
    ## 1716  10001716                                TAQUERIA EL CHINITO     76001
    ## 1717  10001717        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 1718  10001718            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1719  10001719                                        UBIQUO LABS     76002
    ## 1720  10001720            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1721  10001721 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 1722  10001722                              HOSPITAL LAS AMERICAS     76002
    ## 1723  10001723                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1724  10001724                                        UBIQUO LABS     76001
    ## 1725  10001725                EL GALLO NEGRO / Despacho a cliente     76001
    ## 1726  10001726           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1727  10001727                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 1728  10001728                                TAQUERIA EL CHINITO     76002
    ## 1729  10001729      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1730  10001730                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 1731  10001731            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 1732  10001732            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 1733  10001733            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 1734  10001734                          POLLO PINULITO|||FALTANTE     76002
    ## 1735  10001735                EL GALLO NEGRO / Despacho a cliente     76001
    ## 1736  10001736                          POLLO PINULITO|||FALTANTE     76002
    ## 1737  10001737                          POLLO PINULITO|||FALTANTE     76001
    ## 1738  10001738           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1739  10001739        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1740  10001740        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1741  10001741                                     BAR LA OFICINA     76001
    ## 1742  10001742            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1743  10001743      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1744  10001744        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1745  10001745        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1746  10001746                                TAQUERIA EL CHINITO     76002
    ## 1747  10001747            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 1748  10001748                          POLLO PINULITO|||FALTANTE     76002
    ## 1749  10001749            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1750  10001750                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1751  10001751                            UBIQUO LABS |||FALTANTE     76001
    ## 1752  10001752                          POLLO PINULITO|||FALTANTE     76001
    ## 1753  10001753        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1754  10001754                     EL PINCHE OBELISCO |||Faltante     76002
    ## 1755  10001755                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1756  10001756                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1757  10001757                                TAQUERIA EL CHINITO     76002
    ## 1758  10001758            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1759  10001759      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1760  10001760 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1761  10001761                          POLLO PINULITO|||FALTANTE     76001
    ## 1762  10001762                              HOSPITAL LAS AMERICAS     76001
    ## 1763  10001763            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1764  10001764                                        UBIQUO LABS     76002
    ## 1765  10001765                            UBIQUO LABS |||FALTANTE     76002
    ## 1766  10001766        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 1767  10001767 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 1768  10001768           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 1769  10001769      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1770  10001770                              HOSPITAL LAS AMERICAS     76001
    ## 1771  10001771                                TAQUERIA EL CHINITO     76001
    ## 1772  10001772 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 1773  10001773 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 1774  10001774                          POLLO PINULITO|||FALTANTE     76002
    ## 1775  10001775 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1776  10001776            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1777  10001777                  POLLO PINULITO/Despacho a cliente     76002
    ## 1778  10001778                EL GALLO NEGRO / Despacho a cliente     76001
    ## 1779  10001779                                TAQUERIA EL CHINITO     76001
    ## 1780  10001780                                        UBIQUO LABS     76002
    ## 1781  10001781                EL GALLO NEGRO / Despacho a cliente     76002
    ## 1782  10001782                                TAQUERIA EL CHINITO     76002
    ## 1783  10001783                                        UBIQUO LABS     76002
    ## 1784  10001784                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 1785  10001785                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1786  10001786            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1787  10001787                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1788  10001788            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1789  10001789                            UBIQUO LABS |||FALTANTE     76001
    ## 1790  10001790                          POLLO PINULITO|||FALTANTE     76002
    ## 1791  10001791                                        UBIQUO LABS     76002
    ## 1792  10001792                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1793  10001793                                TAQUERIA EL CHINITO     76001
    ## 1794  10001794            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 1795  10001795                              HOSPITAL LAS AMERICAS     76001
    ## 1796  10001796                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 1797  10001797           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1798  10001798                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 1799  10001799 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1800  10001800        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 1801  10001801            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 1802  10001802                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1803  10001803                                TAQUERIA EL CHINITO     76001
    ## 1804  10001804            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1805  10001805 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1806  10001806                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1807  10001807                            UBIQUO LABS |||FALTANTE     76001
    ## 1808  10001808      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 1809  10001809                            UBIQUO LABS |||FALTANTE     76002
    ## 1810  10001810                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 1811  10001811            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1812  10001812                              HOSPITAL LAS AMERICAS     76001
    ## 1813  10001813                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 1814  10001814            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1815  10001815            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1816  10001816            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 1817  10001817                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1818  10001818                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 1819  10001819                     EL PINCHE OBELISCO |||Faltante     76002
    ## 1820  10001820                                     BAR LA OFICINA     76002
    ## 1821  10001821            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1822  10001822        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 1823  10001823                              HOSPITAL LAS AMERICAS     76002
    ## 1824  10001824                                     BAR LA OFICINA     76001
    ## 1825  10001825            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 1826  10001826      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1827  10001827                              HOSPITAL LAS AMERICAS     76002
    ## 1828  10001828                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 1829  10001829                     EL PINCHE OBELISCO |||Faltante     76002
    ## 1830  10001830            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1831  10001831        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1832  10001832                                     BAR LA OFICINA     76001
    ## 1833  10001833                  POLLO PINULITO/Despacho a cliente     76002
    ## 1834  10001834                  POLLO PINULITO/Despacho a cliente     76001
    ## 1835  10001835        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 1836  10001836            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 1837  10001837                                        UBIQUO LABS     76001
    ## 1838  10001838           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1839  10001839                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 1840  10001840            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1841  10001841                EL GALLO NEGRO / Despacho a cliente     76002
    ## 1842  10001842                                        UBIQUO LABS     76002
    ## 1843  10001843                            UBIQUO LABS |||FALTANTE     76001
    ## 1844  10001844                                TAQUERIA EL CHINITO     76002
    ## 1845  10001845                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 1846  10001846                            UBIQUO LABS |||FALTANTE     76001
    ## 1847  10001847                                     BAR LA OFICINA     76001
    ## 1848  10001848                                        UBIQUO LABS     76001
    ## 1849  10001849                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1850  10001850                              HOSPITAL LAS AMERICAS     76001
    ## 1851  10001851      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1852  10001852                            UBIQUO LABS |||FALTANTE     76001
    ## 1853  10001853                                TAQUERIA EL CHINITO     76002
    ## 1854  10001854           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1855  10001855            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1856  10001856                                     BAR LA OFICINA     76002
    ## 1857  10001857            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 1858  10001858                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1859  10001859                            UBIQUO LABS |||FALTANTE     76002
    ## 1860  10001860                                        UBIQUO LABS     76001
    ## 1861  10001861                EL GALLO NEGRO / Despacho a cliente     76002
    ## 1862  10001862 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1863  10001863                            UBIQUO LABS |||FALTANTE     76001
    ## 1864  10001864                     EL PINCHE OBELISCO |||Faltante     76002
    ## 1865  10001865                          POLLO PINULITO|||FALTANTE     76001
    ## 1866  10001866 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1867  10001867      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 1868  10001868                                     BAR LA OFICINA     76002
    ## 1869  10001869                                        UBIQUO LABS     76001
    ## 1870  10001870 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1871  10001871            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 1872  10001872 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1873  10001873            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1874  10001874                EL GALLO NEGRO / Despacho a cliente     76002
    ## 1875  10001875      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 1876  10001876           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1877  10001877                                TAQUERIA EL CHINITO     76001
    ## 1878  10001878                EL GALLO NEGRO / Despacho a cliente     76002
    ## 1879  10001879                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1880  10001880            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 1881  10001881                            UBIQUO LABS |||FALTANTE     76001
    ## 1882  10001882                                TAQUERIA EL CHINITO     76002
    ## 1883  10001883                            UBIQUO LABS |||FALTANTE     76001
    ## 1884  10001884            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1885  10001885                                     BAR LA OFICINA     76002
    ## 1886  10001886                                     BAR LA OFICINA     76002
    ## 1887  10001887                                     BAR LA OFICINA     76001
    ## 1888  10001888                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 1889  10001889                            UBIQUO LABS |||FALTANTE     76001
    ## 1890  10001890                                TAQUERIA EL CHINITO     76001
    ## 1891  10001891            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 1892  10001892                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1893  10001893                            UBIQUO LABS |||FALTANTE     76001
    ## 1894  10001894            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1895  10001895            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 1896  10001896                          POLLO PINULITO|||FALTANTE     76002
    ## 1897  10001897                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 1898  10001898                                TAQUERIA EL CHINITO     76001
    ## 1899  10001899                EL GALLO NEGRO / Despacho a cliente     76001
    ## 1900  10001900 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 1901  10001901                                TAQUERIA EL CHINITO     76002
    ## 1902  10001902                                TAQUERIA EL CHINITO     76001
    ## 1903  10001903                                TAQUERIA EL CHINITO     76001
    ## 1904  10001904           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1905  10001905            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1906  10001906            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 1907  10001907                              HOSPITAL LAS AMERICAS     76002
    ## 1908  10001908                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1909  10001909      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 1910  10001910            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1911  10001911                                        UBIQUO LABS     76002
    ## 1912  10001912            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1913  10001913            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 1914  10001914                                TAQUERIA EL CHINITO     76001
    ## 1915  10001915      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 1916  10001916                EL GALLO NEGRO / Despacho a cliente     76001
    ## 1917  10001917            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1918  10001918                     EL PINCHE OBELISCO |||Faltante     76002
    ## 1919  10001919        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 1920  10001920        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1921  10001921 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1922  10001922                     EL PINCHE OBELISCO |||Faltante     76002
    ## 1923  10001923      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 1924  10001924           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1925  10001925                     EL PINCHE OBELISCO |||Faltante     76001
    ## 1926  10001926                          POLLO PINULITO|||FALTANTE     76002
    ## 1927  10001927            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 1928  10001928                                        UBIQUO LABS     76001
    ## 1929  10001929            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 1930  10001930            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1931  10001931                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 1932  10001932                                TAQUERIA EL CHINITO     76002
    ## 1933  10001933                              HOSPITAL LAS AMERICAS     76001
    ## 1934  10001934        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1935  10001935 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 1936  10001936                  POLLO PINULITO/Despacho a cliente     76001
    ## 1937  10001937           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 1938  10001938                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 1939  10001939            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 1940  10001940                  POLLO PINULITO/Despacho a cliente     76001
    ## 1941  10001941            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1942  10001942                  POLLO PINULITO/Despacho a cliente     76002
    ## 1943  10001943                              HOSPITAL LAS AMERICAS     76001
    ## 1944  10001944                          POLLO PINULITO|||FALTANTE     76002
    ## 1945  10001945            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1946  10001946                                     BAR LA OFICINA     76002
    ## 1947  10001947            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 1948  10001948                                TAQUERIA EL CHINITO     76001
    ## 1949  10001949                                TAQUERIA EL CHINITO     76002
    ## 1950  10001950                                     BAR LA OFICINA     76002
    ## 1951  10001951                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 1952  10001952                                     BAR LA OFICINA     76001
    ## 1953  10001953            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1954  10001954            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1955  10001955            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 1956  10001956            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 1957  10001957                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1958  10001958                                TAQUERIA EL CHINITO     76002
    ## 1959  10001959           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1960  10001960            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1961  10001961                  POLLO PINULITO/Despacho a cliente     76001
    ## 1962  10001962                              HOSPITAL LAS AMERICAS     76001
    ## 1963  10001963           TIENDA LA BENDICION / Despacho a cliente     76002
    ## 1964  10001964                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 1965  10001965                                TAQUERIA EL CHINITO     76002
    ## 1966  10001966        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 1967  10001967                              HOSPITAL LAS AMERICAS     76002
    ## 1968  10001968                                TAQUERIA EL CHINITO     76002
    ## 1969  10001969                     EL PINCHE OBELISCO |||Faltante     76002
    ## 1970  10001970                          POLLO PINULITO|||FALTANTE     76002
    ## 1971  10001971      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 1972  10001972      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1973  10001973                  POLLO PINULITO/Despacho a cliente     76002
    ## 1974  10001974                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 1975  10001975            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1976  10001976                EL GALLO NEGRO / Despacho a cliente     76001
    ## 1977  10001977                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 1978  10001978      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1979  10001979      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1980  10001980                EL GALLO NEGRO / Despacho a cliente     76001
    ## 1981  10001981                     EL PINCHE OBELISCO |||Faltante     76002
    ## 1982  10001982                            UBIQUO LABS |||FALTANTE     76002
    ## 1983  10001983      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 1984  10001984                EL GALLO NEGRO / Despacho a cliente     76001
    ## 1985  10001985            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 1986  10001986                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1987  10001987                                        UBIQUO LABS     76001
    ## 1988  10001988      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1989  10001989                              HOSPITAL LAS AMERICAS     76001
    ## 1990  10001990                            UBIQUO LABS |||FALTANTE     76001
    ## 1991  10001991                                TAQUERIA EL CHINITO     76002
    ## 1992  10001992                                     BAR LA OFICINA     76002
    ## 1993  10001993        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 1994  10001994                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 1995  10001995      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 1996  10001996                                     BAR LA OFICINA     76002
    ## 1997  10001997            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 1998  10001998                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 1999  10001999                                     BAR LA OFICINA     76001
    ## 2000  10002000                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 2001  10002001 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 2002  10002002                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 2003  10002003                                TAQUERIA EL CHINITO     76001
    ## 2004  10002004                EL GALLO NEGRO / Despacho a cliente     76001
    ## 2005  10002005            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 2006  10002006        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 2007  10002007 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 2008  10002008            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 2009  10002009        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 2010  10002010            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 2011  10002011                                     BAR LA OFICINA     76001
    ## 2012  10002012                                TAQUERIA EL CHINITO     76001
    ## 2013  10002013                                        UBIQUO LABS     76002
    ## 2014  10002014                     EL PINCHE OBELISCO |||Faltante     76002
    ## 2015  10002015                          POLLO PINULITO|||FALTANTE     76002
    ## 2016  10002016      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 2017  10002017                                        UBIQUO LABS     76002
    ## 2018  10002018            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 2019  10002019                            UBIQUO LABS |||FALTANTE     76002
    ## 2020  10002020                EL GALLO NEGRO / Despacho a cliente     76002
    ## 2021  10002021           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 2022  10002022 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 2023  10002023            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 2024  10002024                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 2025  10002025                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 2026  10002026            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 2027  10002027                     EL PINCHE OBELISCO |||Faltante     76001
    ## 2028  10002028                          POLLO PINULITO|||FALTANTE     76001
    ## 2029  10002029                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 2030  10002030                EL GALLO NEGRO / Despacho a cliente     76002
    ## 2031  10002031                              HOSPITAL LAS AMERICAS     76001
    ## 2032  10002032            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 2033  10002033                                TAQUERIA EL CHINITO     76002
    ## 2034  10002034                     EL PINCHE OBELISCO |||Faltante     76001
    ## 2035  10002035                              HOSPITAL LAS AMERICAS     76002
    ## 2036  10002036                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 2037  10002037                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 2038  10002038                          POLLO PINULITO|||FALTANTE     76001
    ## 2039  10002039            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 2040  10002040                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 2041  10002041                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 2042  10002042                EL GALLO NEGRO / Despacho a cliente     76001
    ## 2043  10002043                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 2044  10002044                EL GALLO NEGRO / Despacho a cliente     76002
    ## 2045  10002045                          POLLO PINULITO|||FALTANTE     76002
    ## 2046  10002046                                        UBIQUO LABS     76002
    ## 2047  10002047                                TAQUERIA EL CHINITO     76002
    ## 2048  10002048           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 2049  10002049                              HOSPITAL LAS AMERICAS     76002
    ## 2050  10002050                                        UBIQUO LABS     76001
    ## 2051  10002051                EL GALLO NEGRO / Despacho a cliente     76002
    ## 2052  10002052            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 2053  10002053            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 2054  10002054                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 2055  10002055                              HOSPITAL LAS AMERICAS     76001
    ## 2056  10002056        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 2057  10002057                                     BAR LA OFICINA     76002
    ## 2058  10002058                                TAQUERIA EL CHINITO     76002
    ## 2059  10002059           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 2060  10002060                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 2061  10002061                          POLLO PINULITO|||FALTANTE     76002
    ## 2062  10002062                                TAQUERIA EL CHINITO     76002
    ## 2063  10002063                                     BAR LA OFICINA     76001
    ## 2064  10002064                     EL PINCHE OBELISCO |||Faltante     76002
    ## 2065  10002065                       EL GALLO NEGRO |||DEVOLUCION     76002
    ## 2066  10002066                     EL PINCHE OBELISCO |||Faltante     76002
    ## 2067  10002067                          POLLO PINULITO|||FALTANTE     76002
    ## 2068  10002068 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 2069  10002069            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 2070  10002070 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 2071  10002071 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76002
    ## 2072  10002072                              HOSPITAL LAS AMERICAS     76001
    ## 2073  10002073           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 2074  10002074                          POLLO PINULITO|||FALTANTE     76001
    ## 2075  10002075            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 2076  10002076                          POLLO PINULITO|||FALTANTE     76001
    ## 2077  10002077                EL GALLO NEGRO / Despacho a cliente     76002
    ## 2078  10002078        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 2079  10002079                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 2080  10002080                  POLLO PINULITO/Despacho a cliente     76002
    ## 2081  10002081                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 2082  10002082                              HOSPITAL LAS AMERICAS     76001
    ## 2083  10002083                            UBIQUO LABS |||FALTANTE     76002
    ## 2084  10002084      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 2085  10002085            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 2086  10002086           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 2087  10002087                          POLLO PINULITO|||FALTANTE     76001
    ## 2088  10002088                          POLLO PINULITO|||FALTANTE     76001
    ## 2089  10002089                  POLLO PINULITO/Despacho a cliente     76001
    ## 2090  10002090            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 2091  10002091                     EL PINCHE OBELISCO |||Faltante     76001
    ## 2092  10002092                              HOSPITAL LAS AMERICAS     76002
    ## 2093  10002093      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 2094  10002094            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 2095  10002095                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 2096  10002096                                TAQUERIA EL CHINITO     76002
    ## 2097  10002097                  POLLO PINULITO/Despacho a cliente     76001
    ## 2098  10002098                EL GALLO NEGRO / Despacho a cliente     76001
    ## 2099  10002099                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 2100  10002100                              HOSPITAL LAS AMERICAS     76001
    ## 2101  10002101                                     BAR LA OFICINA     76002
    ## 2102  10002102                                     BAR LA OFICINA     76001
    ## 2103  10002103                     EL PINCHE OBELISCO |||Faltante     76002
    ## 2104  10002104            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 2105  10002105                              HOSPITAL LAS AMERICAS     76001
    ## 2106  10002106                  POLLO PINULITO/Despacho a cliente     76002
    ## 2107  10002107            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 2108  10002108           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 2109  10002109                              HOSPITAL LAS AMERICAS     76002
    ## 2110  10002110                                TAQUERIA EL CHINITO     76002
    ## 2111  10002111                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 2112  10002112            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 2113  10002113                                     BAR LA OFICINA     76001
    ## 2114  10002114            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 2115  10002115                                     BAR LA OFICINA     76001
    ## 2116  10002116           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 2117  10002117                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 2118  10002118                                     BAR LA OFICINA     76002
    ## 2119  10002119                          POLLO PINULITO|||FALTANTE     76002
    ## 2120  10002120            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 2121  10002121      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 2122  10002122        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 2123  10002123            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 2124  10002124                EL GALLO NEGRO / Despacho a cliente     76002
    ## 2125  10002125            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 2126  10002126            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 2127  10002127        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 2128  10002128                              HOSPITAL LAS AMERICAS     76001
    ## 2129  10002129                                TAQUERIA EL CHINITO     76002
    ## 2130  10002130                          POLLO PINULITO|||FALTANTE     76001
    ## 2131  10002131                     EL PINCHE OBELISCO |||Faltante     76002
    ## 2132  10002132                            UBIQUO LABS |||FALTANTE     76002
    ## 2133  10002133                EL GALLO NEGRO / Despacho a cliente     76001
    ## 2134  10002134        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 2135  10002135                          POLLO PINULITO|||FALTANTE     76002
    ## 2136  10002136                EL GALLO NEGRO / Despacho a cliente     76002
    ## 2137  10002137      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 2138  10002138            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 2139  10002139            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 2140  10002140                                TAQUERIA EL CHINITO     76002
    ## 2141  10002141        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 2142  10002142            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ## 2143  10002143 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 2144  10002144            EL PINCHE OBELISCO / Despacho a cliente     76002
    ## 2145  10002145           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 2146  10002146                EL GALLO NEGRO / Despacho a cliente     76002
    ## 2147  10002147                                     BAR LA OFICINA     76002
    ## 2148  10002148                       EL GALLO NEGRO |||DEVOLUCION     76001
    ## 2149  10002149        SPORTA, S.A./Despacho a cliente |||Faltante     76002
    ## 2150  10002150           TIENDA LA BENDICION / Despacho a cliente     76001
    ## 2151  10002151                                        UBIQUO LABS     76001
    ## 2152  10002152                          POLLO PINULITO|||FALTANTE     76001
    ## 2153  10002153                                     BAR LA OFICINA     76001
    ## 2154  10002154            HOSPITAL ROOSEVELT / Despacho a cliente     76002
    ## 2155  10002155            HOSPITAL ROOSEVELT / Despacho a cliente     76001
    ## 2156  10002156                     EL PINCHE OBELISCO |||Faltante     76002
    ## 2157  10002157                          POLLO PINULITO|||FALTANTE     76002
    ## 2158  10002158                     EL PINCHE OBELISCO |||Faltante     76002
    ## 2159  10002159                     EL PINCHE OBELISCO |||Faltante     76001
    ## 2160  10002160            ABARROTERIA EBENEZER/Despacho a cliente     76002
    ## 2161  10002161      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 2162  10002162      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76002
    ## 2163  10002163                              HOSPITAL LAS AMERICAS     76002
    ## 2164  10002164                              HOSPITAL LAS AMERICAS     76002
    ## 2165  10002165                                     BAR LA OFICINA     76002
    ## 2166  10002166 UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente     76001
    ## 2167  10002167                EL GALLO NEGRO / Despacho a cliente     76001
    ## 2168  10002168            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 2169  10002169                    TAQUERIA EL CHINITO |||Faltante     76002
    ## 2170  10002170      CHICHARRONERIA EL RICO COLESTEROL |||Faltante     76001
    ## 2171  10002171                                        UBIQUO LABS     76002
    ## 2172  10002172                                     BAR LA OFICINA     76002
    ## 2173  10002173                  POLLO PINULITO/Despacho a cliente     76002
    ## 2174  10002174        SPORTA, S.A./Despacho a cliente |||Faltante     76001
    ## 2175  10002175                    TAQUERIA EL CHINITO |||Faltante     76001
    ## 2176  10002176                                     BAR LA OFICINA     76002
    ## 2177  10002177            EL PINCHE OBELISCO / Despacho a cliente     76001
    ## 2178  10002178                            UBIQUO LABS |||FALTANTE     76002
    ## 2179  10002179                EL GALLO NEGRO / Despacho a cliente     76001
    ## 2180  10002180            ABARROTERIA EBENEZER/Despacho a cliente     76001
    ##      CANTIDAD                        PILOTO      Q CREDITO         UNIDAD
    ## 1        1200       Fernando Mariano Berrio 300.00      30  Camion Grande
    ## 2        1433        Hector Aragones Frutos 358.25      90  Camion Grande
    ## 3        1857          Pedro Alvarez Parejo 464.25      60  Camion Grande
    ## 4         339          Angel Valdez Alegria  84.75      30          Panel
    ## 5        1644 Juan Francisco Portillo Gomez 411.00      30  Camion Grande
    ## 6        1827             Luis Jaime Urbano 456.75      30  Camion Grande
    ## 7        1947      Ismael Rodero Monteagudo 486.75      90  Camion Grande
    ## 8        1716 Juan Francisco Portillo Gomez 429.00      60  Camion Grande
    ## 9        1601      Ismael Rodero Monteagudo 400.25      30  Camion Grande
    ## 10       1343       Fernando Mariano Berrio 335.75      90  Camion Grande
    ## 11       1782             Luis Jaime Urbano 445.50      60  Camion Grande
    ## 12        234              Felipe Villatoro  58.50      30          Panel
    ## 13       1542        Hector Aragones Frutos 385.50      60  Camion Grande
    ## 14        304        Hector Aragones Frutos  76.00      90          Panel
    ## 15       1667             Luis Jaime Urbano 416.75      90  Camion Grande
    ## 16       1325             Luis Jaime Urbano 331.25      30  Camion Grande
    ## 17       1186      Ismael Rodero Monteagudo 296.50      30  Camion Grande
    ## 18        231      Ismael Rodero Monteagudo  57.75      60          Panel
    ## 19        783          Pedro Alvarez Parejo 195.75      30 Camion Pequeño
    ## 20        971              Felipe Villatoro 242.75      60 Camion Pequeño
    ## 21        547       Fernando Mariano Berrio 136.75      90 Camion Pequeño
    ## 22       1550       Fernando Mariano Berrio 387.50      30  Camion Grande
    ## 23        335          Pedro Alvarez Parejo  83.75      60          Panel
    ## 24       1173          Pedro Alvarez Parejo 293.25      90  Camion Grande
    ## 25       1061       Fernando Mariano Berrio 265.25      60  Camion Grande
    ## 26        840 Juan Francisco Portillo Gomez 210.00      30 Camion Pequeño
    ## 27        795       Fernando Mariano Berrio 198.75      60 Camion Pequeño
    ## 28       1330          Angel Valdez Alegria 332.50      30  Camion Grande
    ## 29       1648 Juan Francisco Portillo Gomez 412.00      30  Camion Grande
    ## 30       1502                  Hector Giron 375.50      90  Camion Grande
    ## 31       1659 Juan Francisco Portillo Gomez 414.75      30  Camion Grande
    ## 32       1508                  Hector Giron 377.00      60  Camion Grande
    ## 33        875 Juan Francisco Portillo Gomez 218.75      60 Camion Pequeño
    ## 34        342        Hector Aragones Frutos  85.50      60          Panel
    ## 35       1980          Pedro Alvarez Parejo 495.00      60  Camion Grande
    ## 36       1442              Felipe Villatoro 360.50      30  Camion Grande
    ## 37       1467        Hector Aragones Frutos 366.75      60  Camion Grande
    ## 38       1991        Hector Aragones Frutos 497.75      90  Camion Grande
    ## 39       1589        Hector Aragones Frutos 397.25      30  Camion Grande
    ## 40        732              Felipe Villatoro 183.00      30 Camion Pequeño
    ## 41        766              Felipe Villatoro 191.50      30 Camion Pequeño
    ## 42        827        Hector Aragones Frutos 206.75      90 Camion Pequeño
    ## 43       1027       Fernando Mariano Berrio 256.75      60  Camion Grande
    ## 44       1706             Luis Jaime Urbano 426.50      60  Camion Grande
    ## 45       1044      Ismael Rodero Monteagudo 261.00      60  Camion Grande
    ## 46        596             Luis Jaime Urbano 149.00      60 Camion Pequeño
    ## 47        362             Luis Jaime Urbano  90.50      90          Panel
    ## 48       1937      Ismael Rodero Monteagudo 484.25      60  Camion Grande
    ## 49        297          Angel Valdez Alegria  74.25      90          Panel
    ## 50        400      Ismael Rodero Monteagudo 100.00      90          Panel
    ## 51        225        Hector Aragones Frutos  56.25      60          Panel
    ## 52        562      Ismael Rodero Monteagudo 140.50      60 Camion Pequeño
    ## 53       1791       Fernando Mariano Berrio 447.75      60  Camion Grande
    ## 54        356             Luis Jaime Urbano  89.00      30          Panel
    ## 55       1903             Luis Jaime Urbano 475.75      30  Camion Grande
    ## 56       1231          Pedro Alvarez Parejo 307.75      30  Camion Grande
    ## 57       1682        Hector Aragones Frutos 420.50      60  Camion Grande
    ## 58       1653          Pedro Alvarez Parejo 413.25      90  Camion Grande
    ## 59       1907          Angel Valdez Alegria 476.75      30  Camion Grande
    ## 60       1954      Ismael Rodero Monteagudo 488.50      90  Camion Grande
    ## 61       1177          Pedro Alvarez Parejo 294.25      90  Camion Grande
    ## 62       1247 Juan Francisco Portillo Gomez 311.75      90  Camion Grande
    ## 63       1551                  Hector Giron 387.75      90  Camion Grande
    ## 64       1481             Luis Jaime Urbano 370.25      30  Camion Grande
    ## 65       1126          Angel Valdez Alegria 281.50      60  Camion Grande
    ## 66       1239             Luis Jaime Urbano 309.75      30  Camion Grande
    ## 67       1598       Fernando Mariano Berrio 399.50      90  Camion Grande
    ## 68        965                  Hector Giron 241.25      90 Camion Pequeño
    ## 69       1111        Hector Aragones Frutos 277.75      30  Camion Grande
    ## 70       1092          Pedro Alvarez Parejo 273.00      30  Camion Grande
    ## 71       1107      Ismael Rodero Monteagudo 276.75      30  Camion Grande
    ## 72        958      Ismael Rodero Monteagudo 239.50      90 Camion Pequeño
    ## 73       1402              Felipe Villatoro 350.50      60  Camion Grande
    ## 74       1059 Juan Francisco Portillo Gomez 264.75      60  Camion Grande
    ## 75        592              Felipe Villatoro 148.00      30 Camion Pequeño
    ## 76       1278             Luis Jaime Urbano 319.50      90  Camion Grande
    ## 77       1578                  Hector Giron 394.50      90  Camion Grande
    ## 78       1760 Juan Francisco Portillo Gomez 440.00      30  Camion Grande
    ## 79        628          Angel Valdez Alegria 157.00      90 Camion Pequeño
    ## 80       1221          Angel Valdez Alegria 305.25      60  Camion Grande
    ## 81        547       Fernando Mariano Berrio 136.75      30 Camion Pequeño
    ## 82        742       Fernando Mariano Berrio 185.50      30 Camion Pequeño
    ## 83       1709          Pedro Alvarez Parejo 427.25      90  Camion Grande
    ## 84       1769          Pedro Alvarez Parejo 442.25      30  Camion Grande
    ## 85        384      Ismael Rodero Monteagudo  96.00      90          Panel
    ## 86       1203      Ismael Rodero Monteagudo 300.75      60  Camion Grande
    ## 87        663                  Hector Giron 165.75      90 Camion Pequeño
    ## 88        929                  Hector Giron 232.25      90 Camion Pequeño
    ## 89        859        Hector Aragones Frutos 214.75      60 Camion Pequeño
    ## 90       1144          Pedro Alvarez Parejo 286.00      60  Camion Grande
    ## 91       1351        Hector Aragones Frutos 337.75      90  Camion Grande
    ## 92       1134 Juan Francisco Portillo Gomez 283.50      30  Camion Grande
    ## 93       1929          Pedro Alvarez Parejo 482.25      90  Camion Grande
    ## 94       1285              Felipe Villatoro 321.25      30  Camion Grande
    ## 95       1256          Angel Valdez Alegria 314.00      30  Camion Grande
    ## 96        516        Hector Aragones Frutos 129.00      30 Camion Pequeño
    ## 97        829          Angel Valdez Alegria 207.25      30 Camion Pequeño
    ## 98       1706       Fernando Mariano Berrio 426.50      90  Camion Grande
    ## 99       1201      Ismael Rodero Monteagudo 300.25      60  Camion Grande
    ## 100      1373      Ismael Rodero Monteagudo 343.25      30  Camion Grande
    ## 101      1164       Fernando Mariano Berrio 291.00      90  Camion Grande
    ## 102      1405             Luis Jaime Urbano 351.25      60  Camion Grande
    ## 103      1722          Pedro Alvarez Parejo 430.50      30  Camion Grande
    ## 104       666          Pedro Alvarez Parejo 166.50      90 Camion Pequeño
    ## 105       595      Ismael Rodero Monteagudo 148.75      60 Camion Pequeño
    ## 106       482              Felipe Villatoro 120.50      90          Panel
    ## 107      1340       Fernando Mariano Berrio 335.00      90  Camion Grande
    ## 108      1887          Pedro Alvarez Parejo 471.75      60  Camion Grande
    ## 109       835                  Hector Giron 208.75      30 Camion Pequeño
    ## 110       696             Luis Jaime Urbano 174.00      60 Camion Pequeño
    ## 111       414       Fernando Mariano Berrio 103.50      60          Panel
    ## 112      1888          Pedro Alvarez Parejo 472.00      30  Camion Grande
    ## 113      1080              Felipe Villatoro 270.00      30  Camion Grande
    ## 114      1070 Juan Francisco Portillo Gomez 267.50      90  Camion Grande
    ## 115       710       Fernando Mariano Berrio 177.50      60 Camion Pequeño
    ## 116       557              Felipe Villatoro 139.25      90 Camion Pequeño
    ## 117      1666      Ismael Rodero Monteagudo 416.50      90  Camion Grande
    ## 118      1456                  Hector Giron 364.00      30  Camion Grande
    ## 119      1652                  Hector Giron 413.00      60  Camion Grande
    ## 120      1153        Hector Aragones Frutos 288.25      90  Camion Grande
    ## 121      1668                  Hector Giron 417.00      60  Camion Grande
    ## 122      1315       Fernando Mariano Berrio 328.75      90  Camion Grande
    ## 123      1695 Juan Francisco Portillo Gomez 423.75      90  Camion Grande
    ## 124      1618              Felipe Villatoro 404.50      30  Camion Grande
    ## 125      1912          Angel Valdez Alegria 478.00      30  Camion Grande
    ## 126      1112              Felipe Villatoro 278.00      90  Camion Grande
    ## 127       828              Felipe Villatoro 207.00      90 Camion Pequeño
    ## 128      1820          Pedro Alvarez Parejo 455.00      30  Camion Grande
    ## 129       306          Pedro Alvarez Parejo  76.50      90          Panel
    ## 130      1459        Hector Aragones Frutos 364.75      30  Camion Grande
    ## 131      1230          Angel Valdez Alegria 307.50      90  Camion Grande
    ## 132       456 Juan Francisco Portillo Gomez 114.00      30          Panel
    ## 133       248          Pedro Alvarez Parejo  62.00      30          Panel
    ## 134       237       Fernando Mariano Berrio  59.25      30          Panel
    ## 135      1280       Fernando Mariano Berrio 320.00      60  Camion Grande
    ## 136      1193              Felipe Villatoro 298.25      30  Camion Grande
    ## 137      1294                  Hector Giron 323.50      90  Camion Grande
    ## 138      1435          Pedro Alvarez Parejo 358.75      90  Camion Grande
    ## 139      1589        Hector Aragones Frutos 397.25      30  Camion Grande
    ## 140      1259          Angel Valdez Alegria 314.75      90  Camion Grande
    ## 141      1544      Ismael Rodero Monteagudo 386.00      60  Camion Grande
    ## 142       894 Juan Francisco Portillo Gomez 223.50      60 Camion Pequeño
    ## 143      1764                  Hector Giron 441.00      90  Camion Grande
    ## 144       540          Angel Valdez Alegria 135.00      30 Camion Pequeño
    ## 145      1383              Felipe Villatoro 345.75      60  Camion Grande
    ## 146       288             Luis Jaime Urbano  72.00      60          Panel
    ## 147       636        Hector Aragones Frutos 159.00      90 Camion Pequeño
    ## 148       311          Angel Valdez Alegria  77.75      60          Panel
    ## 149       936       Fernando Mariano Berrio 234.00      90 Camion Pequeño
    ## 150      1607       Fernando Mariano Berrio 401.75      30  Camion Grande
    ## 151      1459                  Hector Giron 364.75      60  Camion Grande
    ## 152       743             Luis Jaime Urbano 185.75      60 Camion Pequeño
    ## 153      1952                  Hector Giron 488.00      30  Camion Grande
    ## 154      1575              Felipe Villatoro 393.75      30  Camion Grande
    ## 155       344          Pedro Alvarez Parejo  86.00      90          Panel
    ## 156       215          Angel Valdez Alegria  53.75      60          Panel
    ## 157      1685 Juan Francisco Portillo Gomez 421.25      90  Camion Grande
    ## 158      1904          Angel Valdez Alegria 476.00      30  Camion Grande
    ## 159       974                  Hector Giron 243.50      30 Camion Pequeño
    ## 160       369              Felipe Villatoro  92.25      30          Panel
    ## 161       609      Ismael Rodero Monteagudo 152.25      90 Camion Pequeño
    ## 162       209       Fernando Mariano Berrio  52.25      60          Panel
    ## 163      1814      Ismael Rodero Monteagudo 453.50      60  Camion Grande
    ## 164      1729      Ismael Rodero Monteagudo 432.25      30  Camion Grande
    ## 165       349        Hector Aragones Frutos  87.25      60          Panel
    ## 166       890             Luis Jaime Urbano 222.50      30 Camion Pequeño
    ## 167       557       Fernando Mariano Berrio 139.25      60 Camion Pequeño
    ## 168       726             Luis Jaime Urbano 181.50      90 Camion Pequeño
    ## 169      1545 Juan Francisco Portillo Gomez 386.25      30  Camion Grande
    ## 170       596          Pedro Alvarez Parejo 149.00      90 Camion Pequeño
    ## 171      1232             Luis Jaime Urbano 308.00      60  Camion Grande
    ## 172      1442       Fernando Mariano Berrio 360.50      30  Camion Grande
    ## 173      1245          Angel Valdez Alegria 311.25      30  Camion Grande
    ## 174      1398          Angel Valdez Alegria 349.50      60  Camion Grande
    ## 175      1305 Juan Francisco Portillo Gomez 326.25      30  Camion Grande
    ## 176       630              Felipe Villatoro 157.50      60 Camion Pequeño
    ## 177      1047             Luis Jaime Urbano 261.75      30  Camion Grande
    ## 178      1235          Angel Valdez Alegria 308.75      60  Camion Grande
    ## 179      1271              Felipe Villatoro 317.75      60  Camion Grande
    ## 180       862          Pedro Alvarez Parejo 215.50      30 Camion Pequeño
    ## 181      1345 Juan Francisco Portillo Gomez 336.25      60  Camion Grande
    ## 182      1044          Angel Valdez Alegria 261.00      30  Camion Grande
    ## 183      1876 Juan Francisco Portillo Gomez 469.00      60  Camion Grande
    ## 184       898      Ismael Rodero Monteagudo 224.50      60 Camion Pequeño
    ## 185      1398        Hector Aragones Frutos 349.50      30  Camion Grande
    ## 186      1719          Pedro Alvarez Parejo 429.75      30  Camion Grande
    ## 187      1847          Angel Valdez Alegria 461.75      30  Camion Grande
    ## 188       407          Pedro Alvarez Parejo 101.75      90          Panel
    ## 189      1699       Fernando Mariano Berrio 424.75      90  Camion Grande
    ## 190      1061                  Hector Giron 265.25      90  Camion Grande
    ## 191      1447                  Hector Giron 361.75      60  Camion Grande
    ## 192       496      Ismael Rodero Monteagudo 124.00      90          Panel
    ## 193       568 Juan Francisco Portillo Gomez 142.00      60 Camion Pequeño
    ## 194       739 Juan Francisco Portillo Gomez 184.75      90 Camion Pequeño
    ## 195       322             Luis Jaime Urbano  80.50      60          Panel
    ## 196       496             Luis Jaime Urbano 124.00      60          Panel
    ## 197      1640        Hector Aragones Frutos 410.00      30  Camion Grande
    ## 198       955          Pedro Alvarez Parejo 238.75      30 Camion Pequeño
    ## 199      1697                  Hector Giron 424.25      90  Camion Grande
    ## 200      1756          Angel Valdez Alegria 439.00      60  Camion Grande
    ## 201      1283          Angel Valdez Alegria 320.75      60  Camion Grande
    ## 202       708              Felipe Villatoro 177.00      30 Camion Pequeño
    ## 203      1509                  Hector Giron 377.25      90  Camion Grande
    ## 204      1333          Angel Valdez Alegria 333.25      90  Camion Grande
    ## 205      1536              Felipe Villatoro 384.00      90  Camion Grande
    ## 206      1062        Hector Aragones Frutos 265.50      30  Camion Grande
    ## 207      1929              Felipe Villatoro 482.25      30  Camion Grande
    ## 208      1698                  Hector Giron 424.50      90  Camion Grande
    ## 209       294          Pedro Alvarez Parejo  73.50      60          Panel
    ## 210       709                  Hector Giron 177.25      60 Camion Pequeño
    ## 211      1817 Juan Francisco Portillo Gomez 454.25      90  Camion Grande
    ## 212      1080       Fernando Mariano Berrio 270.00      60  Camion Grande
    ## 213      1761             Luis Jaime Urbano 440.25      30  Camion Grande
    ## 214      1366             Luis Jaime Urbano 341.50      30  Camion Grande
    ## 215       852          Angel Valdez Alegria 213.00      30 Camion Pequeño
    ## 216      1421       Fernando Mariano Berrio 355.25      30  Camion Grande
    ## 217       919        Hector Aragones Frutos 229.75      30 Camion Pequeño
    ## 218      1163                  Hector Giron 290.75      90  Camion Grande
    ## 219      1775      Ismael Rodero Monteagudo 443.75      90  Camion Grande
    ## 220      1714        Hector Aragones Frutos 428.50      60  Camion Grande
    ## 221      1387          Angel Valdez Alegria 346.75      60  Camion Grande
    ## 222       749              Felipe Villatoro 187.25      30 Camion Pequeño
    ## 223      1763       Fernando Mariano Berrio 440.75      30  Camion Grande
    ## 224      1416             Luis Jaime Urbano 354.00      60  Camion Grande
    ## 225      1471              Felipe Villatoro 367.75      30  Camion Grande
    ## 226      1846          Angel Valdez Alegria 461.50      60  Camion Grande
    ## 227       555          Angel Valdez Alegria 138.75      30 Camion Pequeño
    ## 228      1669              Felipe Villatoro 417.25      60  Camion Grande
    ## 229      1081      Ismael Rodero Monteagudo 270.25      30  Camion Grande
    ## 230       729              Felipe Villatoro 182.25      60 Camion Pequeño
    ## 231      1132          Angel Valdez Alegria 283.00      90  Camion Grande
    ## 232       291       Fernando Mariano Berrio  72.75      30          Panel
    ## 233      1768              Felipe Villatoro 442.00      60  Camion Grande
    ## 234      1798       Fernando Mariano Berrio 449.50      30  Camion Grande
    ## 235       206 Juan Francisco Portillo Gomez  51.50      90          Panel
    ## 236      1295              Felipe Villatoro 323.75      30  Camion Grande
    ## 237       532          Angel Valdez Alegria 133.00      60 Camion Pequeño
    ## 238       732        Hector Aragones Frutos 183.00      90 Camion Pequeño
    ## 239       698          Angel Valdez Alegria 174.50      90 Camion Pequeño
    ## 240      1101        Hector Aragones Frutos 275.25      90  Camion Grande
    ## 241      1155        Hector Aragones Frutos 288.75      60  Camion Grande
    ## 242      1577                  Hector Giron 394.25      30  Camion Grande
    ## 243      1342          Angel Valdez Alegria 335.50      90  Camion Grande
    ## 244      1427             Luis Jaime Urbano 356.75      60  Camion Grande
    ## 245      1490 Juan Francisco Portillo Gomez 372.50      60  Camion Grande
    ## 246       627              Felipe Villatoro 156.75      90 Camion Pequeño
    ## 247      1661 Juan Francisco Portillo Gomez 415.25      60  Camion Grande
    ## 248      1098      Ismael Rodero Monteagudo 274.50      30  Camion Grande
    ## 249      1941          Pedro Alvarez Parejo 485.25      90  Camion Grande
    ## 250      1302          Angel Valdez Alegria 325.50      60  Camion Grande
    ## 251      1498              Felipe Villatoro 374.50      30  Camion Grande
    ## 252       521          Pedro Alvarez Parejo 130.25      90 Camion Pequeño
    ## 253      1714 Juan Francisco Portillo Gomez 428.50      60  Camion Grande
    ## 254       280 Juan Francisco Portillo Gomez  70.00      30          Panel
    ## 255      1867          Angel Valdez Alegria 466.75      30  Camion Grande
    ## 256       347       Fernando Mariano Berrio  86.75      60          Panel
    ## 257       955              Felipe Villatoro 238.75      30 Camion Pequeño
    ## 258      1126              Felipe Villatoro 281.50      90  Camion Grande
    ## 259       225        Hector Aragones Frutos  56.25      30          Panel
    ## 260       479        Hector Aragones Frutos 119.75      60          Panel
    ## 261      1054                  Hector Giron 263.50      30  Camion Grande
    ## 262      1582 Juan Francisco Portillo Gomez 395.50      60  Camion Grande
    ## 263       222      Ismael Rodero Monteagudo  55.50      30          Panel
    ## 264      1307 Juan Francisco Portillo Gomez 326.75      90  Camion Grande
    ## 265       969 Juan Francisco Portillo Gomez 242.25      60 Camion Pequeño
    ## 266      1750          Angel Valdez Alegria 437.50      60  Camion Grande
    ## 267       679                  Hector Giron 169.75      30 Camion Pequeño
    ## 268       788      Ismael Rodero Monteagudo 197.00      30 Camion Pequeño
    ## 269       723 Juan Francisco Portillo Gomez 180.75      60 Camion Pequeño
    ## 270      1913      Ismael Rodero Monteagudo 478.25      60  Camion Grande
    ## 271       665 Juan Francisco Portillo Gomez 166.25      60 Camion Pequeño
    ## 272       930      Ismael Rodero Monteagudo 232.50      30 Camion Pequeño
    ## 273      1003              Felipe Villatoro 250.75      30  Camion Grande
    ## 274       613       Fernando Mariano Berrio 153.25      60 Camion Pequeño
    ## 275       252             Luis Jaime Urbano  63.00      30          Panel
    ## 276      1698       Fernando Mariano Berrio 424.50      90  Camion Grande
    ## 277       956       Fernando Mariano Berrio 239.00      30 Camion Pequeño
    ## 278       830        Hector Aragones Frutos 207.50      60 Camion Pequeño
    ## 279      1071             Luis Jaime Urbano 267.75      90  Camion Grande
    ## 280       602        Hector Aragones Frutos 150.50      30 Camion Pequeño
    ## 281      1890          Angel Valdez Alegria 472.50      60  Camion Grande
    ## 282      1301                  Hector Giron 325.25      90  Camion Grande
    ## 283      1523          Pedro Alvarez Parejo 380.75      60  Camion Grande
    ## 284      1273                  Hector Giron 318.25      90  Camion Grande
    ## 285      1832 Juan Francisco Portillo Gomez 458.00      30  Camion Grande
    ## 286      1956          Pedro Alvarez Parejo 489.00      90  Camion Grande
    ## 287      1026              Felipe Villatoro 256.50      60  Camion Grande
    ## 288      1025             Luis Jaime Urbano 256.25      60  Camion Grande
    ## 289      1526          Pedro Alvarez Parejo 381.50      60  Camion Grande
    ## 290      1361        Hector Aragones Frutos 340.25      90  Camion Grande
    ## 291      1872             Luis Jaime Urbano 468.00      90  Camion Grande
    ## 292       350      Ismael Rodero Monteagudo  87.50      90          Panel
    ## 293      1185             Luis Jaime Urbano 296.25      30  Camion Grande
    ## 294      1568      Ismael Rodero Monteagudo 392.00      30  Camion Grande
    ## 295      1608 Juan Francisco Portillo Gomez 402.00      30  Camion Grande
    ## 296       645             Luis Jaime Urbano 161.25      30 Camion Pequeño
    ## 297       244          Pedro Alvarez Parejo  61.00      60          Panel
    ## 298       615          Angel Valdez Alegria 153.75      60 Camion Pequeño
    ## 299      1897          Pedro Alvarez Parejo 474.25      60  Camion Grande
    ## 300       786             Luis Jaime Urbano 196.50      60 Camion Pequeño
    ## 301       887 Juan Francisco Portillo Gomez 221.75      90 Camion Pequeño
    ## 302      1604              Felipe Villatoro 401.00      90  Camion Grande
    ## 303      1840          Pedro Alvarez Parejo 460.00      30  Camion Grande
    ## 304      1008        Hector Aragones Frutos 252.00      90  Camion Grande
    ## 305      1926 Juan Francisco Portillo Gomez 481.50      30  Camion Grande
    ## 306       737          Pedro Alvarez Parejo 184.25      90 Camion Pequeño
    ## 307      1876       Fernando Mariano Berrio 469.00      60  Camion Grande
    ## 308       336             Luis Jaime Urbano  84.00      60          Panel
    ## 309       489          Pedro Alvarez Parejo 122.25      30          Panel
    ## 310       227 Juan Francisco Portillo Gomez  56.75      60          Panel
    ## 311      1440          Pedro Alvarez Parejo 360.00      30  Camion Grande
    ## 312      1220        Hector Aragones Frutos 305.00      60  Camion Grande
    ## 313      1561      Ismael Rodero Monteagudo 390.25      30  Camion Grande
    ## 314      1988          Pedro Alvarez Parejo 497.00      60  Camion Grande
    ## 315       278        Hector Aragones Frutos  69.50      60          Panel
    ## 316      1227             Luis Jaime Urbano 306.75      60  Camion Grande
    ## 317      1700                  Hector Giron 425.00      60  Camion Grande
    ## 318      1563          Angel Valdez Alegria 390.75      60  Camion Grande
    ## 319       890        Hector Aragones Frutos 222.50      90 Camion Pequeño
    ## 320      1780              Felipe Villatoro 445.00      60  Camion Grande
    ## 321       745                  Hector Giron 186.25      90 Camion Pequeño
    ## 322       906          Pedro Alvarez Parejo 226.50      60 Camion Pequeño
    ## 323       478          Angel Valdez Alegria 119.50      60          Panel
    ## 324      1739      Ismael Rodero Monteagudo 434.75      30  Camion Grande
    ## 325       212          Pedro Alvarez Parejo  53.00      60          Panel
    ## 326      1504 Juan Francisco Portillo Gomez 376.00      30  Camion Grande
    ## 327       789                  Hector Giron 197.25      90 Camion Pequeño
    ## 328      1521      Ismael Rodero Monteagudo 380.25      90  Camion Grande
    ## 329      1282                  Hector Giron 320.50      30  Camion Grande
    ## 330       831 Juan Francisco Portillo Gomez 207.75      30 Camion Pequeño
    ## 331       570             Luis Jaime Urbano 142.50      90 Camion Pequeño
    ## 332       682        Hector Aragones Frutos 170.50      30 Camion Pequeño
    ## 333      1055      Ismael Rodero Monteagudo 263.75      90  Camion Grande
    ## 334       765        Hector Aragones Frutos 191.25      60 Camion Pequeño
    ## 335      1886                  Hector Giron 471.50      30  Camion Grande
    ## 336      1085 Juan Francisco Portillo Gomez 271.25      90  Camion Grande
    ## 337       732             Luis Jaime Urbano 183.00      30 Camion Pequeño
    ## 338       554              Felipe Villatoro 138.50      30 Camion Pequeño
    ## 339       236                  Hector Giron  59.00      60          Panel
    ## 340      1700       Fernando Mariano Berrio 425.00      90  Camion Grande
    ## 341      1460          Angel Valdez Alegria 365.00      30  Camion Grande
    ## 342      1435        Hector Aragones Frutos 358.75      60  Camion Grande
    ## 343      1471       Fernando Mariano Berrio 367.75      30  Camion Grande
    ## 344       761          Pedro Alvarez Parejo 190.25      30 Camion Pequeño
    ## 345      1148          Angel Valdez Alegria 287.00      90  Camion Grande
    ## 346       692                  Hector Giron 173.00      90 Camion Pequeño
    ## 347       253      Ismael Rodero Monteagudo  63.25      90          Panel
    ## 348      1312 Juan Francisco Portillo Gomez 328.00      30  Camion Grande
    ## 349       841          Angel Valdez Alegria 210.25      60 Camion Pequeño
    ## 350       294       Fernando Mariano Berrio  73.50      30          Panel
    ## 351      1454          Pedro Alvarez Parejo 363.50      90  Camion Grande
    ## 352      1445                  Hector Giron 361.25      30  Camion Grande
    ## 353      1926          Pedro Alvarez Parejo 481.50      30  Camion Grande
    ## 354      1774             Luis Jaime Urbano 443.50      30  Camion Grande
    ## 355      1970              Felipe Villatoro 492.50      90  Camion Grande
    ## 356      1577      Ismael Rodero Monteagudo 394.25      60  Camion Grande
    ## 357       238      Ismael Rodero Monteagudo  59.50      90          Panel
    ## 358       354          Angel Valdez Alegria  88.50      60          Panel
    ## 359      1944       Fernando Mariano Berrio 486.00      60  Camion Grande
    ## 360       970      Ismael Rodero Monteagudo 242.50      90 Camion Pequeño
    ## 361      1993                  Hector Giron 498.25      90  Camion Grande
    ## 362      1777       Fernando Mariano Berrio 444.25      60  Camion Grande
    ## 363      1915             Luis Jaime Urbano 478.75      90  Camion Grande
    ## 364      1267       Fernando Mariano Berrio 316.75      30  Camion Grande
    ## 365       950              Felipe Villatoro 237.50      90 Camion Pequeño
    ## 366      1710        Hector Aragones Frutos 427.50      30  Camion Grande
    ## 367       905              Felipe Villatoro 226.25      90 Camion Pequeño
    ## 368      1866             Luis Jaime Urbano 466.50      60  Camion Grande
    ## 369       210             Luis Jaime Urbano  52.50      60          Panel
    ## 370      1402       Fernando Mariano Berrio 350.50      90  Camion Grande
    ## 371       398             Luis Jaime Urbano  99.50      60          Panel
    ## 372      1172          Pedro Alvarez Parejo 293.00      60  Camion Grande
    ## 373       945          Angel Valdez Alegria 236.25      90 Camion Pequeño
    ## 374       402             Luis Jaime Urbano 100.50      30          Panel
    ## 375       200          Pedro Alvarez Parejo  50.00      90          Panel
    ## 376      1639      Ismael Rodero Monteagudo 409.75      60  Camion Grande
    ## 377       264             Luis Jaime Urbano  66.00      60          Panel
    ## 378       733          Pedro Alvarez Parejo 183.25      60 Camion Pequeño
    ## 379       336              Felipe Villatoro  84.00      90          Panel
    ## 380      1304          Angel Valdez Alegria 326.00      60  Camion Grande
    ## 381       620             Luis Jaime Urbano 155.00      60 Camion Pequeño
    ## 382       693          Angel Valdez Alegria 173.25      30 Camion Pequeño
    ## 383      1480          Angel Valdez Alegria 370.00      30  Camion Grande
    ## 384      1225             Luis Jaime Urbano 306.25      90  Camion Grande
    ## 385       600          Pedro Alvarez Parejo 150.00      60 Camion Pequeño
    ## 386       373       Fernando Mariano Berrio  93.25      60          Panel
    ## 387       442              Felipe Villatoro 110.50      60          Panel
    ## 388      1616       Fernando Mariano Berrio 404.00      30  Camion Grande
    ## 389      1099              Felipe Villatoro 274.75      90  Camion Grande
    ## 390       787              Felipe Villatoro 196.75      30 Camion Pequeño
    ## 391      1314 Juan Francisco Portillo Gomez 328.50      90  Camion Grande
    ## 392       862      Ismael Rodero Monteagudo 215.50      60 Camion Pequeño
    ## 393       445          Pedro Alvarez Parejo 111.25      30          Panel
    ## 394       485      Ismael Rodero Monteagudo 121.25      30          Panel
    ## 395       266        Hector Aragones Frutos  66.50      90          Panel
    ## 396      1979      Ismael Rodero Monteagudo 494.75      30  Camion Grande
    ## 397      1116              Felipe Villatoro 279.00      30  Camion Grande
    ## 398       384          Angel Valdez Alegria  96.00      90          Panel
    ## 399       599 Juan Francisco Portillo Gomez 149.75      30 Camion Pequeño
    ## 400       388                  Hector Giron  97.00      90          Panel
    ## 401      1164        Hector Aragones Frutos 291.00      30  Camion Grande
    ## 402      1945       Fernando Mariano Berrio 486.25      90  Camion Grande
    ## 403       724 Juan Francisco Portillo Gomez 181.00      60 Camion Pequeño
    ## 404       412          Angel Valdez Alegria 103.00      60          Panel
    ## 405       807 Juan Francisco Portillo Gomez 201.75      60 Camion Pequeño
    ## 406      1950       Fernando Mariano Berrio 487.50      90  Camion Grande
    ## 407      1957      Ismael Rodero Monteagudo 489.25      30  Camion Grande
    ## 408      1664      Ismael Rodero Monteagudo 416.00      30  Camion Grande
    ## 409      1126             Luis Jaime Urbano 281.50      30  Camion Grande
    ## 410      1328              Felipe Villatoro 332.00      90  Camion Grande
    ## 411       336      Ismael Rodero Monteagudo  84.00      30          Panel
    ## 412      1893          Pedro Alvarez Parejo 473.25      60  Camion Grande
    ## 413       680       Fernando Mariano Berrio 170.00      90 Camion Pequeño
    ## 414      1092        Hector Aragones Frutos 273.00      90  Camion Grande
    ## 415      1174              Felipe Villatoro 293.50      90  Camion Grande
    ## 416       737 Juan Francisco Portillo Gomez 184.25      30 Camion Pequeño
    ## 417       484       Fernando Mariano Berrio 121.00      60          Panel
    ## 418       948       Fernando Mariano Berrio 237.00      60 Camion Pequeño
    ## 419      1312             Luis Jaime Urbano 328.00      90  Camion Grande
    ## 420      1030              Felipe Villatoro 257.50      90  Camion Grande
    ## 421      1903          Pedro Alvarez Parejo 475.75      30  Camion Grande
    ## 422       492 Juan Francisco Portillo Gomez 123.00      30          Panel
    ## 423      1285       Fernando Mariano Berrio 321.25      60  Camion Grande
    ## 424       462              Felipe Villatoro 115.50      90          Panel
    ## 425       294 Juan Francisco Portillo Gomez  73.50      30          Panel
    ## 426      1722                  Hector Giron 430.50      60  Camion Grande
    ## 427       436              Felipe Villatoro 109.00      60          Panel
    ## 428       271       Fernando Mariano Berrio  67.75      60          Panel
    ## 429      1905        Hector Aragones Frutos 476.25      90  Camion Grande
    ## 430       833      Ismael Rodero Monteagudo 208.25      30 Camion Pequeño
    ## 431       224             Luis Jaime Urbano  56.00      60          Panel
    ## 432       703      Ismael Rodero Monteagudo 175.75      60 Camion Pequeño
    ## 433       464      Ismael Rodero Monteagudo 116.00      60          Panel
    ## 434      1238      Ismael Rodero Monteagudo 309.50      60  Camion Grande
    ## 435       462              Felipe Villatoro 115.50      60          Panel
    ## 436      1917             Luis Jaime Urbano 479.25      30  Camion Grande
    ## 437       248      Ismael Rodero Monteagudo  62.00      90          Panel
    ## 438      1270             Luis Jaime Urbano 317.50      90  Camion Grande
    ## 439      1973          Angel Valdez Alegria 493.25      60  Camion Grande
    ## 440       864        Hector Aragones Frutos 216.00      90 Camion Pequeño
    ## 441       424       Fernando Mariano Berrio 106.00      60          Panel
    ## 442       312        Hector Aragones Frutos  78.00      60          Panel
    ## 443      1788      Ismael Rodero Monteagudo 447.00      60  Camion Grande
    ## 444      1186        Hector Aragones Frutos 296.50      90  Camion Grande
    ## 445       414          Pedro Alvarez Parejo 103.50      30          Panel
    ## 446       519                  Hector Giron 129.75      30 Camion Pequeño
    ## 447      1495       Fernando Mariano Berrio 373.75      30  Camion Grande
    ## 448       769                  Hector Giron 192.25      90 Camion Pequeño
    ## 449      1381          Angel Valdez Alegria 345.25      30  Camion Grande
    ## 450       690          Pedro Alvarez Parejo 172.50      30 Camion Pequeño
    ## 451      1593      Ismael Rodero Monteagudo 398.25      30  Camion Grande
    ## 452      1433             Luis Jaime Urbano 358.25      90  Camion Grande
    ## 453       713        Hector Aragones Frutos 178.25      60 Camion Pequeño
    ## 454       636                  Hector Giron 159.00      60 Camion Pequeño
    ## 455       638        Hector Aragones Frutos 159.50      60 Camion Pequeño
    ## 456      1708                  Hector Giron 427.00      30  Camion Grande
    ## 457      1202             Luis Jaime Urbano 300.50      30  Camion Grande
    ## 458       602          Angel Valdez Alegria 150.50      60 Camion Pequeño
    ## 459       323        Hector Aragones Frutos  80.75      60          Panel
    ## 460       320      Ismael Rodero Monteagudo  80.00      60          Panel
    ## 461       381              Felipe Villatoro  95.25      60          Panel
    ## 462       535          Angel Valdez Alegria 133.75      60 Camion Pequeño
    ## 463       562             Luis Jaime Urbano 140.50      30 Camion Pequeño
    ## 464       950       Fernando Mariano Berrio 237.50      30 Camion Pequeño
    ## 465      1883      Ismael Rodero Monteagudo 470.75      60  Camion Grande
    ## 466      1690       Fernando Mariano Berrio 422.50      30  Camion Grande
    ## 467      1327              Felipe Villatoro 331.75      90  Camion Grande
    ## 468      1458             Luis Jaime Urbano 364.50      30  Camion Grande
    ## 469      1059          Angel Valdez Alegria 264.75      30  Camion Grande
    ## 470      1002             Luis Jaime Urbano 250.50      30  Camion Grande
    ## 471      1302          Angel Valdez Alegria 325.50      30  Camion Grande
    ## 472      1639      Ismael Rodero Monteagudo 409.75      60  Camion Grande
    ## 473       840        Hector Aragones Frutos 210.00      60 Camion Pequeño
    ## 474      1849             Luis Jaime Urbano 462.25      30  Camion Grande
    ## 475      1065             Luis Jaime Urbano 266.25      60  Camion Grande
    ## 476      1529      Ismael Rodero Monteagudo 382.25      60  Camion Grande
    ## 477       337      Ismael Rodero Monteagudo  84.25      90          Panel
    ## 478       719        Hector Aragones Frutos 179.75      30 Camion Pequeño
    ## 479      1913                  Hector Giron 478.25      90  Camion Grande
    ## 480      1180          Pedro Alvarez Parejo 295.00      30  Camion Grande
    ## 481      1052          Pedro Alvarez Parejo 263.00      30  Camion Grande
    ## 482      1037          Pedro Alvarez Parejo 259.25      90  Camion Grande
    ## 483      1953      Ismael Rodero Monteagudo 488.25      30  Camion Grande
    ## 484      1083       Fernando Mariano Berrio 270.75      90  Camion Grande
    ## 485      1482              Felipe Villatoro 370.50      30  Camion Grande
    ## 486      1681          Angel Valdez Alegria 420.25      30  Camion Grande
    ## 487      1418      Ismael Rodero Monteagudo 354.50      30  Camion Grande
    ## 488      1285          Angel Valdez Alegria 321.25      60  Camion Grande
    ## 489       878      Ismael Rodero Monteagudo 219.50      90 Camion Pequeño
    ## 490       332          Angel Valdez Alegria  83.00      30          Panel
    ## 491       256             Luis Jaime Urbano  64.00      30          Panel
    ## 492      1724              Felipe Villatoro 431.00      60  Camion Grande
    ## 493      1996          Angel Valdez Alegria 499.00      90  Camion Grande
    ## 494      1097                  Hector Giron 274.25      60  Camion Grande
    ## 495      1009              Felipe Villatoro 252.25      90  Camion Grande
    ## 496      1730       Fernando Mariano Berrio 432.50      60  Camion Grande
    ## 497      1718 Juan Francisco Portillo Gomez 429.50      30  Camion Grande
    ## 498      1720        Hector Aragones Frutos 430.00      60  Camion Grande
    ## 499      1737             Luis Jaime Urbano 434.25      90  Camion Grande
    ## 500       590          Angel Valdez Alegria 147.50      60 Camion Pequeño
    ## 501      1040             Luis Jaime Urbano 260.00      60  Camion Grande
    ## 502       443             Luis Jaime Urbano 110.75      90          Panel
    ## 503      1020      Ismael Rodero Monteagudo 255.00      60  Camion Grande
    ## 504      1309          Pedro Alvarez Parejo 327.25      30  Camion Grande
    ## 505       611      Ismael Rodero Monteagudo 152.75      60 Camion Pequeño
    ## 506      1941          Pedro Alvarez Parejo 485.25      60  Camion Grande
    ## 507       790                  Hector Giron 197.50      30 Camion Pequeño
    ## 508       372              Felipe Villatoro  93.00      30          Panel
    ## 509      1808 Juan Francisco Portillo Gomez 452.00      30  Camion Grande
    ## 510       996       Fernando Mariano Berrio 249.00      60 Camion Pequeño
    ## 511      1580      Ismael Rodero Monteagudo 395.00      90  Camion Grande
    ## 512      1580             Luis Jaime Urbano 395.00      60  Camion Grande
    ## 513      1276              Felipe Villatoro 319.00      90  Camion Grande
    ## 514      1683       Fernando Mariano Berrio 420.75      60  Camion Grande
    ## 515      1055      Ismael Rodero Monteagudo 263.75      30  Camion Grande
    ## 516       814              Felipe Villatoro 203.50      30 Camion Pequeño
    ## 517       397        Hector Aragones Frutos  99.25      30          Panel
    ## 518      1386        Hector Aragones Frutos 346.50      60  Camion Grande
    ## 519      1960          Angel Valdez Alegria 490.00      90  Camion Grande
    ## 520       229       Fernando Mariano Berrio  57.25      90          Panel
    ## 521       831 Juan Francisco Portillo Gomez 207.75      60 Camion Pequeño
    ## 522      1720        Hector Aragones Frutos 430.00      30  Camion Grande
    ## 523      1811       Fernando Mariano Berrio 452.75      30  Camion Grande
    ## 524       319              Felipe Villatoro  79.75      30          Panel
    ## 525      1414             Luis Jaime Urbano 353.50      60  Camion Grande
    ## 526      1097        Hector Aragones Frutos 274.25      90  Camion Grande
    ## 527      1859      Ismael Rodero Monteagudo 464.75      90  Camion Grande
    ## 528       440      Ismael Rodero Monteagudo 110.00      90          Panel
    ## 529       499        Hector Aragones Frutos 124.75      60          Panel
    ## 530      1396 Juan Francisco Portillo Gomez 349.00      90  Camion Grande
    ## 531       403                  Hector Giron 100.75      60          Panel
    ## 532      1445                  Hector Giron 361.25      30  Camion Grande
    ## 533       378              Felipe Villatoro  94.50      30          Panel
    ## 534       924       Fernando Mariano Berrio 231.00      60 Camion Pequeño
    ## 535      1444              Felipe Villatoro 361.00      90  Camion Grande
    ## 536       253          Angel Valdez Alegria  63.25      60          Panel
    ## 537      1947       Fernando Mariano Berrio 486.75      30  Camion Grande
    ## 538      1563              Felipe Villatoro 390.75      30  Camion Grande
    ## 539       742        Hector Aragones Frutos 185.50      30 Camion Pequeño
    ## 540       243              Felipe Villatoro  60.75      90          Panel
    ## 541      1506 Juan Francisco Portillo Gomez 376.50      30  Camion Grande
    ## 542      1889       Fernando Mariano Berrio 472.25      60  Camion Grande
    ## 543       531          Pedro Alvarez Parejo 132.75      90 Camion Pequeño
    ## 544      1037          Angel Valdez Alegria 259.25      90  Camion Grande
    ## 545      1692        Hector Aragones Frutos 423.00      30  Camion Grande
    ## 546       894              Felipe Villatoro 223.50      60 Camion Pequeño
    ## 547       504       Fernando Mariano Berrio 126.00      60 Camion Pequeño
    ## 548       665        Hector Aragones Frutos 166.25      30 Camion Pequeño
    ## 549       964          Angel Valdez Alegria 241.00      90 Camion Pequeño
    ## 550       743          Pedro Alvarez Parejo 185.75      30 Camion Pequeño
    ## 551      1193      Ismael Rodero Monteagudo 298.25      30  Camion Grande
    ## 552      1865       Fernando Mariano Berrio 466.25      60  Camion Grande
    ## 553      1456      Ismael Rodero Monteagudo 364.00      60  Camion Grande
    ## 554       772             Luis Jaime Urbano 193.00      30 Camion Pequeño
    ## 555       385 Juan Francisco Portillo Gomez  96.25      90          Panel
    ## 556       591       Fernando Mariano Berrio 147.75      30 Camion Pequeño
    ## 557      1241             Luis Jaime Urbano 310.25      90  Camion Grande
    ## 558       794          Angel Valdez Alegria 198.50      90 Camion Pequeño
    ## 559      1944      Ismael Rodero Monteagudo 486.00      60  Camion Grande
    ## 560      1355              Felipe Villatoro 338.75      60  Camion Grande
    ## 561       474          Angel Valdez Alegria 118.50      90          Panel
    ## 562      1014          Pedro Alvarez Parejo 253.50      60  Camion Grande
    ## 563       906              Felipe Villatoro 226.50      90 Camion Pequeño
    ## 564       408       Fernando Mariano Berrio 102.00      30          Panel
    ## 565       873                  Hector Giron 218.25      90 Camion Pequeño
    ## 566       905             Luis Jaime Urbano 226.25      90 Camion Pequeño
    ## 567      1249             Luis Jaime Urbano 312.25      30  Camion Grande
    ## 568      1040        Hector Aragones Frutos 260.00      90  Camion Grande
    ## 569       467        Hector Aragones Frutos 116.75      30          Panel
    ## 570      1294      Ismael Rodero Monteagudo 323.50      30  Camion Grande
    ## 571      1497             Luis Jaime Urbano 374.25      30  Camion Grande
    ## 572      1481                  Hector Giron 370.25      90  Camion Grande
    ## 573      1378          Angel Valdez Alegria 344.50      90  Camion Grande
    ## 574       809          Pedro Alvarez Parejo 202.25      90 Camion Pequeño
    ## 575       856      Ismael Rodero Monteagudo 214.00      90 Camion Pequeño
    ## 576       558        Hector Aragones Frutos 139.50      30 Camion Pequeño
    ## 577       280                  Hector Giron  70.00      90          Panel
    ## 578      1049          Angel Valdez Alegria 262.25      60  Camion Grande
    ## 579       533              Felipe Villatoro 133.25      90 Camion Pequeño
    ## 580      1788                  Hector Giron 447.00      90  Camion Grande
    ## 581      1987             Luis Jaime Urbano 496.75      60  Camion Grande
    ## 582       649              Felipe Villatoro 162.25      30 Camion Pequeño
    ## 583       924        Hector Aragones Frutos 231.00      30 Camion Pequeño
    ## 584      1086      Ismael Rodero Monteagudo 271.50      90  Camion Grande
    ## 585      1505          Angel Valdez Alegria 376.25      30  Camion Grande
    ## 586      1532             Luis Jaime Urbano 383.00      90  Camion Grande
    ## 587       502          Pedro Alvarez Parejo 125.50      90 Camion Pequeño
    ## 588      1108                  Hector Giron 277.00      90  Camion Grande
    ## 589      1197             Luis Jaime Urbano 299.25      90  Camion Grande
    ## 590       761       Fernando Mariano Berrio 190.25      30 Camion Pequeño
    ## 591      1274                  Hector Giron 318.50      30  Camion Grande
    ## 592       826          Pedro Alvarez Parejo 206.50      30 Camion Pequeño
    ## 593       485        Hector Aragones Frutos 121.25      60          Panel
    ## 594       311          Pedro Alvarez Parejo  77.75      90          Panel
    ## 595      1457          Pedro Alvarez Parejo 364.25      60  Camion Grande
    ## 596       410             Luis Jaime Urbano 102.50      30          Panel
    ## 597      1679       Fernando Mariano Berrio 419.75      90  Camion Grande
    ## 598       700          Pedro Alvarez Parejo 175.00      60 Camion Pequeño
    ## 599       809          Pedro Alvarez Parejo 202.25      60 Camion Pequeño
    ## 600       529          Pedro Alvarez Parejo 132.25      90 Camion Pequeño
    ## 601      1078        Hector Aragones Frutos 269.50      90  Camion Grande
    ## 602      1086          Angel Valdez Alegria 271.50      60  Camion Grande
    ## 603       465              Felipe Villatoro 116.25      90          Panel
    ## 604       291        Hector Aragones Frutos  72.75      30          Panel
    ## 605      1684                  Hector Giron 421.00      60  Camion Grande
    ## 606       455      Ismael Rodero Monteagudo 113.75      60          Panel
    ## 607      1131          Pedro Alvarez Parejo 282.75      60  Camion Grande
    ## 608      1104              Felipe Villatoro 276.00      90  Camion Grande
    ## 609      1827                  Hector Giron 456.75      90  Camion Grande
    ## 610       504      Ismael Rodero Monteagudo 126.00      30 Camion Pequeño
    ## 611      1016              Felipe Villatoro 254.00      60  Camion Grande
    ## 612       461          Angel Valdez Alegria 115.25      90          Panel
    ## 613      1368                  Hector Giron 342.00      30  Camion Grande
    ## 614      1049          Pedro Alvarez Parejo 262.25      60  Camion Grande
    ## 615       718       Fernando Mariano Berrio 179.50      60 Camion Pequeño
    ## 616      1317          Angel Valdez Alegria 329.25      90  Camion Grande
    ## 617      1945       Fernando Mariano Berrio 486.25      90  Camion Grande
    ## 618       247                  Hector Giron  61.75      30          Panel
    ## 619       980                  Hector Giron 245.00      90 Camion Pequeño
    ## 620       326 Juan Francisco Portillo Gomez  81.50      60          Panel
    ## 621      1956          Pedro Alvarez Parejo 489.00      30  Camion Grande
    ## 622       902       Fernando Mariano Berrio 225.50      60 Camion Pequeño
    ## 623      1147          Angel Valdez Alegria 286.75      60  Camion Grande
    ## 624       539             Luis Jaime Urbano 134.75      90 Camion Pequeño
    ## 625      1113          Angel Valdez Alegria 278.25      60  Camion Grande
    ## 626      1103          Pedro Alvarez Parejo 275.75      90  Camion Grande
    ## 627       303             Luis Jaime Urbano  75.75      60          Panel
    ## 628      1546       Fernando Mariano Berrio 386.50      60  Camion Grande
    ## 629       294          Pedro Alvarez Parejo  73.50      90          Panel
    ## 630      1694       Fernando Mariano Berrio 423.50      30  Camion Grande
    ## 631      1416                  Hector Giron 354.00      60  Camion Grande
    ## 632      1481        Hector Aragones Frutos 370.25      30  Camion Grande
    ## 633      1838             Luis Jaime Urbano 459.50      60  Camion Grande
    ## 634      1487             Luis Jaime Urbano 371.75      60  Camion Grande
    ## 635      1074        Hector Aragones Frutos 268.50      90  Camion Grande
    ## 636      1741        Hector Aragones Frutos 435.25      60  Camion Grande
    ## 637      1458        Hector Aragones Frutos 364.50      60  Camion Grande
    ## 638       813       Fernando Mariano Berrio 203.25      90 Camion Pequeño
    ## 639       856             Luis Jaime Urbano 214.00      60 Camion Pequeño
    ## 640       650              Felipe Villatoro 162.50      30 Camion Pequeño
    ## 641       567          Angel Valdez Alegria 141.75      30 Camion Pequeño
    ## 642      1256        Hector Aragones Frutos 314.00      30  Camion Grande
    ## 643       218             Luis Jaime Urbano  54.50      90          Panel
    ## 644       867          Pedro Alvarez Parejo 216.75      30 Camion Pequeño
    ## 645       913      Ismael Rodero Monteagudo 228.25      60 Camion Pequeño
    ## 646      1512          Angel Valdez Alegria 378.00      60  Camion Grande
    ## 647       203                  Hector Giron  50.75      90          Panel
    ## 648      1012          Angel Valdez Alegria 253.00      60  Camion Grande
    ## 649      1122        Hector Aragones Frutos 280.50      30  Camion Grande
    ## 650      1880                  Hector Giron 470.00      90  Camion Grande
    ## 651       751          Angel Valdez Alegria 187.75      60 Camion Pequeño
    ## 652      1886          Pedro Alvarez Parejo 471.50      60  Camion Grande
    ## 653       363                  Hector Giron  90.75      90          Panel
    ## 654       566 Juan Francisco Portillo Gomez 141.50      30 Camion Pequeño
    ## 655      1330              Felipe Villatoro 332.50      90  Camion Grande
    ## 656      1385 Juan Francisco Portillo Gomez 346.25      90  Camion Grande
    ## 657      1880             Luis Jaime Urbano 470.00      90  Camion Grande
    ## 658      1704          Angel Valdez Alegria 426.00      90  Camion Grande
    ## 659       214      Ismael Rodero Monteagudo  53.50      60          Panel
    ## 660       665       Fernando Mariano Berrio 166.25      60 Camion Pequeño
    ## 661      1950        Hector Aragones Frutos 487.50      90  Camion Grande
    ## 662       500          Angel Valdez Alegria 125.00      90          Panel
    ## 663       990                  Hector Giron 247.50      90 Camion Pequeño
    ## 664      1551        Hector Aragones Frutos 387.75      30  Camion Grande
    ## 665       613       Fernando Mariano Berrio 153.25      60 Camion Pequeño
    ## 666      1571       Fernando Mariano Berrio 392.75      60  Camion Grande
    ## 667       794       Fernando Mariano Berrio 198.50      60 Camion Pequeño
    ## 668      1707              Felipe Villatoro 426.75      60  Camion Grande
    ## 669      1343                  Hector Giron 335.75      60  Camion Grande
    ## 670      1022                  Hector Giron 255.50      90  Camion Grande
    ## 671       413      Ismael Rodero Monteagudo 103.25      60          Panel
    ## 672       690          Angel Valdez Alegria 172.50      90 Camion Pequeño
    ## 673       576          Pedro Alvarez Parejo 144.00      30 Camion Pequeño
    ## 674       769 Juan Francisco Portillo Gomez 192.25      60 Camion Pequeño
    ## 675       735          Pedro Alvarez Parejo 183.75      30 Camion Pequeño
    ## 676       578       Fernando Mariano Berrio 144.50      30 Camion Pequeño
    ## 677      1126 Juan Francisco Portillo Gomez 281.50      30  Camion Grande
    ## 678       907                  Hector Giron 226.75      90 Camion Pequeño
    ## 679      1842       Fernando Mariano Berrio 460.50      90  Camion Grande
    ## 680       651                  Hector Giron 162.75      90 Camion Pequeño
    ## 681       701       Fernando Mariano Berrio 175.25      90 Camion Pequeño
    ## 682       733             Luis Jaime Urbano 183.25      60 Camion Pequeño
    ## 683      1571        Hector Aragones Frutos 392.75      60  Camion Grande
    ## 684       405        Hector Aragones Frutos 101.25      30          Panel
    ## 685       872          Angel Valdez Alegria 218.00      30 Camion Pequeño
    ## 686      1167       Fernando Mariano Berrio 291.75      60  Camion Grande
    ## 687      1867      Ismael Rodero Monteagudo 466.75      60  Camion Grande
    ## 688      1975          Pedro Alvarez Parejo 493.75      30  Camion Grande
    ## 689       900      Ismael Rodero Monteagudo 225.00      60 Camion Pequeño
    ## 690      1400 Juan Francisco Portillo Gomez 350.00      30  Camion Grande
    ## 691       272          Pedro Alvarez Parejo  68.00      90          Panel
    ## 692       358 Juan Francisco Portillo Gomez  89.50      60          Panel
    ## 693      1951 Juan Francisco Portillo Gomez 487.75      60  Camion Grande
    ## 694       807        Hector Aragones Frutos 201.75      30 Camion Pequeño
    ## 695      1748          Angel Valdez Alegria 437.00      30  Camion Grande
    ## 696      1485        Hector Aragones Frutos 371.25      60  Camion Grande
    ## 697      1150      Ismael Rodero Monteagudo 287.50      60  Camion Grande
    ## 698       215          Angel Valdez Alegria  53.75      90          Panel
    ## 699       359                  Hector Giron  89.75      60          Panel
    ## 700       856       Fernando Mariano Berrio 214.00      60 Camion Pequeño
    ## 701       298          Angel Valdez Alegria  74.50      90          Panel
    ## 702       918      Ismael Rodero Monteagudo 229.50      60 Camion Pequeño
    ## 703      1093 Juan Francisco Portillo Gomez 273.25      60  Camion Grande
    ## 704      1713 Juan Francisco Portillo Gomez 428.25      30  Camion Grande
    ## 705      1763          Angel Valdez Alegria 440.75      60  Camion Grande
    ## 706      1809              Felipe Villatoro 452.25      30  Camion Grande
    ## 707      1349      Ismael Rodero Monteagudo 337.25      90  Camion Grande
    ## 708       511       Fernando Mariano Berrio 127.75      30 Camion Pequeño
    ## 709       718              Felipe Villatoro 179.50      30 Camion Pequeño
    ## 710       930              Felipe Villatoro 232.50      60 Camion Pequeño
    ## 711       272       Fernando Mariano Berrio  68.00      30          Panel
    ## 712      1804          Pedro Alvarez Parejo 451.00      30  Camion Grande
    ## 713      1373       Fernando Mariano Berrio 343.25      30  Camion Grande
    ## 714       800              Felipe Villatoro 200.00      60 Camion Pequeño
    ## 715      1638          Pedro Alvarez Parejo 409.50      60  Camion Grande
    ## 716       591          Angel Valdez Alegria 147.75      30 Camion Pequeño
    ## 717       823 Juan Francisco Portillo Gomez 205.75      90 Camion Pequeño
    ## 718      1302          Angel Valdez Alegria 325.50      30  Camion Grande
    ## 719       889          Pedro Alvarez Parejo 222.25      90 Camion Pequeño
    ## 720      1496       Fernando Mariano Berrio 374.00      60  Camion Grande
    ## 721      1394        Hector Aragones Frutos 348.50      60  Camion Grande
    ## 722      1905        Hector Aragones Frutos 476.25      90  Camion Grande
    ## 723       218             Luis Jaime Urbano  54.50      60          Panel
    ## 724      1445      Ismael Rodero Monteagudo 361.25      90  Camion Grande
    ## 725      1581      Ismael Rodero Monteagudo 395.25      90  Camion Grande
    ## 726      1407       Fernando Mariano Berrio 351.75      90  Camion Grande
    ## 727       557              Felipe Villatoro 139.25      90 Camion Pequeño
    ## 728      1636             Luis Jaime Urbano 409.00      30  Camion Grande
    ## 729      1784                  Hector Giron 446.00      60  Camion Grande
    ## 730       718        Hector Aragones Frutos 179.50      30 Camion Pequeño
    ## 731       257        Hector Aragones Frutos  64.25      60          Panel
    ## 732       425       Fernando Mariano Berrio 106.25      90          Panel
    ## 733       685        Hector Aragones Frutos 171.25      90 Camion Pequeño
    ## 734       861             Luis Jaime Urbano 215.25      60 Camion Pequeño
    ## 735      1024                  Hector Giron 256.00      30  Camion Grande
    ## 736      1180        Hector Aragones Frutos 295.00      30  Camion Grande
    ## 737      1595              Felipe Villatoro 398.75      30  Camion Grande
    ## 738       237          Angel Valdez Alegria  59.25      90          Panel
    ## 739       227             Luis Jaime Urbano  56.75      90          Panel
    ## 740      1695 Juan Francisco Portillo Gomez 423.75      30  Camion Grande
    ## 741      1666              Felipe Villatoro 416.50      90  Camion Grande
    ## 742       914                  Hector Giron 228.50      60 Camion Pequeño
    ## 743      1168              Felipe Villatoro 292.00      60  Camion Grande
    ## 744      1958      Ismael Rodero Monteagudo 489.50      60  Camion Grande
    ## 745       318       Fernando Mariano Berrio  79.50      30          Panel
    ## 746      1637 Juan Francisco Portillo Gomez 409.25      30  Camion Grande
    ## 747       923      Ismael Rodero Monteagudo 230.75      60 Camion Pequeño
    ## 748      1346             Luis Jaime Urbano 336.50      30  Camion Grande
    ## 749       817             Luis Jaime Urbano 204.25      90 Camion Pequeño
    ## 750      1948        Hector Aragones Frutos 487.00      90  Camion Grande
    ## 751      1293        Hector Aragones Frutos 323.25      30  Camion Grande
    ## 752       300        Hector Aragones Frutos  75.00      30          Panel
    ## 753      1908                  Hector Giron 477.00      30  Camion Grande
    ## 754       946             Luis Jaime Urbano 236.50      60 Camion Pequeño
    ## 755      1494       Fernando Mariano Berrio 373.50      30  Camion Grande
    ## 756       269          Angel Valdez Alegria  67.25      90          Panel
    ## 757      1608       Fernando Mariano Berrio 402.00      60  Camion Grande
    ## 758      1348             Luis Jaime Urbano 337.00      60  Camion Grande
    ## 759      1090       Fernando Mariano Berrio 272.50      60  Camion Grande
    ## 760       417              Felipe Villatoro 104.25      90          Panel
    ## 761      1748 Juan Francisco Portillo Gomez 437.00      60  Camion Grande
    ## 762       945          Angel Valdez Alegria 236.25      90 Camion Pequeño
    ## 763      1823             Luis Jaime Urbano 455.75      60  Camion Grande
    ## 764      1805              Felipe Villatoro 451.25      30  Camion Grande
    ## 765       243      Ismael Rodero Monteagudo  60.75      30          Panel
    ## 766      1198 Juan Francisco Portillo Gomez 299.50      90  Camion Grande
    ## 767       630        Hector Aragones Frutos 157.50      30 Camion Pequeño
    ## 768       218              Felipe Villatoro  54.50      90          Panel
    ## 769       239          Pedro Alvarez Parejo  59.75      30          Panel
    ## 770      1585      Ismael Rodero Monteagudo 396.25      30  Camion Grande
    ## 771      1640        Hector Aragones Frutos 410.00      60  Camion Grande
    ## 772       723                  Hector Giron 180.75      30 Camion Pequeño
    ## 773       766      Ismael Rodero Monteagudo 191.50      90 Camion Pequeño
    ## 774       509             Luis Jaime Urbano 127.25      30 Camion Pequeño
    ## 775       243       Fernando Mariano Berrio  60.75      30          Panel
    ## 776      1513      Ismael Rodero Monteagudo 378.25      60  Camion Grande
    ## 777       287             Luis Jaime Urbano  71.75      60          Panel
    ## 778      1764          Angel Valdez Alegria 441.00      90  Camion Grande
    ## 779      1669 Juan Francisco Portillo Gomez 417.25      90  Camion Grande
    ## 780       954        Hector Aragones Frutos 238.50      30 Camion Pequeño
    ## 781       679 Juan Francisco Portillo Gomez 169.75      30 Camion Pequeño
    ## 782       267                  Hector Giron  66.75      90          Panel
    ## 783       922             Luis Jaime Urbano 230.50      60 Camion Pequeño
    ## 784       938        Hector Aragones Frutos 234.50      90 Camion Pequeño
    ## 785      1822 Juan Francisco Portillo Gomez 455.50      90  Camion Grande
    ## 786       711       Fernando Mariano Berrio 177.75      60 Camion Pequeño
    ## 787      1829        Hector Aragones Frutos 457.25      30  Camion Grande
    ## 788       323                  Hector Giron  80.75      60          Panel
    ## 789       559                  Hector Giron 139.75      60 Camion Pequeño
    ## 790      1847          Angel Valdez Alegria 461.75      60  Camion Grande
    ## 791      1735             Luis Jaime Urbano 433.75      30  Camion Grande
    ## 792      1811 Juan Francisco Portillo Gomez 452.75      90  Camion Grande
    ## 793      1381             Luis Jaime Urbano 345.25      30  Camion Grande
    ## 794      1879        Hector Aragones Frutos 469.75      90  Camion Grande
    ## 795      1914             Luis Jaime Urbano 478.50      60  Camion Grande
    ## 796      1490          Angel Valdez Alegria 372.50      60  Camion Grande
    ## 797       465        Hector Aragones Frutos 116.25      90          Panel
    ## 798       279                  Hector Giron  69.75      30          Panel
    ## 799       310             Luis Jaime Urbano  77.50      30          Panel
    ## 800      1439              Felipe Villatoro 359.75      30  Camion Grande
    ## 801      1387        Hector Aragones Frutos 346.75      30  Camion Grande
    ## 802      1596             Luis Jaime Urbano 399.00      30  Camion Grande
    ## 803      1074          Pedro Alvarez Parejo 268.50      90  Camion Grande
    ## 804       505          Angel Valdez Alegria 126.25      30 Camion Pequeño
    ## 805      1662          Pedro Alvarez Parejo 415.50      30  Camion Grande
    ## 806       757       Fernando Mariano Berrio 189.25      90 Camion Pequeño
    ## 807      1991      Ismael Rodero Monteagudo 497.75      30  Camion Grande
    ## 808       231 Juan Francisco Portillo Gomez  57.75      90          Panel
    ## 809       959                  Hector Giron 239.75      90 Camion Pequeño
    ## 810       382       Fernando Mariano Berrio  95.50      60          Panel
    ## 811       736              Felipe Villatoro 184.00      90 Camion Pequeño
    ## 812      1734      Ismael Rodero Monteagudo 433.50      90  Camion Grande
    ## 813       584       Fernando Mariano Berrio 146.00      30 Camion Pequeño
    ## 814       524                  Hector Giron 131.00      90 Camion Pequeño
    ## 815       598                  Hector Giron 149.50      30 Camion Pequeño
    ## 816       851                  Hector Giron 212.75      60 Camion Pequeño
    ## 817      1621        Hector Aragones Frutos 405.25      30  Camion Grande
    ## 818       659          Angel Valdez Alegria 164.75      60 Camion Pequeño
    ## 819       810          Angel Valdez Alegria 202.50      90 Camion Pequeño
    ## 820       937             Luis Jaime Urbano 234.25      90 Camion Pequeño
    ## 821      1884      Ismael Rodero Monteagudo 471.00      60  Camion Grande
    ## 822       689 Juan Francisco Portillo Gomez 172.25      90 Camion Pequeño
    ## 823      1754      Ismael Rodero Monteagudo 438.50      30  Camion Grande
    ## 824      1705      Ismael Rodero Monteagudo 426.25      90  Camion Grande
    ## 825      1100      Ismael Rodero Monteagudo 275.00      60  Camion Grande
    ## 826       899          Angel Valdez Alegria 224.75      30 Camion Pequeño
    ## 827       953          Angel Valdez Alegria 238.25      90 Camion Pequeño
    ## 828      1262             Luis Jaime Urbano 315.50      60  Camion Grande
    ## 829      1496      Ismael Rodero Monteagudo 374.00      90  Camion Grande
    ## 830       810       Fernando Mariano Berrio 202.50      30 Camion Pequeño
    ## 831      1821       Fernando Mariano Berrio 455.25      90  Camion Grande
    ## 832       563          Angel Valdez Alegria 140.75      60 Camion Pequeño
    ## 833      1378 Juan Francisco Portillo Gomez 344.50      90  Camion Grande
    ## 834       561 Juan Francisco Portillo Gomez 140.25      60 Camion Pequeño
    ## 835      1349                  Hector Giron 337.25      30  Camion Grande
    ## 836      1047       Fernando Mariano Berrio 261.75      60  Camion Grande
    ## 837      1219              Felipe Villatoro 304.75      90  Camion Grande
    ## 838       591              Felipe Villatoro 147.75      60 Camion Pequeño
    ## 839       980      Ismael Rodero Monteagudo 245.00      60 Camion Pequeño
    ## 840       537       Fernando Mariano Berrio 134.25      60 Camion Pequeño
    ## 841       395 Juan Francisco Portillo Gomez  98.75      30          Panel
    ## 842      1396        Hector Aragones Frutos 349.00      60  Camion Grande
    ## 843      1559        Hector Aragones Frutos 389.75      60  Camion Grande
    ## 844      1116             Luis Jaime Urbano 279.00      30  Camion Grande
    ## 845      1507        Hector Aragones Frutos 376.75      60  Camion Grande
    ## 846       752       Fernando Mariano Berrio 188.00      90 Camion Pequeño
    ## 847      1938                  Hector Giron 484.50      60  Camion Grande
    ## 848      1823       Fernando Mariano Berrio 455.75      90  Camion Grande
    ## 849      1740       Fernando Mariano Berrio 435.00      60  Camion Grande
    ## 850       937                  Hector Giron 234.25      90 Camion Pequeño
    ## 851      1842          Pedro Alvarez Parejo 460.50      90  Camion Grande
    ## 852      1090       Fernando Mariano Berrio 272.50      60  Camion Grande
    ## 853      1342      Ismael Rodero Monteagudo 335.50      60  Camion Grande
    ## 854      1618       Fernando Mariano Berrio 404.50      90  Camion Grande
    ## 855      1753          Pedro Alvarez Parejo 438.25      30  Camion Grande
    ## 856       681             Luis Jaime Urbano 170.25      30 Camion Pequeño
    ## 857      1908             Luis Jaime Urbano 477.00      90  Camion Grande
    ## 858       340             Luis Jaime Urbano  85.00      30          Panel
    ## 859      1902       Fernando Mariano Berrio 475.50      60  Camion Grande
    ## 860      1949        Hector Aragones Frutos 487.25      30  Camion Grande
    ## 861      1583      Ismael Rodero Monteagudo 395.75      30  Camion Grande
    ## 862      1713      Ismael Rodero Monteagudo 428.25      30  Camion Grande
    ## 863      1454             Luis Jaime Urbano 363.50      60  Camion Grande
    ## 864       469       Fernando Mariano Berrio 117.25      60          Panel
    ## 865       620              Felipe Villatoro 155.00      90 Camion Pequeño
    ## 866      1169       Fernando Mariano Berrio 292.25      30  Camion Grande
    ## 867      1619        Hector Aragones Frutos 404.75      30  Camion Grande
    ## 868       880 Juan Francisco Portillo Gomez 220.00      30 Camion Pequeño
    ## 869      1623          Pedro Alvarez Parejo 405.75      90  Camion Grande
    ## 870      1079        Hector Aragones Frutos 269.75      60  Camion Grande
    ## 871       582              Felipe Villatoro 145.50      60 Camion Pequeño
    ## 872      1882       Fernando Mariano Berrio 470.50      60  Camion Grande
    ## 873      1330             Luis Jaime Urbano 332.50      60  Camion Grande
    ## 874       751        Hector Aragones Frutos 187.75      90 Camion Pequeño
    ## 875      1697 Juan Francisco Portillo Gomez 424.25      30  Camion Grande
    ## 876      1261 Juan Francisco Portillo Gomez 315.25      60  Camion Grande
    ## 877      1722             Luis Jaime Urbano 430.50      30  Camion Grande
    ## 878       639      Ismael Rodero Monteagudo 159.75      60 Camion Pequeño
    ## 879       504        Hector Aragones Frutos 126.00      60 Camion Pequeño
    ## 880      1400             Luis Jaime Urbano 350.00      30  Camion Grande
    ## 881      1132      Ismael Rodero Monteagudo 283.00      60  Camion Grande
    ## 882       331             Luis Jaime Urbano  82.75      60          Panel
    ## 883      1064      Ismael Rodero Monteagudo 266.00      30  Camion Grande
    ## 884      1455       Fernando Mariano Berrio 363.75      30  Camion Grande
    ## 885       925          Angel Valdez Alegria 231.25      30 Camion Pequeño
    ## 886       683          Pedro Alvarez Parejo 170.75      60 Camion Pequeño
    ## 887       370 Juan Francisco Portillo Gomez  92.50      30          Panel
    ## 888       510              Felipe Villatoro 127.50      30 Camion Pequeño
    ## 889       875       Fernando Mariano Berrio 218.75      60 Camion Pequeño
    ## 890       271                  Hector Giron  67.75      60          Panel
    ## 891      1522              Felipe Villatoro 380.50      30  Camion Grande
    ## 892      1082             Luis Jaime Urbano 270.50      30  Camion Grande
    ## 893       966          Angel Valdez Alegria 241.50      90 Camion Pequeño
    ## 894      1150          Angel Valdez Alegria 287.50      30  Camion Grande
    ## 895      1887              Felipe Villatoro 471.75      90  Camion Grande
    ## 896       714              Felipe Villatoro 178.50      90 Camion Pequeño
    ## 897      1340        Hector Aragones Frutos 335.00      30  Camion Grande
    ## 898      1289                  Hector Giron 322.25      60  Camion Grande
    ## 899       807          Pedro Alvarez Parejo 201.75      60 Camion Pequeño
    ## 900      1833        Hector Aragones Frutos 458.25      90  Camion Grande
    ## 901      1301       Fernando Mariano Berrio 325.25      60  Camion Grande
    ## 902       651          Pedro Alvarez Parejo 162.75      60 Camion Pequeño
    ## 903       225          Pedro Alvarez Parejo  56.25      90          Panel
    ## 904       935 Juan Francisco Portillo Gomez 233.75      90 Camion Pequeño
    ## 905       840                  Hector Giron 210.00      60 Camion Pequeño
    ## 906       820             Luis Jaime Urbano 205.00      90 Camion Pequeño
    ## 907       587 Juan Francisco Portillo Gomez 146.75      30 Camion Pequeño
    ## 908       581             Luis Jaime Urbano 145.25      60 Camion Pequeño
    ## 909      1769             Luis Jaime Urbano 442.25      30  Camion Grande
    ## 910       847       Fernando Mariano Berrio 211.75      30 Camion Pequeño
    ## 911      1168      Ismael Rodero Monteagudo 292.00      60  Camion Grande
    ## 912      1760 Juan Francisco Portillo Gomez 440.00      90  Camion Grande
    ## 913      1877                  Hector Giron 469.25      60  Camion Grande
    ## 914      1132       Fernando Mariano Berrio 283.00      90  Camion Grande
    ## 915      1187             Luis Jaime Urbano 296.75      90  Camion Grande
    ## 916       472              Felipe Villatoro 118.00      30          Panel
    ## 917      1774      Ismael Rodero Monteagudo 443.50      60  Camion Grande
    ## 918       362                  Hector Giron  90.50      60          Panel
    ## 919      1310              Felipe Villatoro 327.50      60  Camion Grande
    ## 920       885              Felipe Villatoro 221.25      60 Camion Pequeño
    ## 921      1920 Juan Francisco Portillo Gomez 480.00      30  Camion Grande
    ## 922       319              Felipe Villatoro  79.75      30          Panel
    ## 923       202        Hector Aragones Frutos  50.50      60          Panel
    ## 924       504             Luis Jaime Urbano 126.00      30 Camion Pequeño
    ## 925      1167      Ismael Rodero Monteagudo 291.75      30  Camion Grande
    ## 926      1755      Ismael Rodero Monteagudo 438.75      60  Camion Grande
    ## 927       489        Hector Aragones Frutos 122.25      90          Panel
    ## 928       299          Angel Valdez Alegria  74.75      60          Panel
    ## 929      1754                  Hector Giron 438.50      90  Camion Grande
    ## 930       810 Juan Francisco Portillo Gomez 202.50      30 Camion Pequeño
    ## 931      1805      Ismael Rodero Monteagudo 451.25      90  Camion Grande
    ## 932      1980       Fernando Mariano Berrio 495.00      90  Camion Grande
    ## 933       942              Felipe Villatoro 235.50      30 Camion Pequeño
    ## 934      1810       Fernando Mariano Berrio 452.50      90  Camion Grande
    ## 935       916          Angel Valdez Alegria 229.00      60 Camion Pequeño
    ## 936       681                  Hector Giron 170.25      30 Camion Pequeño
    ## 937      1028          Pedro Alvarez Parejo 257.00      90  Camion Grande
    ## 938      1818          Pedro Alvarez Parejo 454.50      90  Camion Grande
    ## 939       813        Hector Aragones Frutos 203.25      30 Camion Pequeño
    ## 940       575          Angel Valdez Alegria 143.75      60 Camion Pequeño
    ## 941      1296                  Hector Giron 324.00      60  Camion Grande
    ## 942      1023          Pedro Alvarez Parejo 255.75      30  Camion Grande
    ## 943      1333             Luis Jaime Urbano 333.25      90  Camion Grande
    ## 944       560 Juan Francisco Portillo Gomez 140.00      60 Camion Pequeño
    ## 945       719                  Hector Giron 179.75      60 Camion Pequeño
    ## 946      1442       Fernando Mariano Berrio 360.50      60  Camion Grande
    ## 947      1430       Fernando Mariano Berrio 357.50      60  Camion Grande
    ## 948      1201        Hector Aragones Frutos 300.25      60  Camion Grande
    ## 949      1212 Juan Francisco Portillo Gomez 303.00      30  Camion Grande
    ## 950       310             Luis Jaime Urbano  77.50      60          Panel
    ## 951      1057          Pedro Alvarez Parejo 264.25      90  Camion Grande
    ## 952       971          Angel Valdez Alegria 242.75      60 Camion Pequeño
    ## 953      1006          Pedro Alvarez Parejo 251.50      30  Camion Grande
    ## 954      1005 Juan Francisco Portillo Gomez 251.25      30  Camion Grande
    ## 955       470 Juan Francisco Portillo Gomez 117.50      60          Panel
    ## 956      1641                  Hector Giron 410.25      90  Camion Grande
    ## 957       890          Angel Valdez Alegria 222.50      60 Camion Pequeño
    ## 958      1392       Fernando Mariano Berrio 348.00      90  Camion Grande
    ## 959      1317          Pedro Alvarez Parejo 329.25      30  Camion Grande
    ## 960      1802        Hector Aragones Frutos 450.50      30  Camion Grande
    ## 961       870       Fernando Mariano Berrio 217.50      90 Camion Pequeño
    ## 962      1497          Angel Valdez Alegria 374.25      30  Camion Grande
    ## 963       736                  Hector Giron 184.00      60 Camion Pequeño
    ## 964       767             Luis Jaime Urbano 191.75      60 Camion Pequeño
    ## 965      1371          Angel Valdez Alegria 342.75      90  Camion Grande
    ## 966      1211          Angel Valdez Alegria 302.75      30  Camion Grande
    ## 967      1816 Juan Francisco Portillo Gomez 454.00      90  Camion Grande
    ## 968      1726       Fernando Mariano Berrio 431.50      90  Camion Grande
    ## 969       801          Angel Valdez Alegria 200.25      90 Camion Pequeño
    ## 970      1512 Juan Francisco Portillo Gomez 378.00      30  Camion Grande
    ## 971       633                  Hector Giron 158.25      60 Camion Pequeño
    ## 972       933          Pedro Alvarez Parejo 233.25      60 Camion Pequeño
    ## 973      1657       Fernando Mariano Berrio 414.25      30  Camion Grande
    ## 974       234          Angel Valdez Alegria  58.50      90          Panel
    ## 975       203             Luis Jaime Urbano  50.75      30          Panel
    ## 976      1848          Angel Valdez Alegria 462.00      30  Camion Grande
    ## 977      1307        Hector Aragones Frutos 326.75      60  Camion Grande
    ## 978      1139        Hector Aragones Frutos 284.75      90  Camion Grande
    ## 979       784       Fernando Mariano Berrio 196.00      60 Camion Pequeño
    ## 980      1825             Luis Jaime Urbano 456.25      60  Camion Grande
    ## 981       682              Felipe Villatoro 170.50      60 Camion Pequeño
    ## 982       735             Luis Jaime Urbano 183.75      60 Camion Pequeño
    ## 983      1062              Felipe Villatoro 265.50      60  Camion Grande
    ## 984      1733              Felipe Villatoro 433.25      90  Camion Grande
    ## 985      1483                  Hector Giron 370.75      30  Camion Grande
    ## 986      1311                  Hector Giron 327.75      60  Camion Grande
    ## 987       933        Hector Aragones Frutos 233.25      60 Camion Pequeño
    ## 988      1838             Luis Jaime Urbano 459.50      90  Camion Grande
    ## 989      1325          Pedro Alvarez Parejo 331.25      30  Camion Grande
    ## 990       497                  Hector Giron 124.25      60          Panel
    ## 991      1177       Fernando Mariano Berrio 294.25      60  Camion Grande
    ## 992      1872      Ismael Rodero Monteagudo 468.00      90  Camion Grande
    ## 993      1663                  Hector Giron 415.75      90  Camion Grande
    ## 994      1657          Pedro Alvarez Parejo 414.25      90  Camion Grande
    ## 995       457          Angel Valdez Alegria 114.25      60          Panel
    ## 996      1574              Felipe Villatoro 393.50      30  Camion Grande
    ## 997       904        Hector Aragones Frutos 226.00      30 Camion Pequeño
    ## 998      1939        Hector Aragones Frutos 484.75      90  Camion Grande
    ## 999      1852             Luis Jaime Urbano 463.00      60  Camion Grande
    ## 1000     1909        Hector Aragones Frutos 477.25      90  Camion Grande
    ## 1001     1411              Felipe Villatoro 352.75      90  Camion Grande
    ## 1002      398       Fernando Mariano Berrio  99.50      60          Panel
    ## 1003      251        Hector Aragones Frutos  62.75      60          Panel
    ## 1004      235        Hector Aragones Frutos  58.75      90          Panel
    ## 1005     1026              Felipe Villatoro 256.50      60  Camion Grande
    ## 1006     1221                  Hector Giron 305.25      90  Camion Grande
    ## 1007      313          Angel Valdez Alegria  78.25      90          Panel
    ## 1008     1474                  Hector Giron 368.50      30  Camion Grande
    ## 1009     1194                  Hector Giron 298.50      60  Camion Grande
    ## 1010     1147        Hector Aragones Frutos 286.75      60  Camion Grande
    ## 1011      439          Pedro Alvarez Parejo 109.75      60          Panel
    ## 1012     1748       Fernando Mariano Berrio 437.00      60  Camion Grande
    ## 1013      577      Ismael Rodero Monteagudo 144.25      60 Camion Pequeño
    ## 1014     1550        Hector Aragones Frutos 387.50      60  Camion Grande
    ## 1015      319 Juan Francisco Portillo Gomez  79.75      90          Panel
    ## 1016      960                  Hector Giron 240.00      90 Camion Pequeño
    ## 1017      264          Pedro Alvarez Parejo  66.00      90          Panel
    ## 1018      859              Felipe Villatoro 214.75      30 Camion Pequeño
    ## 1019     1210                  Hector Giron 302.50      60  Camion Grande
    ## 1020     1592          Angel Valdez Alegria 398.00      30  Camion Grande
    ## 1021     1186              Felipe Villatoro 296.50      30  Camion Grande
    ## 1022     1763          Angel Valdez Alegria 440.75      60  Camion Grande
    ## 1023      217              Felipe Villatoro  54.25      60          Panel
    ## 1024     1783      Ismael Rodero Monteagudo 445.75      90  Camion Grande
    ## 1025      805      Ismael Rodero Monteagudo 201.25      90 Camion Pequeño
    ## 1026     1241          Angel Valdez Alegria 310.25      30  Camion Grande
    ## 1027     1760                  Hector Giron 440.00      30  Camion Grande
    ## 1028     1915 Juan Francisco Portillo Gomez 478.75      60  Camion Grande
    ## 1029     1798             Luis Jaime Urbano 449.50      30  Camion Grande
    ## 1030     1327       Fernando Mariano Berrio 331.75      30  Camion Grande
    ## 1031     1167       Fernando Mariano Berrio 291.75      30  Camion Grande
    ## 1032     1256      Ismael Rodero Monteagudo 314.00      60  Camion Grande
    ## 1033     1290 Juan Francisco Portillo Gomez 322.50      60  Camion Grande
    ## 1034      511       Fernando Mariano Berrio 127.75      90 Camion Pequeño
    ## 1035      287        Hector Aragones Frutos  71.75      90          Panel
    ## 1036     1318             Luis Jaime Urbano 329.50      60  Camion Grande
    ## 1037      906          Pedro Alvarez Parejo 226.50      60 Camion Pequeño
    ## 1038     1921          Angel Valdez Alegria 480.25      90  Camion Grande
    ## 1039     1279          Pedro Alvarez Parejo 319.75      30  Camion Grande
    ## 1040      445      Ismael Rodero Monteagudo 111.25      60          Panel
    ## 1041     1616       Fernando Mariano Berrio 404.00      30  Camion Grande
    ## 1042      868                  Hector Giron 217.00      30 Camion Pequeño
    ## 1043      789          Pedro Alvarez Parejo 197.25      30 Camion Pequeño
    ## 1044      547             Luis Jaime Urbano 136.75      90 Camion Pequeño
    ## 1045      700              Felipe Villatoro 175.00      90 Camion Pequeño
    ## 1046     1377      Ismael Rodero Monteagudo 344.25      30  Camion Grande
    ## 1047     1326          Angel Valdez Alegria 331.50      60  Camion Grande
    ## 1048      322 Juan Francisco Portillo Gomez  80.50      60          Panel
    ## 1049      223             Luis Jaime Urbano  55.75      60          Panel
    ## 1050     1767             Luis Jaime Urbano 441.75      90  Camion Grande
    ## 1051     1048                  Hector Giron 262.00      90  Camion Grande
    ## 1052     1114          Angel Valdez Alegria 278.50      60  Camion Grande
    ## 1053      740              Felipe Villatoro 185.00      90 Camion Pequeño
    ## 1054      776          Angel Valdez Alegria 194.00      60 Camion Pequeño
    ## 1055      609              Felipe Villatoro 152.25      60 Camion Pequeño
    ## 1056     1660             Luis Jaime Urbano 415.00      30  Camion Grande
    ## 1057      272             Luis Jaime Urbano  68.00      60          Panel
    ## 1058      526       Fernando Mariano Berrio 131.50      90 Camion Pequeño
    ## 1059      598          Angel Valdez Alegria 149.50      90 Camion Pequeño
    ## 1060      747        Hector Aragones Frutos 186.75      90 Camion Pequeño
    ## 1061     1351              Felipe Villatoro 337.75      90  Camion Grande
    ## 1062     1515          Angel Valdez Alegria 378.75      60  Camion Grande
    ## 1063     1499          Pedro Alvarez Parejo 374.75      30  Camion Grande
    ## 1064     1757          Angel Valdez Alegria 439.25      90  Camion Grande
    ## 1065     1613      Ismael Rodero Monteagudo 403.25      30  Camion Grande
    ## 1066     1928          Pedro Alvarez Parejo 482.00      30  Camion Grande
    ## 1067     1124          Angel Valdez Alegria 281.00      30  Camion Grande
    ## 1068     1052             Luis Jaime Urbano 263.00      30  Camion Grande
    ## 1069     1035             Luis Jaime Urbano 258.75      90  Camion Grande
    ## 1070     1275             Luis Jaime Urbano 318.75      90  Camion Grande
    ## 1071      323      Ismael Rodero Monteagudo  80.75      30          Panel
    ## 1072     1496             Luis Jaime Urbano 374.00      60  Camion Grande
    ## 1073      771        Hector Aragones Frutos 192.75      30 Camion Pequeño
    ## 1074     1625      Ismael Rodero Monteagudo 406.25      90  Camion Grande
    ## 1075     1252             Luis Jaime Urbano 313.00      60  Camion Grande
    ## 1076     1199       Fernando Mariano Berrio 299.75      60  Camion Grande
    ## 1077     1889                  Hector Giron 472.25      90  Camion Grande
    ## 1078     1051 Juan Francisco Portillo Gomez 262.75      90  Camion Grande
    ## 1079      595                  Hector Giron 148.75      30 Camion Pequeño
    ## 1080      965                  Hector Giron 241.25      30 Camion Pequeño
    ## 1081     1615          Angel Valdez Alegria 403.75      60  Camion Grande
    ## 1082      296        Hector Aragones Frutos  74.00      30          Panel
    ## 1083     1784       Fernando Mariano Berrio 446.00      90  Camion Grande
    ## 1084      386              Felipe Villatoro  96.50      60          Panel
    ## 1085     1176             Luis Jaime Urbano 294.00      90  Camion Grande
    ## 1086      259      Ismael Rodero Monteagudo  64.75      90          Panel
    ## 1087      443 Juan Francisco Portillo Gomez 110.75      90          Panel
    ## 1088      917       Fernando Mariano Berrio 229.25      90 Camion Pequeño
    ## 1089      917 Juan Francisco Portillo Gomez 229.25      90 Camion Pequeño
    ## 1090      406          Pedro Alvarez Parejo 101.50      90          Panel
    ## 1091     1674       Fernando Mariano Berrio 418.50      60  Camion Grande
    ## 1092      546              Felipe Villatoro 136.50      30 Camion Pequeño
    ## 1093     1263                  Hector Giron 315.75      60  Camion Grande
    ## 1094      509 Juan Francisco Portillo Gomez 127.25      90 Camion Pequeño
    ## 1095     1840          Pedro Alvarez Parejo 460.00      30  Camion Grande
    ## 1096     1472             Luis Jaime Urbano 368.00      30  Camion Grande
    ## 1097      242          Pedro Alvarez Parejo  60.50      30          Panel
    ## 1098      880      Ismael Rodero Monteagudo 220.00      90 Camion Pequeño
    ## 1099     1028        Hector Aragones Frutos 257.00      30  Camion Grande
    ## 1100     1830          Pedro Alvarez Parejo 457.50      30  Camion Grande
    ## 1101      597                  Hector Giron 149.25      60 Camion Pequeño
    ## 1102     1755        Hector Aragones Frutos 438.75      30  Camion Grande
    ## 1103      608             Luis Jaime Urbano 152.00      90 Camion Pequeño
    ## 1104      883       Fernando Mariano Berrio 220.75      30 Camion Pequeño
    ## 1105      677        Hector Aragones Frutos 169.25      60 Camion Pequeño
    ## 1106     1554       Fernando Mariano Berrio 388.50      30  Camion Grande
    ## 1107     1004          Pedro Alvarez Parejo 251.00      60  Camion Grande
    ## 1108      540                  Hector Giron 135.00      60 Camion Pequeño
    ## 1109     1936                  Hector Giron 484.00      30  Camion Grande
    ## 1110     1227              Felipe Villatoro 306.75      30  Camion Grande
    ## 1111     1266                  Hector Giron 316.50      60  Camion Grande
    ## 1112     1291              Felipe Villatoro 322.75      60  Camion Grande
    ## 1113     1470        Hector Aragones Frutos 367.50      60  Camion Grande
    ## 1114     1029             Luis Jaime Urbano 257.25      30  Camion Grande
    ## 1115      614             Luis Jaime Urbano 153.50      30 Camion Pequeño
    ## 1116      809          Pedro Alvarez Parejo 202.25      60 Camion Pequeño
    ## 1117      986                  Hector Giron 246.50      90 Camion Pequeño
    ## 1118     1573              Felipe Villatoro 393.25      60  Camion Grande
    ## 1119      227        Hector Aragones Frutos  56.75      60          Panel
    ## 1120      723                  Hector Giron 180.75      60 Camion Pequeño
    ## 1121      351        Hector Aragones Frutos  87.75      60          Panel
    ## 1122     1865                  Hector Giron 466.25      60  Camion Grande
    ## 1123     1480             Luis Jaime Urbano 370.00      60  Camion Grande
    ## 1124     1206              Felipe Villatoro 301.50      90  Camion Grande
    ## 1125      576      Ismael Rodero Monteagudo 144.00      90 Camion Pequeño
    ## 1126     1273      Ismael Rodero Monteagudo 318.25      30  Camion Grande
    ## 1127     1598      Ismael Rodero Monteagudo 399.50      30  Camion Grande
    ## 1128      318      Ismael Rodero Monteagudo  79.50      60          Panel
    ## 1129      381             Luis Jaime Urbano  95.25      30          Panel
    ## 1130     1342 Juan Francisco Portillo Gomez 335.50      90  Camion Grande
    ## 1131     1985          Pedro Alvarez Parejo 496.25      90  Camion Grande
    ## 1132      273             Luis Jaime Urbano  68.25      60          Panel
    ## 1133     1008      Ismael Rodero Monteagudo 252.00      60  Camion Grande
    ## 1134      633      Ismael Rodero Monteagudo 158.25      30 Camion Pequeño
    ## 1135     1694 Juan Francisco Portillo Gomez 423.50      30  Camion Grande
    ## 1136      359              Felipe Villatoro  89.75      30          Panel
    ## 1137      282          Angel Valdez Alegria  70.50      60          Panel
    ## 1138      266       Fernando Mariano Berrio  66.50      30          Panel
    ## 1139     1095          Pedro Alvarez Parejo 273.75      90  Camion Grande
    ## 1140     1468      Ismael Rodero Monteagudo 367.00      30  Camion Grande
    ## 1141      370       Fernando Mariano Berrio  92.50      30          Panel
    ## 1142     1561      Ismael Rodero Monteagudo 390.25      30  Camion Grande
    ## 1143      918      Ismael Rodero Monteagudo 229.50      30 Camion Pequeño
    ## 1144     1771             Luis Jaime Urbano 442.75      30  Camion Grande
    ## 1145     1872        Hector Aragones Frutos 468.00      90  Camion Grande
    ## 1146      860                  Hector Giron 215.00      60 Camion Pequeño
    ## 1147     1270          Angel Valdez Alegria 317.50      60  Camion Grande
    ## 1148     1260        Hector Aragones Frutos 315.00      90  Camion Grande
    ## 1149     1762             Luis Jaime Urbano 440.50      90  Camion Grande
    ## 1150     1680        Hector Aragones Frutos 420.00      30  Camion Grande
    ## 1151      973        Hector Aragones Frutos 243.25      90 Camion Pequeño
    ## 1152     1060       Fernando Mariano Berrio 265.00      60  Camion Grande
    ## 1153      831             Luis Jaime Urbano 207.75      30 Camion Pequeño
    ## 1154      864                  Hector Giron 216.00      90 Camion Pequeño
    ## 1155     1564              Felipe Villatoro 391.00      30  Camion Grande
    ## 1156      210             Luis Jaime Urbano  52.50      60          Panel
    ## 1157      474          Angel Valdez Alegria 118.50      90          Panel
    ## 1158     1484          Pedro Alvarez Parejo 371.00      30  Camion Grande
    ## 1159      668             Luis Jaime Urbano 167.00      60 Camion Pequeño
    ## 1160      949                  Hector Giron 237.25      30 Camion Pequeño
    ## 1161     1057        Hector Aragones Frutos 264.25      30  Camion Grande
    ## 1162      558 Juan Francisco Portillo Gomez 139.50      60 Camion Pequeño
    ## 1163      237                  Hector Giron  59.25      90          Panel
    ## 1164      549       Fernando Mariano Berrio 137.25      60 Camion Pequeño
    ## 1165      530             Luis Jaime Urbano 132.50      60 Camion Pequeño
    ## 1166      453              Felipe Villatoro 113.25      90          Panel
    ## 1167      616       Fernando Mariano Berrio 154.00      60 Camion Pequeño
    ## 1168     1973        Hector Aragones Frutos 493.25      30  Camion Grande
    ## 1169      862          Angel Valdez Alegria 215.50      60 Camion Pequeño
    ## 1170     1641          Angel Valdez Alegria 410.25      90  Camion Grande
    ## 1171     1795              Felipe Villatoro 448.75      90  Camion Grande
    ## 1172     1951             Luis Jaime Urbano 487.75      60  Camion Grande
    ## 1173      358      Ismael Rodero Monteagudo  89.50      60          Panel
    ## 1174      426          Pedro Alvarez Parejo 106.50      30          Panel
    ## 1175     1330 Juan Francisco Portillo Gomez 332.50      90  Camion Grande
    ## 1176     1595      Ismael Rodero Monteagudo 398.75      90  Camion Grande
    ## 1177      294          Angel Valdez Alegria  73.50      90          Panel
    ## 1178      668 Juan Francisco Portillo Gomez 167.00      30 Camion Pequeño
    ## 1179      256 Juan Francisco Portillo Gomez  64.00      30          Panel
    ## 1180     1959              Felipe Villatoro 489.75      30  Camion Grande
    ## 1181     1970       Fernando Mariano Berrio 492.50      30  Camion Grande
    ## 1182     1367        Hector Aragones Frutos 341.75      90  Camion Grande
    ## 1183     1391          Angel Valdez Alegria 347.75      60  Camion Grande
    ## 1184      344          Pedro Alvarez Parejo  86.00      60          Panel
    ## 1185      633              Felipe Villatoro 158.25      30 Camion Pequeño
    ## 1186     1085       Fernando Mariano Berrio 271.25      60  Camion Grande
    ## 1187     1212          Angel Valdez Alegria 303.00      60  Camion Grande
    ## 1188      899        Hector Aragones Frutos 224.75      30 Camion Pequeño
    ## 1189     1562       Fernando Mariano Berrio 390.50      30  Camion Grande
    ## 1190     1113          Angel Valdez Alegria 278.25      30  Camion Grande
    ## 1191      446          Pedro Alvarez Parejo 111.50      30          Panel
    ## 1192     1517             Luis Jaime Urbano 379.25      90  Camion Grande
    ## 1193      570                  Hector Giron 142.50      60 Camion Pequeño
    ## 1194     1619          Pedro Alvarez Parejo 404.75      60  Camion Grande
    ## 1195      213      Ismael Rodero Monteagudo  53.25      30          Panel
    ## 1196      318      Ismael Rodero Monteagudo  79.50      30          Panel
    ## 1197      956          Angel Valdez Alegria 239.00      30 Camion Pequeño
    ## 1198      948                  Hector Giron 237.00      30 Camion Pequeño
    ## 1199      447 Juan Francisco Portillo Gomez 111.75      60          Panel
    ## 1200     1752              Felipe Villatoro 438.00      60  Camion Grande
    ## 1201      247              Felipe Villatoro  61.75      30          Panel
    ## 1202      627          Angel Valdez Alegria 156.75      90 Camion Pequeño
    ## 1203     1334          Angel Valdez Alegria 333.50      90  Camion Grande
    ## 1204     1297                  Hector Giron 324.25      30  Camion Grande
    ## 1205      518          Angel Valdez Alegria 129.50      30 Camion Pequeño
    ## 1206     1162 Juan Francisco Portillo Gomez 290.50      90  Camion Grande
    ## 1207     1088          Pedro Alvarez Parejo 272.00      30  Camion Grande
    ## 1208      994       Fernando Mariano Berrio 248.50      90 Camion Pequeño
    ## 1209     1368              Felipe Villatoro 342.00      90  Camion Grande
    ## 1210     1839              Felipe Villatoro 459.75      30  Camion Grande
    ## 1211     1574              Felipe Villatoro 393.50      30  Camion Grande
    ## 1212      259          Angel Valdez Alegria  64.75      60          Panel
    ## 1213      267      Ismael Rodero Monteagudo  66.75      30          Panel
    ## 1214     1794        Hector Aragones Frutos 448.50      30  Camion Grande
    ## 1215      855          Pedro Alvarez Parejo 213.75      60 Camion Pequeño
    ## 1216     1322      Ismael Rodero Monteagudo 330.50      60  Camion Grande
    ## 1217     1696                  Hector Giron 424.00      60  Camion Grande
    ## 1218     1532      Ismael Rodero Monteagudo 383.00      30  Camion Grande
    ## 1219     1769       Fernando Mariano Berrio 442.25      90  Camion Grande
    ## 1220     1864              Felipe Villatoro 466.00      30  Camion Grande
    ## 1221      225                  Hector Giron  56.25      60          Panel
    ## 1222     1434             Luis Jaime Urbano 358.50      30  Camion Grande
    ## 1223      836             Luis Jaime Urbano 209.00      90 Camion Pequeño
    ## 1224     1703             Luis Jaime Urbano 425.75      90  Camion Grande
    ## 1225     1369 Juan Francisco Portillo Gomez 342.25      30  Camion Grande
    ## 1226     1577      Ismael Rodero Monteagudo 394.25      60  Camion Grande
    ## 1227      778        Hector Aragones Frutos 194.50      30 Camion Pequeño
    ## 1228     1451 Juan Francisco Portillo Gomez 362.75      60  Camion Grande
    ## 1229      784          Pedro Alvarez Parejo 196.00      90 Camion Pequeño
    ## 1230      206      Ismael Rodero Monteagudo  51.50      30          Panel
    ## 1231      269       Fernando Mariano Berrio  67.25      30          Panel
    ## 1232      562          Pedro Alvarez Parejo 140.50      30 Camion Pequeño
    ## 1233     1251       Fernando Mariano Berrio 312.75      60  Camion Grande
    ## 1234     1031 Juan Francisco Portillo Gomez 257.75      90  Camion Grande
    ## 1235      663      Ismael Rodero Monteagudo 165.75      60 Camion Pequeño
    ## 1236     1852 Juan Francisco Portillo Gomez 463.00      30  Camion Grande
    ## 1237      847       Fernando Mariano Berrio 211.75      30 Camion Pequeño
    ## 1238     1144          Pedro Alvarez Parejo 286.00      30  Camion Grande
    ## 1239     1564             Luis Jaime Urbano 391.00      30  Camion Grande
    ## 1240     1349              Felipe Villatoro 337.25      60  Camion Grande
    ## 1241      467                  Hector Giron 116.75      60          Panel
    ## 1242     1353          Angel Valdez Alegria 338.25      90  Camion Grande
    ## 1243     1122 Juan Francisco Portillo Gomez 280.50      90  Camion Grande
    ## 1244      357 Juan Francisco Portillo Gomez  89.25      30          Panel
    ## 1245      921                  Hector Giron 230.25      90 Camion Pequeño
    ## 1246      520              Felipe Villatoro 130.00      30 Camion Pequeño
    ## 1247     1144      Ismael Rodero Monteagudo 286.00      30  Camion Grande
    ## 1248      808       Fernando Mariano Berrio 202.00      30 Camion Pequeño
    ## 1249     1627             Luis Jaime Urbano 406.75      60  Camion Grande
    ## 1250      665        Hector Aragones Frutos 166.25      90 Camion Pequeño
    ## 1251     1793              Felipe Villatoro 448.25      30  Camion Grande
    ## 1252     1393                  Hector Giron 348.25      30  Camion Grande
    ## 1253      625 Juan Francisco Portillo Gomez 156.25      90 Camion Pequeño
    ## 1254     1063              Felipe Villatoro 265.75      30  Camion Grande
    ## 1255     1461      Ismael Rodero Monteagudo 365.25      30  Camion Grande
    ## 1256     1564          Pedro Alvarez Parejo 391.00      30  Camion Grande
    ## 1257     1309       Fernando Mariano Berrio 327.25      30  Camion Grande
    ## 1258     1475       Fernando Mariano Berrio 368.75      90  Camion Grande
    ## 1259      581          Angel Valdez Alegria 145.25      60 Camion Pequeño
    ## 1260     1787          Pedro Alvarez Parejo 446.75      90  Camion Grande
    ## 1261     1363                  Hector Giron 340.75      30  Camion Grande
    ## 1262     1550                  Hector Giron 387.50      90  Camion Grande
    ## 1263     1408                  Hector Giron 352.00      60  Camion Grande
    ## 1264      725 Juan Francisco Portillo Gomez 181.25      30 Camion Pequeño
    ## 1265     1452        Hector Aragones Frutos 363.00      90  Camion Grande
    ## 1266     1080             Luis Jaime Urbano 270.00      60  Camion Grande
    ## 1267     1919        Hector Aragones Frutos 479.75      30  Camion Grande
    ## 1268     1205        Hector Aragones Frutos 301.25      60  Camion Grande
    ## 1269     1917       Fernando Mariano Berrio 479.25      90  Camion Grande
    ## 1270     1852              Felipe Villatoro 463.00      90  Camion Grande
    ## 1271     1690          Pedro Alvarez Parejo 422.50      30  Camion Grande
    ## 1272      725        Hector Aragones Frutos 181.25      60 Camion Pequeño
    ## 1273      260                  Hector Giron  65.00      90          Panel
    ## 1274     1330                  Hector Giron 332.50      90  Camion Grande
    ## 1275      439              Felipe Villatoro 109.75      60          Panel
    ## 1276      390      Ismael Rodero Monteagudo  97.50      90          Panel
    ## 1277     1605       Fernando Mariano Berrio 401.25      90  Camion Grande
    ## 1278     1785              Felipe Villatoro 446.25      60  Camion Grande
    ## 1279     1321              Felipe Villatoro 330.25      90  Camion Grande
    ## 1280      455          Pedro Alvarez Parejo 113.75      90          Panel
    ## 1281      400 Juan Francisco Portillo Gomez 100.00      60          Panel
    ## 1282     1536       Fernando Mariano Berrio 384.00      60  Camion Grande
    ## 1283     1506              Felipe Villatoro 376.50      60  Camion Grande
    ## 1284     1444          Pedro Alvarez Parejo 361.00      60  Camion Grande
    ## 1285     1336       Fernando Mariano Berrio 334.00      30  Camion Grande
    ## 1286     1114 Juan Francisco Portillo Gomez 278.50      60  Camion Grande
    ## 1287      417              Felipe Villatoro 104.25      90          Panel
    ## 1288     1460      Ismael Rodero Monteagudo 365.00      60  Camion Grande
    ## 1289     1444        Hector Aragones Frutos 361.00      30  Camion Grande
    ## 1290     1985        Hector Aragones Frutos 496.25      60  Camion Grande
    ## 1291      474        Hector Aragones Frutos 118.50      30          Panel
    ## 1292      528              Felipe Villatoro 132.00      60 Camion Pequeño
    ## 1293     1295       Fernando Mariano Berrio 323.75      30  Camion Grande
    ## 1294      735          Pedro Alvarez Parejo 183.75      30 Camion Pequeño
    ## 1295     1207 Juan Francisco Portillo Gomez 301.75      30  Camion Grande
    ## 1296     1840      Ismael Rodero Monteagudo 460.00      60  Camion Grande
    ## 1297      217        Hector Aragones Frutos  54.25      60          Panel
    ## 1298     1568          Pedro Alvarez Parejo 392.00      90  Camion Grande
    ## 1299      540      Ismael Rodero Monteagudo 135.00      60 Camion Pequeño
    ## 1300     1292 Juan Francisco Portillo Gomez 323.00      90  Camion Grande
    ## 1301     1445          Angel Valdez Alegria 361.25      60  Camion Grande
    ## 1302     1083              Felipe Villatoro 270.75      30  Camion Grande
    ## 1303     1290      Ismael Rodero Monteagudo 322.50      30  Camion Grande
    ## 1304     1479          Angel Valdez Alegria 369.75      60  Camion Grande
    ## 1305      867        Hector Aragones Frutos 216.75      30 Camion Pequeño
    ## 1306      673      Ismael Rodero Monteagudo 168.25      60 Camion Pequeño
    ## 1307      658             Luis Jaime Urbano 164.50      30 Camion Pequeño
    ## 1308     1673       Fernando Mariano Berrio 418.25      60  Camion Grande
    ## 1309     1462      Ismael Rodero Monteagudo 365.50      60  Camion Grande
    ## 1310     1333      Ismael Rodero Monteagudo 333.25      30  Camion Grande
    ## 1311      593          Pedro Alvarez Parejo 148.25      90 Camion Pequeño
    ## 1312      223 Juan Francisco Portillo Gomez  55.75      30          Panel
    ## 1313     1486 Juan Francisco Portillo Gomez 371.50      60  Camion Grande
    ## 1314     1684      Ismael Rodero Monteagudo 421.00      30  Camion Grande
    ## 1315     1957       Fernando Mariano Berrio 489.25      60  Camion Grande
    ## 1316      221      Ismael Rodero Monteagudo  55.25      60          Panel
    ## 1317      604             Luis Jaime Urbano 151.00      60 Camion Pequeño
    ## 1318     1619          Angel Valdez Alegria 404.75      60  Camion Grande
    ## 1319      355          Pedro Alvarez Parejo  88.75      60          Panel
    ## 1320     1524          Pedro Alvarez Parejo 381.00      90  Camion Grande
    ## 1321     1178        Hector Aragones Frutos 294.50      30  Camion Grande
    ## 1322     1174          Pedro Alvarez Parejo 293.50      90  Camion Grande
    ## 1323      645              Felipe Villatoro 161.25      90 Camion Pequeño
    ## 1324     1368              Felipe Villatoro 342.00      90  Camion Grande
    ## 1325      652                  Hector Giron 163.00      30 Camion Pequeño
    ## 1326      866 Juan Francisco Portillo Gomez 216.50      30 Camion Pequeño
    ## 1327     1337          Pedro Alvarez Parejo 334.25      90  Camion Grande
    ## 1328      570              Felipe Villatoro 142.50      60 Camion Pequeño
    ## 1329      484 Juan Francisco Portillo Gomez 121.00      60          Panel
    ## 1330     1858          Angel Valdez Alegria 464.50      60  Camion Grande
    ## 1331     1427       Fernando Mariano Berrio 356.75      90  Camion Grande
    ## 1332     1920             Luis Jaime Urbano 480.00      30  Camion Grande
    ## 1333     1183      Ismael Rodero Monteagudo 295.75      30  Camion Grande
    ## 1334     1536          Angel Valdez Alegria 384.00      90  Camion Grande
    ## 1335     1063          Angel Valdez Alegria 265.75      30  Camion Grande
    ## 1336     1981             Luis Jaime Urbano 495.25      60  Camion Grande
    ## 1337      298        Hector Aragones Frutos  74.50      30          Panel
    ## 1338      356             Luis Jaime Urbano  89.00      90          Panel
    ## 1339      915      Ismael Rodero Monteagudo 228.75      60 Camion Pequeño
    ## 1340      214      Ismael Rodero Monteagudo  53.50      60          Panel
    ## 1341     1811          Angel Valdez Alegria 452.75      30  Camion Grande
    ## 1342     1349      Ismael Rodero Monteagudo 337.25      60  Camion Grande
    ## 1343      652          Pedro Alvarez Parejo 163.00      30 Camion Pequeño
    ## 1344      437             Luis Jaime Urbano 109.25      60          Panel
    ## 1345      583 Juan Francisco Portillo Gomez 145.75      60 Camion Pequeño
    ## 1346     1818                  Hector Giron 454.50      30  Camion Grande
    ## 1347     1811             Luis Jaime Urbano 452.75      60  Camion Grande
    ## 1348     1938       Fernando Mariano Berrio 484.50      90  Camion Grande
    ## 1349     1930      Ismael Rodero Monteagudo 482.50      60  Camion Grande
    ## 1350      907       Fernando Mariano Berrio 226.75      60 Camion Pequeño
    ## 1351      212        Hector Aragones Frutos  53.00      30          Panel
    ## 1352     1387             Luis Jaime Urbano 346.75      90  Camion Grande
    ## 1353      316                  Hector Giron  79.00      90          Panel
    ## 1354      218                  Hector Giron  54.50      60          Panel
    ## 1355      565                  Hector Giron 141.25      60 Camion Pequeño
    ## 1356      893        Hector Aragones Frutos 223.25      90 Camion Pequeño
    ## 1357      595              Felipe Villatoro 148.75      30 Camion Pequeño
    ## 1358     1303             Luis Jaime Urbano 325.75      60  Camion Grande
    ## 1359      303 Juan Francisco Portillo Gomez  75.75      90          Panel
    ## 1360      703        Hector Aragones Frutos 175.75      30 Camion Pequeño
    ## 1361     1149                  Hector Giron 287.25      60  Camion Grande
    ## 1362      996          Pedro Alvarez Parejo 249.00      60 Camion Pequeño
    ## 1363     1107        Hector Aragones Frutos 276.75      90  Camion Grande
    ## 1364     1668              Felipe Villatoro 417.00      90  Camion Grande
    ## 1365     1196              Felipe Villatoro 299.00      30  Camion Grande
    ## 1366     1863 Juan Francisco Portillo Gomez 465.75      30  Camion Grande
    ## 1367     1980          Angel Valdez Alegria 495.00      30  Camion Grande
    ## 1368      419                  Hector Giron 104.75      90          Panel
    ## 1369     1269          Angel Valdez Alegria 317.25      60  Camion Grande
    ## 1370      304        Hector Aragones Frutos  76.00      90          Panel
    ## 1371      625          Pedro Alvarez Parejo 156.25      90 Camion Pequeño
    ## 1372     1073       Fernando Mariano Berrio 268.25      90  Camion Grande
    ## 1373      541        Hector Aragones Frutos 135.25      90 Camion Pequeño
    ## 1374      363              Felipe Villatoro  90.75      60          Panel
    ## 1375      314 Juan Francisco Portillo Gomez  78.50      30          Panel
    ## 1376      304       Fernando Mariano Berrio  76.00      30          Panel
    ## 1377     1151             Luis Jaime Urbano 287.75      30  Camion Grande
    ## 1378     1050      Ismael Rodero Monteagudo 262.50      60  Camion Grande
    ## 1379     1723                  Hector Giron 430.75      60  Camion Grande
    ## 1380     1305          Angel Valdez Alegria 326.25      60  Camion Grande
    ## 1381      422        Hector Aragones Frutos 105.50      30          Panel
    ## 1382     1641      Ismael Rodero Monteagudo 410.25      60  Camion Grande
    ## 1383      386      Ismael Rodero Monteagudo  96.50      60          Panel
    ## 1384     1079          Pedro Alvarez Parejo 269.75      60  Camion Grande
    ## 1385      703      Ismael Rodero Monteagudo 175.75      30 Camion Pequeño
    ## 1386      648      Ismael Rodero Monteagudo 162.00      30 Camion Pequeño
    ## 1387     1730       Fernando Mariano Berrio 432.50      30  Camion Grande
    ## 1388     1113             Luis Jaime Urbano 278.25      60  Camion Grande
    ## 1389     1134 Juan Francisco Portillo Gomez 283.50      60  Camion Grande
    ## 1390     1736 Juan Francisco Portillo Gomez 434.00      60  Camion Grande
    ## 1391     1351 Juan Francisco Portillo Gomez 337.75      60  Camion Grande
    ## 1392     1041              Felipe Villatoro 260.25      60  Camion Grande
    ## 1393      397             Luis Jaime Urbano  99.25      30          Panel
    ## 1394      988              Felipe Villatoro 247.00      60 Camion Pequeño
    ## 1395      716          Angel Valdez Alegria 179.00      60 Camion Pequeño
    ## 1396      459      Ismael Rodero Monteagudo 114.75      60          Panel
    ## 1397     1362       Fernando Mariano Berrio 340.50      60  Camion Grande
    ## 1398      879      Ismael Rodero Monteagudo 219.75      30 Camion Pequeño
    ## 1399      661        Hector Aragones Frutos 165.25      30 Camion Pequeño
    ## 1400     1520             Luis Jaime Urbano 380.00      30  Camion Grande
    ## 1401      912          Pedro Alvarez Parejo 228.00      30 Camion Pequeño
    ## 1402     1534                  Hector Giron 383.50      60  Camion Grande
    ## 1403      565 Juan Francisco Portillo Gomez 141.25      30 Camion Pequeño
    ## 1404     1332          Angel Valdez Alegria 333.00      60  Camion Grande
    ## 1405     1725 Juan Francisco Portillo Gomez 431.25      30  Camion Grande
    ## 1406     1824              Felipe Villatoro 456.00      60  Camion Grande
    ## 1407      262              Felipe Villatoro  65.50      30          Panel
    ## 1408      312       Fernando Mariano Berrio  78.00      30          Panel
    ## 1409     1465      Ismael Rodero Monteagudo 366.25      90  Camion Grande
    ## 1410     1095      Ismael Rodero Monteagudo 273.75      90  Camion Grande
    ## 1411     1702      Ismael Rodero Monteagudo 425.50      60  Camion Grande
    ## 1412     1985        Hector Aragones Frutos 496.25      90  Camion Grande
    ## 1413      726                  Hector Giron 181.50      30 Camion Pequeño
    ## 1414     1874      Ismael Rodero Monteagudo 468.50      30  Camion Grande
    ## 1415      367      Ismael Rodero Monteagudo  91.75      30          Panel
    ## 1416      347 Juan Francisco Portillo Gomez  86.75      30          Panel
    ## 1417      454             Luis Jaime Urbano 113.50      30          Panel
    ## 1418     1819 Juan Francisco Portillo Gomez 454.75      30  Camion Grande
    ## 1419     1993 Juan Francisco Portillo Gomez 498.25      90  Camion Grande
    ## 1420     1487      Ismael Rodero Monteagudo 371.75      90  Camion Grande
    ## 1421     1102             Luis Jaime Urbano 275.50      30  Camion Grande
    ## 1422      874          Pedro Alvarez Parejo 218.50      90 Camion Pequeño
    ## 1423     1290          Pedro Alvarez Parejo 322.50      60  Camion Grande
    ## 1424     1838                  Hector Giron 459.50      90  Camion Grande
    ## 1425     1762      Ismael Rodero Monteagudo 440.50      90  Camion Grande
    ## 1426      464      Ismael Rodero Monteagudo 116.00      90          Panel
    ## 1427     1504             Luis Jaime Urbano 376.00      60  Camion Grande
    ## 1428      424             Luis Jaime Urbano 106.00      90          Panel
    ## 1429     1599                  Hector Giron 399.75      30  Camion Grande
    ## 1430      399             Luis Jaime Urbano  99.75      30          Panel
    ## 1431     1343 Juan Francisco Portillo Gomez 335.75      30  Camion Grande
    ## 1432      587              Felipe Villatoro 146.75      60 Camion Pequeño
    ## 1433     1839      Ismael Rodero Monteagudo 459.75      90  Camion Grande
    ## 1434     1238        Hector Aragones Frutos 309.50      90  Camion Grande
    ## 1435     1382                  Hector Giron 345.50      60  Camion Grande
    ## 1436      654 Juan Francisco Portillo Gomez 163.50      60 Camion Pequeño
    ## 1437      211          Pedro Alvarez Parejo  52.75      60          Panel
    ## 1438      701          Pedro Alvarez Parejo 175.25      60 Camion Pequeño
    ## 1439     1975              Felipe Villatoro 493.75      60  Camion Grande
    ## 1440      389              Felipe Villatoro  97.25      60          Panel
    ## 1441      324                  Hector Giron  81.00      90          Panel
    ## 1442     1624 Juan Francisco Portillo Gomez 406.00      60  Camion Grande
    ## 1443     1936              Felipe Villatoro 484.00      60  Camion Grande
    ## 1444      996                  Hector Giron 249.00      60 Camion Pequeño
    ## 1445      997             Luis Jaime Urbano 249.25      30 Camion Pequeño
    ## 1446     1919              Felipe Villatoro 479.75      30  Camion Grande
    ## 1447     1429                  Hector Giron 357.25      30  Camion Grande
    ## 1448      885 Juan Francisco Portillo Gomez 221.25      30 Camion Pequeño
    ## 1449     1429          Pedro Alvarez Parejo 357.25      30  Camion Grande
    ## 1450     1682              Felipe Villatoro 420.50      30  Camion Grande
    ## 1451     1361              Felipe Villatoro 340.25      90  Camion Grande
    ## 1452     1559             Luis Jaime Urbano 389.75      60  Camion Grande
    ## 1453      704 Juan Francisco Portillo Gomez 176.00      30 Camion Pequeño
    ## 1454     1701                  Hector Giron 425.25      60  Camion Grande
    ## 1455      654              Felipe Villatoro 163.50      30 Camion Pequeño
    ## 1456     1458          Angel Valdez Alegria 364.50      30  Camion Grande
    ## 1457      909                  Hector Giron 227.25      90 Camion Pequeño
    ## 1458     1750          Angel Valdez Alegria 437.50      90  Camion Grande
    ## 1459      819 Juan Francisco Portillo Gomez 204.75      60 Camion Pequeño
    ## 1460     1496                  Hector Giron 374.00      30  Camion Grande
    ## 1461     1256                  Hector Giron 314.00      30  Camion Grande
    ## 1462     1454       Fernando Mariano Berrio 363.50      90  Camion Grande
    ## 1463      270        Hector Aragones Frutos  67.50      90          Panel
    ## 1464      534          Angel Valdez Alegria 133.50      90 Camion Pequeño
    ## 1465     1674 Juan Francisco Portillo Gomez 418.50      30  Camion Grande
    ## 1466      217                  Hector Giron  54.25      90          Panel
    ## 1467     1546 Juan Francisco Portillo Gomez 386.50      30  Camion Grande
    ## 1468     1750              Felipe Villatoro 437.50      30  Camion Grande
    ## 1469     1017             Luis Jaime Urbano 254.25      90  Camion Grande
    ## 1470      721 Juan Francisco Portillo Gomez 180.25      60 Camion Pequeño
    ## 1471     1916             Luis Jaime Urbano 479.00      30  Camion Grande
    ## 1472      506       Fernando Mariano Berrio 126.50      90 Camion Pequeño
    ## 1473      574              Felipe Villatoro 143.50      30 Camion Pequeño
    ## 1474     1386          Angel Valdez Alegria 346.50      90  Camion Grande
    ## 1475      214          Pedro Alvarez Parejo  53.50      90          Panel
    ## 1476     1955       Fernando Mariano Berrio 488.75      60  Camion Grande
    ## 1477     1835       Fernando Mariano Berrio 458.75      60  Camion Grande
    ## 1478      692              Felipe Villatoro 173.00      60 Camion Pequeño
    ## 1479      650              Felipe Villatoro 162.50      90 Camion Pequeño
    ## 1480      650 Juan Francisco Portillo Gomez 162.50      60 Camion Pequeño
    ## 1481      875          Angel Valdez Alegria 218.75      30 Camion Pequeño
    ## 1482     1263 Juan Francisco Portillo Gomez 315.75      90  Camion Grande
    ## 1483      647                  Hector Giron 161.75      30 Camion Pequeño
    ## 1484      579        Hector Aragones Frutos 144.75      30 Camion Pequeño
    ## 1485      343 Juan Francisco Portillo Gomez  85.75      60          Panel
    ## 1486      639      Ismael Rodero Monteagudo 159.75      30 Camion Pequeño
    ## 1487     1325                  Hector Giron 331.25      30  Camion Grande
    ## 1488      882      Ismael Rodero Monteagudo 220.50      30 Camion Pequeño
    ## 1489      500        Hector Aragones Frutos 125.00      60          Panel
    ## 1490      871      Ismael Rodero Monteagudo 217.75      60 Camion Pequeño
    ## 1491     1438          Pedro Alvarez Parejo 359.50      60  Camion Grande
    ## 1492      838                  Hector Giron 209.50      30 Camion Pequeño
    ## 1493      519          Angel Valdez Alegria 129.75      90 Camion Pequeño
    ## 1494      204              Felipe Villatoro  51.00      60          Panel
    ## 1495     1347       Fernando Mariano Berrio 336.75      90  Camion Grande
    ## 1496     1181      Ismael Rodero Monteagudo 295.25      90  Camion Grande
    ## 1497     1184                  Hector Giron 296.00      60  Camion Grande
    ## 1498      498 Juan Francisco Portillo Gomez 124.50      30          Panel
    ## 1499      232        Hector Aragones Frutos  58.00      90          Panel
    ## 1500     1175          Pedro Alvarez Parejo 293.75      60  Camion Grande
    ## 1501      361          Angel Valdez Alegria  90.25      90          Panel
    ## 1502     1793      Ismael Rodero Monteagudo 448.25      30  Camion Grande
    ## 1503     1224      Ismael Rodero Monteagudo 306.00      90  Camion Grande
    ## 1504     1572      Ismael Rodero Monteagudo 393.00      30  Camion Grande
    ## 1505     1026          Pedro Alvarez Parejo 256.50      60  Camion Grande
    ## 1506     1304          Pedro Alvarez Parejo 326.00      30  Camion Grande
    ## 1507     1899          Pedro Alvarez Parejo 474.75      90  Camion Grande
    ## 1508      945          Angel Valdez Alegria 236.25      30 Camion Pequeño
    ## 1509      473              Felipe Villatoro 118.25      90          Panel
    ## 1510     1149             Luis Jaime Urbano 287.25      60  Camion Grande
    ## 1511      421                  Hector Giron 105.25      30          Panel
    ## 1512      555             Luis Jaime Urbano 138.75      60 Camion Pequeño
    ## 1513      374              Felipe Villatoro  93.50      90          Panel
    ## 1514     1109          Pedro Alvarez Parejo 277.25      90  Camion Grande
    ## 1515     1196       Fernando Mariano Berrio 299.00      90  Camion Grande
    ## 1516      730       Fernando Mariano Berrio 182.50      30 Camion Pequeño
    ## 1517     1914          Pedro Alvarez Parejo 478.50      60  Camion Grande
    ## 1518      759        Hector Aragones Frutos 189.75      30 Camion Pequeño
    ## 1519     1432 Juan Francisco Portillo Gomez 358.00      90  Camion Grande
    ## 1520     1424                  Hector Giron 356.00      60  Camion Grande
    ## 1521     1414 Juan Francisco Portillo Gomez 353.50      60  Camion Grande
    ## 1522      504              Felipe Villatoro 126.00      90 Camion Pequeño
    ## 1523      361              Felipe Villatoro  90.25      60          Panel
    ## 1524     1141        Hector Aragones Frutos 285.25      90  Camion Grande
    ## 1525     1985          Pedro Alvarez Parejo 496.25      90  Camion Grande
    ## 1526     1599          Pedro Alvarez Parejo 399.75      30  Camion Grande
    ## 1527      245             Luis Jaime Urbano  61.25      30          Panel
    ## 1528      641        Hector Aragones Frutos 160.25      90 Camion Pequeño
    ## 1529      680              Felipe Villatoro 170.00      30 Camion Pequeño
    ## 1530      333          Pedro Alvarez Parejo  83.25      60          Panel
    ## 1531     1037 Juan Francisco Portillo Gomez 259.25      30  Camion Grande
    ## 1532     1946       Fernando Mariano Berrio 486.50      90  Camion Grande
    ## 1533     1447                  Hector Giron 361.75      90  Camion Grande
    ## 1534      282                  Hector Giron  70.50      30          Panel
    ## 1535      590        Hector Aragones Frutos 147.50      30 Camion Pequeño
    ## 1536      776       Fernando Mariano Berrio 194.00      30 Camion Pequeño
    ## 1537      992              Felipe Villatoro 248.00      90 Camion Pequeño
    ## 1538     1653      Ismael Rodero Monteagudo 413.25      60  Camion Grande
    ## 1539     1874          Angel Valdez Alegria 468.50      90  Camion Grande
    ## 1540      981          Angel Valdez Alegria 245.25      90 Camion Pequeño
    ## 1541     1782          Angel Valdez Alegria 445.50      60  Camion Grande
    ## 1542      514                  Hector Giron 128.50      30 Camion Pequeño
    ## 1543      650          Angel Valdez Alegria 162.50      30 Camion Pequeño
    ## 1544      874      Ismael Rodero Monteagudo 218.50      90 Camion Pequeño
    ## 1545      583             Luis Jaime Urbano 145.75      60 Camion Pequeño
    ## 1546      507             Luis Jaime Urbano 126.75      30 Camion Pequeño
    ## 1547     1577       Fernando Mariano Berrio 394.25      90  Camion Grande
    ## 1548     1290             Luis Jaime Urbano 322.50      90  Camion Grande
    ## 1549     1601 Juan Francisco Portillo Gomez 400.25      60  Camion Grande
    ## 1550     1689          Pedro Alvarez Parejo 422.25      60  Camion Grande
    ## 1551      899        Hector Aragones Frutos 224.75      30 Camion Pequeño
    ## 1552      845          Angel Valdez Alegria 211.25      90 Camion Pequeño
    ## 1553     1628       Fernando Mariano Berrio 407.00      30  Camion Grande
    ## 1554      413       Fernando Mariano Berrio 103.25      60          Panel
    ## 1555     1832       Fernando Mariano Berrio 458.00      30  Camion Grande
    ## 1556      884       Fernando Mariano Berrio 221.00      30 Camion Pequeño
    ## 1557      516          Angel Valdez Alegria 129.00      90 Camion Pequeño
    ## 1558      671             Luis Jaime Urbano 167.75      30 Camion Pequeño
    ## 1559      573              Felipe Villatoro 143.25      90 Camion Pequeño
    ## 1560     1622          Angel Valdez Alegria 405.50      60  Camion Grande
    ## 1561      567          Pedro Alvarez Parejo 141.75      30 Camion Pequeño
    ## 1562      288              Felipe Villatoro  72.00      60          Panel
    ## 1563      686              Felipe Villatoro 171.50      90 Camion Pequeño
    ## 1564      209              Felipe Villatoro  52.25      30          Panel
    ## 1565     1760              Felipe Villatoro 440.00      30  Camion Grande
    ## 1566      214      Ismael Rodero Monteagudo  53.50      90          Panel
    ## 1567     1072              Felipe Villatoro 268.00      90  Camion Grande
    ## 1568     1870        Hector Aragones Frutos 467.50      30  Camion Grande
    ## 1569     1028          Pedro Alvarez Parejo 257.00      30  Camion Grande
    ## 1570     1930 Juan Francisco Portillo Gomez 482.50      60  Camion Grande
    ## 1571     1540          Angel Valdez Alegria 385.00      30  Camion Grande
    ## 1572     1859              Felipe Villatoro 464.75      60  Camion Grande
    ## 1573      527          Pedro Alvarez Parejo 131.75      90 Camion Pequeño
    ## 1574      442          Angel Valdez Alegria 110.50      30          Panel
    ## 1575     1086              Felipe Villatoro 271.50      60  Camion Grande
    ## 1576      686          Pedro Alvarez Parejo 171.50      60 Camion Pequeño
    ## 1577     1585      Ismael Rodero Monteagudo 396.25      60  Camion Grande
    ## 1578      287       Fernando Mariano Berrio  71.75      30          Panel
    ## 1579     1559        Hector Aragones Frutos 389.75      60  Camion Grande
    ## 1580      877                  Hector Giron 219.25      30 Camion Pequeño
    ## 1581      380        Hector Aragones Frutos  95.00      90          Panel
    ## 1582      596        Hector Aragones Frutos 149.00      90 Camion Pequeño
    ## 1583     1232              Felipe Villatoro 308.00      60  Camion Grande
    ## 1584     1836                  Hector Giron 459.00      30  Camion Grande
    ## 1585      594              Felipe Villatoro 148.50      30 Camion Pequeño
    ## 1586     1969       Fernando Mariano Berrio 492.25      90  Camion Grande
    ## 1587     1413       Fernando Mariano Berrio 353.25      60  Camion Grande
    ## 1588     1126       Fernando Mariano Berrio 281.50      90  Camion Grande
    ## 1589      248          Pedro Alvarez Parejo  62.00      90          Panel
    ## 1590     1508       Fernando Mariano Berrio 377.00      30  Camion Grande
    ## 1591     1555              Felipe Villatoro 388.75      60  Camion Grande
    ## 1592     1526      Ismael Rodero Monteagudo 381.50      90  Camion Grande
    ## 1593      907       Fernando Mariano Berrio 226.75      60 Camion Pequeño
    ## 1594     1664      Ismael Rodero Monteagudo 416.00      30  Camion Grande
    ## 1595      567       Fernando Mariano Berrio 141.75      60 Camion Pequeño
    ## 1596     1617             Luis Jaime Urbano 404.25      90  Camion Grande
    ## 1597     1407              Felipe Villatoro 351.75      90  Camion Grande
    ## 1598     1006      Ismael Rodero Monteagudo 251.50      90  Camion Grande
    ## 1599     1768        Hector Aragones Frutos 442.00      60  Camion Grande
    ## 1600     1989        Hector Aragones Frutos 497.25      60  Camion Grande
    ## 1601      784              Felipe Villatoro 196.00      30 Camion Pequeño
    ## 1602     1499        Hector Aragones Frutos 374.75      90  Camion Grande
    ## 1603      714             Luis Jaime Urbano 178.50      30 Camion Pequeño
    ## 1604      704                  Hector Giron 176.00      90 Camion Pequeño
    ## 1605      720        Hector Aragones Frutos 180.00      90 Camion Pequeño
    ## 1606     1537              Felipe Villatoro 384.25      30  Camion Grande
    ## 1607      869              Felipe Villatoro 217.25      90 Camion Pequeño
    ## 1608     1974        Hector Aragones Frutos 493.50      90  Camion Grande
    ## 1609      686             Luis Jaime Urbano 171.50      90 Camion Pequeño
    ## 1610     1364          Angel Valdez Alegria 341.00      30  Camion Grande
    ## 1611     1204      Ismael Rodero Monteagudo 301.00      90  Camion Grande
    ## 1612     1466      Ismael Rodero Monteagudo 366.50      30  Camion Grande
    ## 1613     1646      Ismael Rodero Monteagudo 411.50      30  Camion Grande
    ## 1614      696        Hector Aragones Frutos 174.00      30 Camion Pequeño
    ## 1615      761          Angel Valdez Alegria 190.25      60 Camion Pequeño
    ## 1616      326          Pedro Alvarez Parejo  81.50      90          Panel
    ## 1617     1672      Ismael Rodero Monteagudo 418.00      30  Camion Grande
    ## 1618     1803                  Hector Giron 450.75      60  Camion Grande
    ## 1619     1036          Angel Valdez Alegria 259.00      90  Camion Grande
    ## 1620     1729        Hector Aragones Frutos 432.25      60  Camion Grande
    ## 1621      640        Hector Aragones Frutos 160.00      30 Camion Pequeño
    ## 1622      768          Pedro Alvarez Parejo 192.00      30 Camion Pequeño
    ## 1623     1401      Ismael Rodero Monteagudo 350.25      90  Camion Grande
    ## 1624     1138             Luis Jaime Urbano 284.50      30  Camion Grande
    ## 1625      492             Luis Jaime Urbano 123.00      90          Panel
    ## 1626     1524          Angel Valdez Alegria 381.00      30  Camion Grande
    ## 1627     1688             Luis Jaime Urbano 422.00      60  Camion Grande
    ## 1628     1183        Hector Aragones Frutos 295.75      30  Camion Grande
    ## 1629     1004       Fernando Mariano Berrio 251.00      90  Camion Grande
    ## 1630      319          Pedro Alvarez Parejo  79.75      90          Panel
    ## 1631      360      Ismael Rodero Monteagudo  90.00      90          Panel
    ## 1632      257        Hector Aragones Frutos  64.25      90          Panel
    ## 1633      297 Juan Francisco Portillo Gomez  74.25      30          Panel
    ## 1634      269          Pedro Alvarez Parejo  67.25      60          Panel
    ## 1635      242          Angel Valdez Alegria  60.50      60          Panel
    ## 1636      957       Fernando Mariano Berrio 239.25      30 Camion Pequeño
    ## 1637     1198        Hector Aragones Frutos 299.50      30  Camion Grande
    ## 1638     1454       Fernando Mariano Berrio 363.50      90  Camion Grande
    ## 1639     1963          Angel Valdez Alegria 490.75      30  Camion Grande
    ## 1640     1522                  Hector Giron 380.50      60  Camion Grande
    ## 1641     1261          Pedro Alvarez Parejo 315.25      90  Camion Grande
    ## 1642      660             Luis Jaime Urbano 165.00      90 Camion Pequeño
    ## 1643     1585       Fernando Mariano Berrio 396.25      30  Camion Grande
    ## 1644     1725      Ismael Rodero Monteagudo 431.25      90  Camion Grande
    ## 1645      843 Juan Francisco Portillo Gomez 210.75      60 Camion Pequeño
    ## 1646     1126          Pedro Alvarez Parejo 281.50      30  Camion Grande
    ## 1647      883        Hector Aragones Frutos 220.75      30 Camion Pequeño
    ## 1648      255          Angel Valdez Alegria  63.75      30          Panel
    ## 1649      409          Pedro Alvarez Parejo 102.25      90          Panel
    ## 1650     1793             Luis Jaime Urbano 448.25      30  Camion Grande
    ## 1651      984        Hector Aragones Frutos 246.00      30 Camion Pequeño
    ## 1652      245          Pedro Alvarez Parejo  61.25      30          Panel
    ## 1653     1113 Juan Francisco Portillo Gomez 278.25      60  Camion Grande
    ## 1654     1278       Fernando Mariano Berrio 319.50      60  Camion Grande
    ## 1655      531                  Hector Giron 132.75      90 Camion Pequeño
    ## 1656      941             Luis Jaime Urbano 235.25      30 Camion Pequeño
    ## 1657      911          Pedro Alvarez Parejo 227.75      30 Camion Pequeño
    ## 1658      342                  Hector Giron  85.50      30          Panel
    ## 1659     1957        Hector Aragones Frutos 489.25      90  Camion Grande
    ## 1660      514                  Hector Giron 128.50      60 Camion Pequeño
    ## 1661      636             Luis Jaime Urbano 159.00      60 Camion Pequeño
    ## 1662      563          Angel Valdez Alegria 140.75      90 Camion Pequeño
    ## 1663     1346          Pedro Alvarez Parejo 336.50      30  Camion Grande
    ## 1664      805          Angel Valdez Alegria 201.25      60 Camion Pequeño
    ## 1665      217        Hector Aragones Frutos  54.25      90          Panel
    ## 1666      901        Hector Aragones Frutos 225.25      30 Camion Pequeño
    ## 1667      491              Felipe Villatoro 122.75      90          Panel
    ## 1668     1723          Angel Valdez Alegria 430.75      90  Camion Grande
    ## 1669     1209       Fernando Mariano Berrio 302.25      60  Camion Grande
    ## 1670     1565        Hector Aragones Frutos 391.25      90  Camion Grande
    ## 1671     1866 Juan Francisco Portillo Gomez 466.50      30  Camion Grande
    ## 1672     1589              Felipe Villatoro 397.25      90  Camion Grande
    ## 1673      763             Luis Jaime Urbano 190.75      30 Camion Pequeño
    ## 1674     1517          Pedro Alvarez Parejo 379.25      30  Camion Grande
    ## 1675     1140          Angel Valdez Alegria 285.00      90  Camion Grande
    ## 1676     1822          Pedro Alvarez Parejo 455.50      90  Camion Grande
    ## 1677      653              Felipe Villatoro 163.25      30 Camion Pequeño
    ## 1678      465       Fernando Mariano Berrio 116.25      30          Panel
    ## 1679     1776                  Hector Giron 444.00      90  Camion Grande
    ## 1680     1211             Luis Jaime Urbano 302.75      60  Camion Grande
    ## 1681      352        Hector Aragones Frutos  88.00      90          Panel
    ## 1682      590       Fernando Mariano Berrio 147.50      90 Camion Pequeño
    ## 1683     1728                  Hector Giron 432.00      60  Camion Grande
    ## 1684     1240       Fernando Mariano Berrio 310.00      90  Camion Grande
    ## 1685     1832             Luis Jaime Urbano 458.00      60  Camion Grande
    ## 1686      545          Angel Valdez Alegria 136.25      60 Camion Pequeño
    ## 1687     1695          Pedro Alvarez Parejo 423.75      60  Camion Grande
    ## 1688      643       Fernando Mariano Berrio 160.75      30 Camion Pequeño
    ## 1689      427          Angel Valdez Alegria 106.75      60          Panel
    ## 1690     1694          Pedro Alvarez Parejo 423.50      30  Camion Grande
    ## 1691     1083                  Hector Giron 270.75      60  Camion Grande
    ## 1692     1669       Fernando Mariano Berrio 417.25      60  Camion Grande
    ## 1693     1169        Hector Aragones Frutos 292.25      30  Camion Grande
    ## 1694     1853 Juan Francisco Portillo Gomez 463.25      30  Camion Grande
    ## 1695     1844        Hector Aragones Frutos 461.00      30  Camion Grande
    ## 1696     1740              Felipe Villatoro 435.00      60  Camion Grande
    ## 1697     1834             Luis Jaime Urbano 458.50      60  Camion Grande
    ## 1698      276      Ismael Rodero Monteagudo  69.00      30          Panel
    ## 1699      241       Fernando Mariano Berrio  60.25      30          Panel
    ## 1700     1492          Angel Valdez Alegria 373.00      30  Camion Grande
    ## 1701      782        Hector Aragones Frutos 195.50      30 Camion Pequeño
    ## 1702     1680          Angel Valdez Alegria 420.00      90  Camion Grande
    ## 1703      292                  Hector Giron  73.00      90          Panel
    ## 1704      904        Hector Aragones Frutos 226.00      30 Camion Pequeño
    ## 1705      572                  Hector Giron 143.00      90 Camion Pequeño
    ## 1706      680        Hector Aragones Frutos 170.00      60 Camion Pequeño
    ## 1707     1339              Felipe Villatoro 334.75      30  Camion Grande
    ## 1708     1133      Ismael Rodero Monteagudo 283.25      60  Camion Grande
    ## 1709      557             Luis Jaime Urbano 139.25      30 Camion Pequeño
    ## 1710     1943       Fernando Mariano Berrio 485.75      30  Camion Grande
    ## 1711      432       Fernando Mariano Berrio 108.00      60          Panel
    ## 1712     1990                  Hector Giron 497.50      30  Camion Grande
    ## 1713      814 Juan Francisco Portillo Gomez 203.50      30 Camion Pequeño
    ## 1714      998        Hector Aragones Frutos 249.50      60 Camion Pequeño
    ## 1715     1094             Luis Jaime Urbano 273.50      30  Camion Grande
    ## 1716     1624          Pedro Alvarez Parejo 406.00      30  Camion Grande
    ## 1717     1117 Juan Francisco Portillo Gomez 279.25      30  Camion Grande
    ## 1718     1720             Luis Jaime Urbano 430.00      90  Camion Grande
    ## 1719      892          Pedro Alvarez Parejo 223.00      90 Camion Pequeño
    ## 1720     1089              Felipe Villatoro 272.25      60  Camion Grande
    ## 1721     1949       Fernando Mariano Berrio 487.25      30  Camion Grande
    ## 1722     1632      Ismael Rodero Monteagudo 408.00      30  Camion Grande
    ## 1723     1273        Hector Aragones Frutos 318.25      30  Camion Grande
    ## 1724      955      Ismael Rodero Monteagudo 238.75      30 Camion Pequeño
    ## 1725      822          Pedro Alvarez Parejo 205.50      30 Camion Pequeño
    ## 1726      522          Pedro Alvarez Parejo 130.50      90 Camion Pequeño
    ## 1727     1372 Juan Francisco Portillo Gomez 343.00      90  Camion Grande
    ## 1728      700              Felipe Villatoro 175.00      30 Camion Pequeño
    ## 1729     1247          Pedro Alvarez Parejo 311.75      90  Camion Grande
    ## 1730     1940          Pedro Alvarez Parejo 485.00      60  Camion Grande
    ## 1731      700              Felipe Villatoro 175.00      30 Camion Pequeño
    ## 1732     1891          Pedro Alvarez Parejo 472.75      30  Camion Grande
    ## 1733     1297 Juan Francisco Portillo Gomez 324.25      90  Camion Grande
    ## 1734      275        Hector Aragones Frutos  68.75      30          Panel
    ## 1735      325             Luis Jaime Urbano  81.25      30          Panel
    ## 1736     1706              Felipe Villatoro 426.50      30  Camion Grande
    ## 1737      553             Luis Jaime Urbano 138.25      90 Camion Pequeño
    ## 1738      429             Luis Jaime Urbano 107.25      30          Panel
    ## 1739     1416          Angel Valdez Alegria 354.00      90  Camion Grande
    ## 1740     1138        Hector Aragones Frutos 284.50      60  Camion Grande
    ## 1741      332          Angel Valdez Alegria  83.00      60          Panel
    ## 1742      435          Pedro Alvarez Parejo 108.75      60          Panel
    ## 1743     1330 Juan Francisco Portillo Gomez 332.50      60  Camion Grande
    ## 1744     1911                  Hector Giron 477.75      90  Camion Grande
    ## 1745      429                  Hector Giron 107.25      60          Panel
    ## 1746     1711      Ismael Rodero Monteagudo 427.75      90  Camion Grande
    ## 1747      855              Felipe Villatoro 213.75      90 Camion Pequeño
    ## 1748     1443                  Hector Giron 360.75      90  Camion Grande
    ## 1749      594             Luis Jaime Urbano 148.50      90 Camion Pequeño
    ## 1750      671       Fernando Mariano Berrio 167.75      30 Camion Pequeño
    ## 1751      250       Fernando Mariano Berrio  62.50      30          Panel
    ## 1752     1515             Luis Jaime Urbano 378.75      60  Camion Grande
    ## 1753      526                  Hector Giron 131.50      30 Camion Pequeño
    ## 1754      451          Angel Valdez Alegria 112.75      30          Panel
    ## 1755     1255       Fernando Mariano Berrio 313.75      60  Camion Grande
    ## 1756      907          Angel Valdez Alegria 226.75      90 Camion Pequeño
    ## 1757     1959                  Hector Giron 489.75      90  Camion Grande
    ## 1758      844          Angel Valdez Alegria 211.00      90 Camion Pequeño
    ## 1759      815             Luis Jaime Urbano 203.75      60 Camion Pequeño
    ## 1760     1929       Fernando Mariano Berrio 482.25      60  Camion Grande
    ## 1761     1233          Pedro Alvarez Parejo 308.25      60  Camion Grande
    ## 1762      328          Pedro Alvarez Parejo  82.00      90          Panel
    ## 1763     1224              Felipe Villatoro 306.00      30  Camion Grande
    ## 1764     1186 Juan Francisco Portillo Gomez 296.50      90  Camion Grande
    ## 1765     1945 Juan Francisco Portillo Gomez 486.25      30  Camion Grande
    ## 1766     1464       Fernando Mariano Berrio 366.00      90  Camion Grande
    ## 1767     1383        Hector Aragones Frutos 345.75      60  Camion Grande
    ## 1768      641                  Hector Giron 160.25      90 Camion Pequeño
    ## 1769      430       Fernando Mariano Berrio 107.50      60          Panel
    ## 1770     1741                  Hector Giron 435.25      30  Camion Grande
    ## 1771     1820              Felipe Villatoro 455.00      60  Camion Grande
    ## 1772     1555              Felipe Villatoro 388.75      30  Camion Grande
    ## 1773     1754      Ismael Rodero Monteagudo 438.50      30  Camion Grande
    ## 1774     1473      Ismael Rodero Monteagudo 368.25      30  Camion Grande
    ## 1775     1632                  Hector Giron 408.00      30  Camion Grande
    ## 1776     1470                  Hector Giron 367.50      90  Camion Grande
    ## 1777     1562 Juan Francisco Portillo Gomez 390.50      60  Camion Grande
    ## 1778      293 Juan Francisco Portillo Gomez  73.25      30          Panel
    ## 1779     1053          Angel Valdez Alegria 263.25      30  Camion Grande
    ## 1780      525          Angel Valdez Alegria 131.25      60 Camion Pequeño
    ## 1781      618                  Hector Giron 154.50      30 Camion Pequeño
    ## 1782      654      Ismael Rodero Monteagudo 163.50      30 Camion Pequeño
    ## 1783      923                  Hector Giron 230.75      60 Camion Pequeño
    ## 1784      511      Ismael Rodero Monteagudo 127.75      30 Camion Pequeño
    ## 1785      349             Luis Jaime Urbano  87.25      60          Panel
    ## 1786     1278              Felipe Villatoro 319.50      60  Camion Grande
    ## 1787      356          Angel Valdez Alegria  89.00      90          Panel
    ## 1788     1427          Angel Valdez Alegria 356.75      30  Camion Grande
    ## 1789     1949          Angel Valdez Alegria 487.25      90  Camion Grande
    ## 1790     1586          Pedro Alvarez Parejo 396.50      60  Camion Grande
    ## 1791     1368              Felipe Villatoro 342.00      60  Camion Grande
    ## 1792      568                  Hector Giron 142.00      60 Camion Pequeño
    ## 1793      864      Ismael Rodero Monteagudo 216.00      60 Camion Pequeño
    ## 1794     1219          Angel Valdez Alegria 304.75      60  Camion Grande
    ## 1795      425              Felipe Villatoro 106.25      90          Panel
    ## 1796     1006        Hector Aragones Frutos 251.50      30  Camion Grande
    ## 1797     1882      Ismael Rodero Monteagudo 470.50      60  Camion Grande
    ## 1798      249 Juan Francisco Portillo Gomez  62.25      60          Panel
    ## 1799     1323        Hector Aragones Frutos 330.75      90  Camion Grande
    ## 1800     1236 Juan Francisco Portillo Gomez 309.00      30  Camion Grande
    ## 1801     1161          Pedro Alvarez Parejo 290.25      90  Camion Grande
    ## 1802     1453                  Hector Giron 363.25      60  Camion Grande
    ## 1803      822              Felipe Villatoro 205.50      90 Camion Pequeño
    ## 1804     1230       Fernando Mariano Berrio 307.50      30  Camion Grande
    ## 1805      702          Pedro Alvarez Parejo 175.50      90 Camion Pequeño
    ## 1806     1387          Angel Valdez Alegria 346.75      90  Camion Grande
    ## 1807      412        Hector Aragones Frutos 103.00      60          Panel
    ## 1808     1533      Ismael Rodero Monteagudo 383.25      30  Camion Grande
    ## 1809     1945        Hector Aragones Frutos 486.25      90  Camion Grande
    ## 1810      571 Juan Francisco Portillo Gomez 142.75      30 Camion Pequeño
    ## 1811     1483              Felipe Villatoro 370.75      30  Camion Grande
    ## 1812     1826                  Hector Giron 456.50      90  Camion Grande
    ## 1813     1607          Pedro Alvarez Parejo 401.75      90  Camion Grande
    ## 1814     1527       Fernando Mariano Berrio 381.75      30  Camion Grande
    ## 1815     1282              Felipe Villatoro 320.50      30  Camion Grande
    ## 1816      989          Pedro Alvarez Parejo 247.25      60 Camion Pequeño
    ## 1817     1439                  Hector Giron 359.75      90  Camion Grande
    ## 1818      532              Felipe Villatoro 133.00      30 Camion Pequeño
    ## 1819      242             Luis Jaime Urbano  60.50      60          Panel
    ## 1820     1950          Pedro Alvarez Parejo 487.50      90  Camion Grande
    ## 1821     1149        Hector Aragones Frutos 287.25      60  Camion Grande
    ## 1822     1813 Juan Francisco Portillo Gomez 453.25      90  Camion Grande
    ## 1823      572              Felipe Villatoro 143.00      90 Camion Pequeño
    ## 1824      820          Pedro Alvarez Parejo 205.00      30 Camion Pequeño
    ## 1825     1105                  Hector Giron 276.25      60  Camion Grande
    ## 1826     1604        Hector Aragones Frutos 401.00      90  Camion Grande
    ## 1827      371             Luis Jaime Urbano  92.75      60          Panel
    ## 1828     1365       Fernando Mariano Berrio 341.25      60  Camion Grande
    ## 1829     1796       Fernando Mariano Berrio 449.00      30  Camion Grande
    ## 1830     1443                  Hector Giron 360.75      60  Camion Grande
    ## 1831     1728          Angel Valdez Alegria 432.00      30  Camion Grande
    ## 1832      884       Fernando Mariano Berrio 221.00      60 Camion Pequeño
    ## 1833     1135          Angel Valdez Alegria 283.75      30  Camion Grande
    ## 1834      714          Angel Valdez Alegria 178.50      30 Camion Pequeño
    ## 1835     1342        Hector Aragones Frutos 335.50      30  Camion Grande
    ## 1836      549      Ismael Rodero Monteagudo 137.25      30 Camion Pequeño
    ## 1837     1303              Felipe Villatoro 325.75      30  Camion Grande
    ## 1838     1437          Pedro Alvarez Parejo 359.25      60  Camion Grande
    ## 1839      257      Ismael Rodero Monteagudo  64.25      60          Panel
    ## 1840      429       Fernando Mariano Berrio 107.25      60          Panel
    ## 1841     1231          Pedro Alvarez Parejo 307.75      60  Camion Grande
    ## 1842     1434          Pedro Alvarez Parejo 358.50      60  Camion Grande
    ## 1843     1000              Felipe Villatoro 250.00      90 Camion Pequeño
    ## 1844     1148      Ismael Rodero Monteagudo 287.00      90  Camion Grande
    ## 1845     1347        Hector Aragones Frutos 336.75      60  Camion Grande
    ## 1846      802        Hector Aragones Frutos 200.50      30 Camion Pequeño
    ## 1847     1919        Hector Aragones Frutos 479.75      30  Camion Grande
    ## 1848      717 Juan Francisco Portillo Gomez 179.25      60 Camion Pequeño
    ## 1849      838       Fernando Mariano Berrio 209.50      90 Camion Pequeño
    ## 1850      324       Fernando Mariano Berrio  81.00      90          Panel
    ## 1851     1309      Ismael Rodero Monteagudo 327.25      60  Camion Grande
    ## 1852     1333                  Hector Giron 333.25      30  Camion Grande
    ## 1853     1496             Luis Jaime Urbano 374.00      90  Camion Grande
    ## 1854      327              Felipe Villatoro  81.75      30          Panel
    ## 1855     1662        Hector Aragones Frutos 415.50      30  Camion Grande
    ## 1856     1479      Ismael Rodero Monteagudo 369.75      30  Camion Grande
    ## 1857      939      Ismael Rodero Monteagudo 234.75      60 Camion Pequeño
    ## 1858      429              Felipe Villatoro 107.25      30          Panel
    ## 1859     1734                  Hector Giron 433.50      90  Camion Grande
    ## 1860      667 Juan Francisco Portillo Gomez 166.75      90 Camion Pequeño
    ## 1861     1054       Fernando Mariano Berrio 263.50      60  Camion Grande
    ## 1862      913              Felipe Villatoro 228.25      30 Camion Pequeño
    ## 1863     1583        Hector Aragones Frutos 395.75      90  Camion Grande
    ## 1864     1067                  Hector Giron 266.75      90  Camion Grande
    ## 1865     1206       Fernando Mariano Berrio 301.50      30  Camion Grande
    ## 1866     1366          Angel Valdez Alegria 341.50      30  Camion Grande
    ## 1867     1651       Fernando Mariano Berrio 412.75      90  Camion Grande
    ## 1868      481              Felipe Villatoro 120.25      90          Panel
    ## 1869      975      Ismael Rodero Monteagudo 243.75      30 Camion Pequeño
    ## 1870      981              Felipe Villatoro 245.25      90 Camion Pequeño
    ## 1871      558                  Hector Giron 139.50      30 Camion Pequeño
    ## 1872      476          Pedro Alvarez Parejo 119.00      30          Panel
    ## 1873      791       Fernando Mariano Berrio 197.75      60 Camion Pequeño
    ## 1874      353          Angel Valdez Alegria  88.25      60          Panel
    ## 1875     1812          Angel Valdez Alegria 453.00      30  Camion Grande
    ## 1876      884              Felipe Villatoro 221.00      60 Camion Pequeño
    ## 1877     1055             Luis Jaime Urbano 263.75      30  Camion Grande
    ## 1878      831             Luis Jaime Urbano 207.75      60 Camion Pequeño
    ## 1879      623             Luis Jaime Urbano 155.75      60 Camion Pequeño
    ## 1880      488        Hector Aragones Frutos 122.00      30          Panel
    ## 1881      980          Angel Valdez Alegria 245.00      90 Camion Pequeño
    ## 1882     1772                  Hector Giron 443.00      30  Camion Grande
    ## 1883     1164       Fernando Mariano Berrio 291.00      60  Camion Grande
    ## 1884      695          Angel Valdez Alegria 173.75      90 Camion Pequeño
    ## 1885      904          Pedro Alvarez Parejo 226.00      30 Camion Pequeño
    ## 1886     1022          Angel Valdez Alegria 255.50      90  Camion Grande
    ## 1887      671             Luis Jaime Urbano 167.75      60 Camion Pequeño
    ## 1888      523          Pedro Alvarez Parejo 130.75      60 Camion Pequeño
    ## 1889      988             Luis Jaime Urbano 247.00      90 Camion Pequeño
    ## 1890      550 Juan Francisco Portillo Gomez 137.50      30 Camion Pequeño
    ## 1891     1314 Juan Francisco Portillo Gomez 328.50      60  Camion Grande
    ## 1892      956 Juan Francisco Portillo Gomez 239.00      60 Camion Pequeño
    ## 1893     1432       Fernando Mariano Berrio 358.00      90  Camion Grande
    ## 1894      805          Pedro Alvarez Parejo 201.25      60 Camion Pequeño
    ## 1895     1710      Ismael Rodero Monteagudo 427.50      60  Camion Grande
    ## 1896     1496 Juan Francisco Portillo Gomez 374.00      60  Camion Grande
    ## 1897     1771              Felipe Villatoro 442.75      60  Camion Grande
    ## 1898     1416 Juan Francisco Portillo Gomez 354.00      30  Camion Grande
    ## 1899      237              Felipe Villatoro  59.25      60          Panel
    ## 1900     1771          Angel Valdez Alegria 442.75      60  Camion Grande
    ## 1901      625       Fernando Mariano Berrio 156.25      30 Camion Pequeño
    ## 1902      254          Pedro Alvarez Parejo  63.50      30          Panel
    ## 1903      789      Ismael Rodero Monteagudo 197.25      30 Camion Pequeño
    ## 1904     1191        Hector Aragones Frutos 297.75      90  Camion Grande
    ## 1905     1947        Hector Aragones Frutos 486.75      60  Camion Grande
    ## 1906     1121                  Hector Giron 280.25      30  Camion Grande
    ## 1907     1960      Ismael Rodero Monteagudo 490.00      60  Camion Grande
    ## 1908     1272          Pedro Alvarez Parejo 318.00      90  Camion Grande
    ## 1909      889                  Hector Giron 222.25      60 Camion Pequeño
    ## 1910      709          Pedro Alvarez Parejo 177.25      60 Camion Pequeño
    ## 1911      981              Felipe Villatoro 245.25      90 Camion Pequeño
    ## 1912      840      Ismael Rodero Monteagudo 210.00      90 Camion Pequeño
    ## 1913     1145              Felipe Villatoro 286.25      60  Camion Grande
    ## 1914      893                  Hector Giron 223.25      30 Camion Pequeño
    ## 1915     1674          Pedro Alvarez Parejo 418.50      30  Camion Grande
    ## 1916     1110       Fernando Mariano Berrio 277.50      90  Camion Grande
    ## 1917     1341       Fernando Mariano Berrio 335.25      60  Camion Grande
    ## 1918      999             Luis Jaime Urbano 249.75      60 Camion Pequeño
    ## 1919      628      Ismael Rodero Monteagudo 157.00      90 Camion Pequeño
    ## 1920      356             Luis Jaime Urbano  89.00      90          Panel
    ## 1921     1596              Felipe Villatoro 399.00      30  Camion Grande
    ## 1922     1855      Ismael Rodero Monteagudo 463.75      60  Camion Grande
    ## 1923     1262          Pedro Alvarez Parejo 315.50      90  Camion Grande
    ## 1924      620      Ismael Rodero Monteagudo 155.00      60 Camion Pequeño
    ## 1925      647          Angel Valdez Alegria 161.75      30 Camion Pequeño
    ## 1926      417          Pedro Alvarez Parejo 104.25      60          Panel
    ## 1927     1865             Luis Jaime Urbano 466.25      90  Camion Grande
    ## 1928      889       Fernando Mariano Berrio 222.25      90 Camion Pequeño
    ## 1929      821             Luis Jaime Urbano 205.25      90 Camion Pequeño
    ## 1930      918 Juan Francisco Portillo Gomez 229.50      90 Camion Pequeño
    ## 1931     1399                  Hector Giron 349.75      60  Camion Grande
    ## 1932      597             Luis Jaime Urbano 149.25      60 Camion Pequeño
    ## 1933      638      Ismael Rodero Monteagudo 159.50      30 Camion Pequeño
    ## 1934     1921      Ismael Rodero Monteagudo 480.25      60  Camion Grande
    ## 1935      284 Juan Francisco Portillo Gomez  71.00      90          Panel
    ## 1936      387          Pedro Alvarez Parejo  96.75      60          Panel
    ## 1937      374                  Hector Giron  93.50      30          Panel
    ## 1938     1878          Pedro Alvarez Parejo 469.50      30  Camion Grande
    ## 1939     1969        Hector Aragones Frutos 492.25      60  Camion Grande
    ## 1940     1199             Luis Jaime Urbano 299.75      60  Camion Grande
    ## 1941     1480          Pedro Alvarez Parejo 370.00      90  Camion Grande
    ## 1942     1809          Angel Valdez Alegria 452.25      60  Camion Grande
    ## 1943     1058             Luis Jaime Urbano 264.50      60  Camion Grande
    ## 1944      708             Luis Jaime Urbano 177.00      90 Camion Pequeño
    ## 1945      355          Pedro Alvarez Parejo  88.75      60          Panel
    ## 1946      374       Fernando Mariano Berrio  93.50      90          Panel
    ## 1947     1641                  Hector Giron 410.25      90  Camion Grande
    ## 1948     1771        Hector Aragones Frutos 442.75      30  Camion Grande
    ## 1949     1166 Juan Francisco Portillo Gomez 291.50      60  Camion Grande
    ## 1950      691       Fernando Mariano Berrio 172.75      60 Camion Pequeño
    ## 1951     1658        Hector Aragones Frutos 414.50      30  Camion Grande
    ## 1952      670          Pedro Alvarez Parejo 167.50      30 Camion Pequeño
    ## 1953      832                  Hector Giron 208.00      90 Camion Pequeño
    ## 1954      205             Luis Jaime Urbano  51.25      60          Panel
    ## 1955     1562          Pedro Alvarez Parejo 390.50      90  Camion Grande
    ## 1956      980          Pedro Alvarez Parejo 245.00      60 Camion Pequeño
    ## 1957     1297          Angel Valdez Alegria 324.25      90  Camion Grande
    ## 1958     1817      Ismael Rodero Monteagudo 454.25      30  Camion Grande
    ## 1959      676 Juan Francisco Portillo Gomez 169.00      90 Camion Pequeño
    ## 1960     1880             Luis Jaime Urbano 470.00      30  Camion Grande
    ## 1961      578       Fernando Mariano Berrio 144.50      60 Camion Pequeño
    ## 1962     1762          Angel Valdez Alegria 440.50      30  Camion Grande
    ## 1963     1737              Felipe Villatoro 434.25      60  Camion Grande
    ## 1964     1819              Felipe Villatoro 454.75      30  Camion Grande
    ## 1965      508              Felipe Villatoro 127.00      90 Camion Pequeño
    ## 1966      446      Ismael Rodero Monteagudo 111.50      90          Panel
    ## 1967      597        Hector Aragones Frutos 149.25      30 Camion Pequeño
    ## 1968     1771      Ismael Rodero Monteagudo 442.75      90  Camion Grande
    ## 1969     1808       Fernando Mariano Berrio 452.00      30  Camion Grande
    ## 1970     1720              Felipe Villatoro 430.00      30  Camion Grande
    ## 1971     1483      Ismael Rodero Monteagudo 370.75      30  Camion Grande
    ## 1972     1155          Pedro Alvarez Parejo 288.75      90  Camion Grande
    ## 1973     1011 Juan Francisco Portillo Gomez 252.75      90  Camion Grande
    ## 1974     1228             Luis Jaime Urbano 307.00      30  Camion Grande
    ## 1975     1751 Juan Francisco Portillo Gomez 437.75      90  Camion Grande
    ## 1976     1874                  Hector Giron 468.50      30  Camion Grande
    ## 1977     1679       Fernando Mariano Berrio 419.75      60  Camion Grande
    ## 1978      945              Felipe Villatoro 236.25      90 Camion Pequeño
    ## 1979     1605              Felipe Villatoro 401.25      60  Camion Grande
    ## 1980     1931       Fernando Mariano Berrio 482.75      30  Camion Grande
    ## 1981      225        Hector Aragones Frutos  56.25      60          Panel
    ## 1982      263       Fernando Mariano Berrio  65.75      30          Panel
    ## 1983      967      Ismael Rodero Monteagudo 241.75      90 Camion Pequeño
    ## 1984     1639      Ismael Rodero Monteagudo 409.75      30  Camion Grande
    ## 1985     1811          Angel Valdez Alegria 452.75      60  Camion Grande
    ## 1986      676 Juan Francisco Portillo Gomez 169.00      60 Camion Pequeño
    ## 1987      566        Hector Aragones Frutos 141.50      90 Camion Pequeño
    ## 1988     1723      Ismael Rodero Monteagudo 430.75      30  Camion Grande
    ## 1989      873          Pedro Alvarez Parejo 218.25      90 Camion Pequeño
    ## 1990     1524          Pedro Alvarez Parejo 381.00      30  Camion Grande
    ## 1991      805          Pedro Alvarez Parejo 201.25      90 Camion Pequeño
    ## 1992     1382       Fernando Mariano Berrio 345.50      30  Camion Grande
    ## 1993     1430          Pedro Alvarez Parejo 357.50      60  Camion Grande
    ## 1994     1055              Felipe Villatoro 263.75      90  Camion Grande
    ## 1995     1862          Pedro Alvarez Parejo 465.50      60  Camion Grande
    ## 1996     1881       Fernando Mariano Berrio 470.25      30  Camion Grande
    ## 1997     1025          Angel Valdez Alegria 256.25      90  Camion Grande
    ## 1998      836              Felipe Villatoro 209.00      90 Camion Pequeño
    ## 1999      205          Pedro Alvarez Parejo  51.25      60          Panel
    ## 2000     1876 Juan Francisco Portillo Gomez 469.00      30  Camion Grande
    ## 2001     1065              Felipe Villatoro 266.25      60  Camion Grande
    ## 2002     1597          Pedro Alvarez Parejo 399.25      60  Camion Grande
    ## 2003     1606             Luis Jaime Urbano 401.50      90  Camion Grande
    ## 2004     1986        Hector Aragones Frutos 496.50      60  Camion Grande
    ## 2005     1233      Ismael Rodero Monteagudo 308.25      90  Camion Grande
    ## 2006     1757       Fernando Mariano Berrio 439.25      90  Camion Grande
    ## 2007      477 Juan Francisco Portillo Gomez 119.25      60          Panel
    ## 2008      531          Pedro Alvarez Parejo 132.75      30 Camion Pequeño
    ## 2009      826              Felipe Villatoro 206.50      30 Camion Pequeño
    ## 2010     1075        Hector Aragones Frutos 268.75      30  Camion Grande
    ## 2011     1556          Angel Valdez Alegria 389.00      90  Camion Grande
    ## 2012     1850 Juan Francisco Portillo Gomez 462.50      60  Camion Grande
    ## 2013      359          Pedro Alvarez Parejo  89.75      60          Panel
    ## 2014     1173          Angel Valdez Alegria 293.25      30  Camion Grande
    ## 2015     1883             Luis Jaime Urbano 470.75      90  Camion Grande
    ## 2016      289      Ismael Rodero Monteagudo  72.25      60          Panel
    ## 2017     1087 Juan Francisco Portillo Gomez 271.75      90  Camion Grande
    ## 2018      946       Fernando Mariano Berrio 236.50      30 Camion Pequeño
    ## 2019     1950          Pedro Alvarez Parejo 487.50      60  Camion Grande
    ## 2020      938       Fernando Mariano Berrio 234.50      60 Camion Pequeño
    ## 2021     1092      Ismael Rodero Monteagudo 273.00      60  Camion Grande
    ## 2022     1256             Luis Jaime Urbano 314.00      90  Camion Grande
    ## 2023      959        Hector Aragones Frutos 239.75      60 Camion Pequeño
    ## 2024     1699          Angel Valdez Alegria 424.75      60  Camion Grande
    ## 2025      612          Pedro Alvarez Parejo 153.00      30 Camion Pequeño
    ## 2026     1991              Felipe Villatoro 497.75      30  Camion Grande
    ## 2027     1105          Angel Valdez Alegria 276.25      30  Camion Grande
    ## 2028     1973      Ismael Rodero Monteagudo 493.25      60  Camion Grande
    ## 2029      768             Luis Jaime Urbano 192.00      30 Camion Pequeño
    ## 2030     1364       Fernando Mariano Berrio 341.00      90  Camion Grande
    ## 2031      838             Luis Jaime Urbano 209.50      60 Camion Pequeño
    ## 2032      641        Hector Aragones Frutos 160.25      90 Camion Pequeño
    ## 2033     1842          Angel Valdez Alegria 460.50      60  Camion Grande
    ## 2034      505                  Hector Giron 126.25      60 Camion Pequeño
    ## 2035     1817          Pedro Alvarez Parejo 454.25      90  Camion Grande
    ## 2036      890          Angel Valdez Alegria 222.50      30 Camion Pequeño
    ## 2037      466          Pedro Alvarez Parejo 116.50      30          Panel
    ## 2038      410          Pedro Alvarez Parejo 102.50      60          Panel
    ## 2039      599        Hector Aragones Frutos 149.75      30 Camion Pequeño
    ## 2040     1771      Ismael Rodero Monteagudo 442.75      60  Camion Grande
    ## 2041      632 Juan Francisco Portillo Gomez 158.00      60 Camion Pequeño
    ## 2042     1935      Ismael Rodero Monteagudo 483.75      60  Camion Grande
    ## 2043     1761       Fernando Mariano Berrio 440.25      60  Camion Grande
    ## 2044      952              Felipe Villatoro 238.00      90 Camion Pequeño
    ## 2045      640      Ismael Rodero Monteagudo 160.00      90 Camion Pequeño
    ## 2046     1103       Fernando Mariano Berrio 275.75      90  Camion Grande
    ## 2047     1577       Fernando Mariano Berrio 394.25      90  Camion Grande
    ## 2048     1778             Luis Jaime Urbano 444.50      30  Camion Grande
    ## 2049     1595       Fernando Mariano Berrio 398.75      60  Camion Grande
    ## 2050     1857              Felipe Villatoro 464.25      60  Camion Grande
    ## 2051      554                  Hector Giron 138.50      90 Camion Pequeño
    ## 2052     1501                  Hector Giron 375.25      90  Camion Grande
    ## 2053      966 Juan Francisco Portillo Gomez 241.50      90 Camion Pequeño
    ## 2054     1813             Luis Jaime Urbano 453.25      90  Camion Grande
    ## 2055     1419              Felipe Villatoro 354.75      30  Camion Grande
    ## 2056     1512              Felipe Villatoro 378.00      60  Camion Grande
    ## 2057      295        Hector Aragones Frutos  73.75      30          Panel
    ## 2058     1775                  Hector Giron 443.75      90  Camion Grande
    ## 2059      334          Pedro Alvarez Parejo  83.50      30          Panel
    ## 2060     1184          Angel Valdez Alegria 296.00      60  Camion Grande
    ## 2061     1873        Hector Aragones Frutos 468.25      30  Camion Grande
    ## 2062     1096          Pedro Alvarez Parejo 274.00      60  Camion Grande
    ## 2063     1387                  Hector Giron 346.75      90  Camion Grande
    ## 2064     1043             Luis Jaime Urbano 260.75      90  Camion Grande
    ## 2065      691        Hector Aragones Frutos 172.75      60 Camion Pequeño
    ## 2066     1731       Fernando Mariano Berrio 432.75      60  Camion Grande
    ## 2067     1804        Hector Aragones Frutos 451.00      60  Camion Grande
    ## 2068      200              Felipe Villatoro  50.00      60          Panel
    ## 2069      377              Felipe Villatoro  94.25      90          Panel
    ## 2070     1673          Pedro Alvarez Parejo 418.25      90  Camion Grande
    ## 2071     1640          Angel Valdez Alegria 410.00      60  Camion Grande
    ## 2072      623       Fernando Mariano Berrio 155.75      30 Camion Pequeño
    ## 2073      870       Fernando Mariano Berrio 217.50      30 Camion Pequeño
    ## 2074      233          Pedro Alvarez Parejo  58.25      30          Panel
    ## 2075      894       Fernando Mariano Berrio 223.50      30 Camion Pequeño
    ## 2076      207          Angel Valdez Alegria  51.75      60          Panel
    ## 2077     1472 Juan Francisco Portillo Gomez 368.00      30  Camion Grande
    ## 2078      643        Hector Aragones Frutos 160.75      30 Camion Pequeño
    ## 2079      961       Fernando Mariano Berrio 240.25      60 Camion Pequeño
    ## 2080     1557 Juan Francisco Portillo Gomez 389.25      60  Camion Grande
    ## 2081     1273          Angel Valdez Alegria 318.25      90  Camion Grande
    ## 2082      398              Felipe Villatoro  99.50      30          Panel
    ## 2083      986      Ismael Rodero Monteagudo 246.50      30 Camion Pequeño
    ## 2084     1569                  Hector Giron 392.25      90  Camion Grande
    ## 2085     1795       Fernando Mariano Berrio 448.75      30  Camion Grande
    ## 2086     1838 Juan Francisco Portillo Gomez 459.50      30  Camion Grande
    ## 2087     1151        Hector Aragones Frutos 287.75      30  Camion Grande
    ## 2088      304             Luis Jaime Urbano  76.00      90          Panel
    ## 2089      247                  Hector Giron  61.75      90          Panel
    ## 2090     1579             Luis Jaime Urbano 394.75      60  Camion Grande
    ## 2091     1727          Pedro Alvarez Parejo 431.75      90  Camion Grande
    ## 2092      801 Juan Francisco Portillo Gomez 200.25      60 Camion Pequeño
    ## 2093     1458       Fernando Mariano Berrio 364.50      30  Camion Grande
    ## 2094      687                  Hector Giron 171.75      60 Camion Pequeño
    ## 2095      503        Hector Aragones Frutos 125.75      30 Camion Pequeño
    ## 2096      683             Luis Jaime Urbano 170.75      30 Camion Pequeño
    ## 2097     1427                  Hector Giron 356.75      90  Camion Grande
    ## 2098     1013          Pedro Alvarez Parejo 253.25      30  Camion Grande
    ## 2099     1720          Angel Valdez Alegria 430.00      30  Camion Grande
    ## 2100      770                  Hector Giron 192.50      30 Camion Pequeño
    ## 2101     1934 Juan Francisco Portillo Gomez 483.50      90  Camion Grande
    ## 2102      743          Pedro Alvarez Parejo 185.75      60 Camion Pequeño
    ## 2103     1850              Felipe Villatoro 462.50      30  Camion Grande
    ## 2104      875              Felipe Villatoro 218.75      30 Camion Pequeño
    ## 2105     1919        Hector Aragones Frutos 479.75      90  Camion Grande
    ## 2106     1073       Fernando Mariano Berrio 268.25      30  Camion Grande
    ## 2107      902        Hector Aragones Frutos 225.50      60 Camion Pequeño
    ## 2108      203                  Hector Giron  50.75      60          Panel
    ## 2109      659       Fernando Mariano Berrio 164.75      60 Camion Pequeño
    ## 2110     1204              Felipe Villatoro 301.00      30  Camion Grande
    ## 2111     1967             Luis Jaime Urbano 491.75      30  Camion Grande
    ## 2112     1837      Ismael Rodero Monteagudo 459.25      90  Camion Grande
    ## 2113      681          Pedro Alvarez Parejo 170.25      90 Camion Pequeño
    ## 2114      451 Juan Francisco Portillo Gomez 112.75      30          Panel
    ## 2115     1670             Luis Jaime Urbano 417.50      90  Camion Grande
    ## 2116     1986        Hector Aragones Frutos 496.50      60  Camion Grande
    ## 2117     1091        Hector Aragones Frutos 272.75      90  Camion Grande
    ## 2118     1302          Angel Valdez Alegria 325.50      60  Camion Grande
    ## 2119     1779      Ismael Rodero Monteagudo 444.75      30  Camion Grande
    ## 2120      970 Juan Francisco Portillo Gomez 242.50      60 Camion Pequeño
    ## 2121     1766          Pedro Alvarez Parejo 441.50      60  Camion Grande
    ## 2122     1062 Juan Francisco Portillo Gomez 265.50      60  Camion Grande
    ## 2123     1804          Angel Valdez Alegria 451.00      30  Camion Grande
    ## 2124     1440          Pedro Alvarez Parejo 360.00      30  Camion Grande
    ## 2125     1353                  Hector Giron 338.25      90  Camion Grande
    ## 2126     1144      Ismael Rodero Monteagudo 286.00      30  Camion Grande
    ## 2127     1366          Pedro Alvarez Parejo 341.50      30  Camion Grande
    ## 2128      228 Juan Francisco Portillo Gomez  57.00      30          Panel
    ## 2129      247          Pedro Alvarez Parejo  61.75      90          Panel
    ## 2130     1152                  Hector Giron 288.00      30  Camion Grande
    ## 2131      758       Fernando Mariano Berrio 189.50      90 Camion Pequeño
    ## 2132      813 Juan Francisco Portillo Gomez 203.25      60 Camion Pequeño
    ## 2133     1598        Hector Aragones Frutos 399.50      30  Camion Grande
    ## 2134     1379 Juan Francisco Portillo Gomez 344.75      90  Camion Grande
    ## 2135     1258             Luis Jaime Urbano 314.50      60  Camion Grande
    ## 2136     1564        Hector Aragones Frutos 391.00      30  Camion Grande
    ## 2137     1626          Angel Valdez Alegria 406.50      60  Camion Grande
    ## 2138     1866              Felipe Villatoro 466.50      90  Camion Grande
    ## 2139      619          Angel Valdez Alegria 154.75      90 Camion Pequeño
    ## 2140     1130       Fernando Mariano Berrio 282.50      90  Camion Grande
    ## 2141     1623          Pedro Alvarez Parejo 405.75      60  Camion Grande
    ## 2142     1981             Luis Jaime Urbano 495.25      30  Camion Grande
    ## 2143      601       Fernando Mariano Berrio 150.25      30 Camion Pequeño
    ## 2144      428        Hector Aragones Frutos 107.00      60          Panel
    ## 2145      259          Angel Valdez Alegria  64.75      90          Panel
    ## 2146     1767             Luis Jaime Urbano 441.75      30  Camion Grande
    ## 2147     1213        Hector Aragones Frutos 303.25      60  Camion Grande
    ## 2148      866 Juan Francisco Portillo Gomez 216.50      60 Camion Pequeño
    ## 2149     1118          Angel Valdez Alegria 279.50      60  Camion Grande
    ## 2150     1397          Angel Valdez Alegria 349.25      60  Camion Grande
    ## 2151     1125       Fernando Mariano Berrio 281.25      30  Camion Grande
    ## 2152     1904       Fernando Mariano Berrio 476.00      90  Camion Grande
    ## 2153      496                  Hector Giron 124.00      60          Panel
    ## 2154     1064      Ismael Rodero Monteagudo 266.00      90  Camion Grande
    ## 2155     1100 Juan Francisco Portillo Gomez 275.00      90  Camion Grande
    ## 2156     1010 Juan Francisco Portillo Gomez 252.50      60  Camion Grande
    ## 2157     1174      Ismael Rodero Monteagudo 293.50      30  Camion Grande
    ## 2158      941          Pedro Alvarez Parejo 235.25      60 Camion Pequeño
    ## 2159      988          Pedro Alvarez Parejo 247.00      30 Camion Pequeño
    ## 2160     1636       Fernando Mariano Berrio 409.00      30  Camion Grande
    ## 2161     1401                  Hector Giron 350.25      30  Camion Grande
    ## 2162     1854          Pedro Alvarez Parejo 463.50      90  Camion Grande
    ## 2163      836 Juan Francisco Portillo Gomez 209.00      90 Camion Pequeño
    ## 2164     1302          Pedro Alvarez Parejo 325.50      90  Camion Grande
    ## 2165      235 Juan Francisco Portillo Gomez  58.75      30          Panel
    ## 2166      977                  Hector Giron 244.25      30 Camion Pequeño
    ## 2167     1970              Felipe Villatoro 492.50      60  Camion Grande
    ## 2168      413          Pedro Alvarez Parejo 103.25      60          Panel
    ## 2169     1573       Fernando Mariano Berrio 393.25      30  Camion Grande
    ## 2170     1564 Juan Francisco Portillo Gomez 391.00      90  Camion Grande
    ## 2171      908          Pedro Alvarez Parejo 227.00      60 Camion Pequeño
    ## 2172     1727        Hector Aragones Frutos 431.75      90  Camion Grande
    ## 2173     1774 Juan Francisco Portillo Gomez 443.50      30  Camion Grande
    ## 2174     1476                  Hector Giron 369.00      30  Camion Grande
    ## 2175      701       Fernando Mariano Berrio 175.25      60 Camion Pequeño
    ## 2176      808          Pedro Alvarez Parejo 202.00      90 Camion Pequeño
    ## 2177      995             Luis Jaime Urbano 248.75      60 Camion Pequeño
    ## 2178     1743 Juan Francisco Portillo Gomez 435.75      90  Camion Grande
    ## 2179      883       Fernando Mariano Berrio 220.75      90 Camion Pequeño
    ## 2180     1062          Angel Valdez Alegria 265.50      90  Camion Grande
    ##        Fecha
    ## 1    01-2018
    ## 2    01-2018
    ## 3    01-2018
    ## 4    01-2018
    ## 5    01-2018
    ## 6    01-2018
    ## 7    01-2018
    ## 8    01-2018
    ## 9    01-2018
    ## 10   01-2018
    ## 11   01-2018
    ## 12   01-2018
    ## 13   01-2018
    ## 14   01-2018
    ## 15   01-2018
    ## 16   01-2018
    ## 17   01-2018
    ## 18   01-2018
    ## 19   01-2018
    ## 20   01-2018
    ## 21   01-2018
    ## 22   01-2018
    ## 23   01-2018
    ## 24   01-2018
    ## 25   01-2018
    ## 26   01-2018
    ## 27   01-2018
    ## 28   01-2018
    ## 29   01-2018
    ## 30   01-2018
    ## 31   01-2018
    ## 32   01-2018
    ## 33   01-2018
    ## 34   01-2018
    ## 35   01-2018
    ## 36   01-2018
    ## 37   01-2018
    ## 38   01-2018
    ## 39   01-2018
    ## 40   01-2018
    ## 41   01-2018
    ## 42   01-2018
    ## 43   01-2018
    ## 44   01-2018
    ## 45   01-2018
    ## 46   01-2018
    ## 47   01-2018
    ## 48   01-2018
    ## 49   01-2018
    ## 50   01-2018
    ## 51   01-2018
    ## 52   01-2018
    ## 53   01-2018
    ## 54   01-2018
    ## 55   01-2018
    ## 56   01-2018
    ## 57   01-2018
    ## 58   01-2018
    ## 59   01-2018
    ## 60   01-2018
    ## 61   01-2018
    ## 62   01-2018
    ## 63   01-2018
    ## 64   01-2018
    ## 65   01-2018
    ## 66   01-2018
    ## 67   01-2018
    ## 68   01-2018
    ## 69   01-2018
    ## 70   01-2018
    ## 71   01-2018
    ## 72   01-2018
    ## 73   01-2018
    ## 74   01-2018
    ## 75   01-2018
    ## 76   01-2018
    ## 77   01-2018
    ## 78   01-2018
    ## 79   01-2018
    ## 80   01-2018
    ## 81   01-2018
    ## 82   01-2018
    ## 83   01-2018
    ## 84   01-2018
    ## 85   01-2018
    ## 86   01-2018
    ## 87   01-2018
    ## 88   01-2018
    ## 89   01-2018
    ## 90   01-2018
    ## 91   01-2018
    ## 92   01-2018
    ## 93   01-2018
    ## 94   01-2018
    ## 95   01-2018
    ## 96   01-2018
    ## 97   01-2018
    ## 98   01-2018
    ## 99   01-2018
    ## 100  01-2018
    ## 101  01-2018
    ## 102  01-2018
    ## 103  01-2018
    ## 104  01-2018
    ## 105  01-2018
    ## 106  01-2018
    ## 107  01-2018
    ## 108  01-2018
    ## 109  01-2018
    ## 110  01-2018
    ## 111  01-2018
    ## 112  01-2018
    ## 113  01-2018
    ## 114  01-2018
    ## 115  01-2018
    ## 116  01-2018
    ## 117  01-2018
    ## 118  01-2018
    ## 119  01-2018
    ## 120  01-2018
    ## 121  01-2018
    ## 122  01-2018
    ## 123  01-2018
    ## 124  01-2018
    ## 125  01-2018
    ## 126  01-2018
    ## 127  01-2018
    ## 128  01-2018
    ## 129  01-2018
    ## 130  01-2018
    ## 131  01-2018
    ## 132  01-2018
    ## 133  01-2018
    ## 134  01-2018
    ## 135  01-2018
    ## 136  01-2018
    ## 137  01-2018
    ## 138  01-2018
    ## 139  01-2018
    ## 140  01-2018
    ## 141  01-2018
    ## 142  01-2018
    ## 143  01-2018
    ## 144  01-2018
    ## 145  01-2018
    ## 146  01-2018
    ## 147  01-2018
    ## 148  01-2018
    ## 149  01-2018
    ## 150  01-2018
    ## 151  01-2018
    ## 152  01-2018
    ## 153  01-2018
    ## 154  01-2018
    ## 155  01-2018
    ## 156  01-2018
    ## 157  01-2018
    ## 158  01-2018
    ## 159  01-2018
    ## 160  01-2018
    ## 161  01-2018
    ## 162  01-2018
    ## 163  01-2018
    ## 164  01-2018
    ## 165  01-2018
    ## 166  01-2018
    ## 167  01-2018
    ## 168  01-2018
    ## 169  01-2018
    ## 170  01-2018
    ## 171  01-2018
    ## 172  01-2018
    ## 173  01-2018
    ## 174  01-2018
    ## 175  01-2018
    ## 176  01-2018
    ## 177  01-2018
    ## 178  01-2018
    ## 179  01-2018
    ## 180  01-2018
    ## 181  01-2018
    ## 182  01-2018
    ## 183  01-2018
    ## 184  01-2018
    ## 185  01-2018
    ## 186  01-2018
    ## 187  01-2018
    ## 188  01-2018
    ## 189  01-2018
    ## 190  01-2018
    ## 191  01-2018
    ## 192  01-2018
    ## 193  02-2018
    ## 194  02-2018
    ## 195  02-2018
    ## 196  02-2018
    ## 197  02-2018
    ## 198  02-2018
    ## 199  02-2018
    ## 200  02-2018
    ## 201  02-2018
    ## 202  02-2018
    ## 203  02-2018
    ## 204  02-2018
    ## 205  02-2018
    ## 206  02-2018
    ## 207  02-2018
    ## 208  02-2018
    ## 209  02-2018
    ## 210  02-2018
    ## 211  02-2018
    ## 212  02-2018
    ## 213  02-2018
    ## 214  02-2018
    ## 215  02-2018
    ## 216  02-2018
    ## 217  02-2018
    ## 218  02-2018
    ## 219  02-2018
    ## 220  02-2018
    ## 221  02-2018
    ## 222  02-2018
    ## 223  02-2018
    ## 224  02-2018
    ## 225  02-2018
    ## 226  02-2018
    ## 227  02-2018
    ## 228  02-2018
    ## 229  02-2018
    ## 230  02-2018
    ## 231  02-2018
    ## 232  02-2018
    ## 233  02-2018
    ## 234  02-2018
    ## 235  02-2018
    ## 236  02-2018
    ## 237  02-2018
    ## 238  02-2018
    ## 239  02-2018
    ## 240  02-2018
    ## 241  02-2018
    ## 242  02-2018
    ## 243  02-2018
    ## 244  02-2018
    ## 245  02-2018
    ## 246  02-2018
    ## 247  02-2018
    ## 248  02-2018
    ## 249  02-2018
    ## 250  02-2018
    ## 251  02-2018
    ## 252  02-2018
    ## 253  02-2018
    ## 254  02-2018
    ## 255  02-2018
    ## 256  02-2018
    ## 257  02-2018
    ## 258  02-2018
    ## 259  02-2018
    ## 260  02-2018
    ## 261  02-2018
    ## 262  02-2018
    ## 263  02-2018
    ## 264  02-2018
    ## 265  02-2018
    ## 266  02-2018
    ## 267  02-2018
    ## 268  02-2018
    ## 269  02-2018
    ## 270  02-2018
    ## 271  02-2018
    ## 272  02-2018
    ## 273  02-2018
    ## 274  02-2018
    ## 275  02-2018
    ## 276  02-2018
    ## 277  02-2018
    ## 278  02-2018
    ## 279  02-2018
    ## 280  02-2018
    ## 281  02-2018
    ## 282  02-2018
    ## 283  02-2018
    ## 284  02-2018
    ## 285  02-2018
    ## 286  02-2018
    ## 287  02-2018
    ## 288  02-2018
    ## 289  02-2018
    ## 290  02-2018
    ## 291  02-2018
    ## 292  02-2018
    ## 293  02-2018
    ## 294  02-2018
    ## 295  02-2018
    ## 296  02-2018
    ## 297  02-2018
    ## 298  02-2018
    ## 299  02-2018
    ## 300  02-2018
    ## 301  02-2018
    ## 302  02-2018
    ## 303  02-2018
    ## 304  02-2018
    ## 305  02-2018
    ## 306  02-2018
    ## 307  02-2018
    ## 308  02-2018
    ## 309  02-2018
    ## 310  02-2018
    ## 311  02-2018
    ## 312  02-2018
    ## 313  02-2018
    ## 314  02-2018
    ## 315  02-2018
    ## 316  02-2018
    ## 317  02-2018
    ## 318  02-2018
    ## 319  02-2018
    ## 320  02-2018
    ## 321  02-2018
    ## 322  02-2018
    ## 323  02-2018
    ## 324  02-2018
    ## 325  02-2018
    ## 326  02-2018
    ## 327  02-2018
    ## 328  02-2018
    ## 329  02-2018
    ## 330  02-2018
    ## 331  02-2018
    ## 332  02-2018
    ## 333  02-2018
    ## 334  02-2018
    ## 335  02-2018
    ## 336  02-2018
    ## 337  02-2018
    ## 338  02-2018
    ## 339  02-2018
    ## 340  02-2018
    ## 341  02-2018
    ## 342  02-2018
    ## 343  02-2018
    ## 344  02-2018
    ## 345  02-2018
    ## 346  02-2018
    ## 347  02-2018
    ## 348  02-2018
    ## 349  02-2018
    ## 350  02-2018
    ## 351  02-2018
    ## 352  02-2018
    ## 353  02-2018
    ## 354  02-2018
    ## 355  02-2018
    ## 356  02-2018
    ## 357  02-2018
    ## 358  02-2018
    ## 359  02-2018
    ## 360  02-2018
    ## 361  02-2018
    ## 362  02-2018
    ## 363  02-2018
    ## 364  02-2018
    ## 365  02-2018
    ## 366  02-2018
    ## 367  02-2018
    ## 368  02-2018
    ## 369  02-2018
    ## 370  02-2018
    ## 371  02-2018
    ## 372  02-2018
    ## 373  02-2018
    ## 374  02-2018
    ## 375  02-2018
    ## 376  02-2018
    ## 377  02-2018
    ## 378  02-2018
    ## 379  02-2018
    ## 380  02-2018
    ## 381  02-2018
    ## 382  02-2018
    ## 383  02-2018
    ## 384  02-2018
    ## 385  02-2018
    ## 386  02-2018
    ## 387  02-2018
    ## 388  02-2018
    ## 389  02-2018
    ## 390  02-2018
    ## 391  02-2018
    ## 392  02-2018
    ## 393  02-2018
    ## 394  02-2018
    ## 395  02-2018
    ## 396  03-2018
    ## 397  03-2018
    ## 398  03-2018
    ## 399  03-2018
    ## 400  03-2018
    ## 401  03-2018
    ## 402  03-2018
    ## 403  03-2018
    ## 404  03-2018
    ## 405  03-2018
    ## 406  03-2018
    ## 407  03-2018
    ## 408  03-2018
    ## 409  03-2018
    ## 410  03-2018
    ## 411  03-2018
    ## 412  03-2018
    ## 413  03-2018
    ## 414  03-2018
    ## 415  03-2018
    ## 416  03-2018
    ## 417  03-2018
    ## 418  03-2018
    ## 419  03-2018
    ## 420  03-2018
    ## 421  03-2018
    ## 422  03-2018
    ## 423  03-2018
    ## 424  03-2018
    ## 425  03-2018
    ## 426  03-2018
    ## 427  03-2018
    ## 428  03-2018
    ## 429  03-2018
    ## 430  03-2018
    ## 431  03-2018
    ## 432  03-2018
    ## 433  03-2018
    ## 434  03-2018
    ## 435  03-2018
    ## 436  03-2018
    ## 437  03-2018
    ## 438  03-2018
    ## 439  03-2018
    ## 440  03-2018
    ## 441  03-2018
    ## 442  03-2018
    ## 443  03-2018
    ## 444  03-2018
    ## 445  03-2018
    ## 446  03-2018
    ## 447  03-2018
    ## 448  03-2018
    ## 449  03-2018
    ## 450  03-2018
    ## 451  03-2018
    ## 452  03-2018
    ## 453  03-2018
    ## 454  03-2018
    ## 455  03-2018
    ## 456  03-2018
    ## 457  03-2018
    ## 458  03-2018
    ## 459  03-2018
    ## 460  03-2018
    ## 461  03-2018
    ## 462  03-2018
    ## 463  03-2018
    ## 464  03-2018
    ## 465  03-2018
    ## 466  03-2018
    ## 467  03-2018
    ## 468  03-2018
    ## 469  03-2018
    ## 470  03-2018
    ## 471  03-2018
    ## 472  03-2018
    ## 473  03-2018
    ## 474  03-2018
    ## 475  03-2018
    ## 476  03-2018
    ## 477  03-2018
    ## 478  03-2018
    ## 479  03-2018
    ## 480  03-2018
    ## 481  03-2018
    ## 482  03-2018
    ## 483  03-2018
    ## 484  03-2018
    ## 485  03-2018
    ## 486  03-2018
    ## 487  03-2018
    ## 488  03-2018
    ## 489  03-2018
    ## 490  03-2018
    ## 491  03-2018
    ## 492  03-2018
    ## 493  03-2018
    ## 494  03-2018
    ## 495  03-2018
    ## 496  03-2018
    ## 497  03-2018
    ## 498  03-2018
    ## 499  03-2018
    ## 500  03-2018
    ## 501  03-2018
    ## 502  03-2018
    ## 503  03-2018
    ## 504  03-2018
    ## 505  03-2018
    ## 506  03-2018
    ## 507  03-2018
    ## 508  03-2018
    ## 509  03-2018
    ## 510  03-2018
    ## 511  03-2018
    ## 512  03-2018
    ## 513  03-2018
    ## 514  03-2018
    ## 515  03-2018
    ## 516  03-2018
    ## 517  03-2018
    ## 518  03-2018
    ## 519  03-2018
    ## 520  03-2018
    ## 521  03-2018
    ## 522  03-2018
    ## 523  03-2018
    ## 524  03-2018
    ## 525  03-2018
    ## 526  03-2018
    ## 527  03-2018
    ## 528  03-2018
    ## 529  03-2018
    ## 530  03-2018
    ## 531  03-2018
    ## 532  03-2018
    ## 533  03-2018
    ## 534  03-2018
    ## 535  03-2018
    ## 536  03-2018
    ## 537  03-2018
    ## 538  03-2018
    ## 539  03-2018
    ## 540  03-2018
    ## 541  03-2018
    ## 542  03-2018
    ## 543  03-2018
    ## 544  03-2018
    ## 545  03-2018
    ## 546  03-2018
    ## 547  03-2018
    ## 548  03-2018
    ## 549  03-2018
    ## 550  03-2018
    ## 551  03-2018
    ## 552  03-2018
    ## 553  03-2018
    ## 554  03-2018
    ## 555  03-2018
    ## 556  03-2018
    ## 557  03-2018
    ## 558  03-2018
    ## 559  03-2018
    ## 560  03-2018
    ## 561  03-2018
    ## 562  03-2018
    ## 563  03-2018
    ## 564  03-2018
    ## 565  03-2018
    ## 566  03-2018
    ## 567  03-2018
    ## 568  03-2018
    ## 569  03-2018
    ## 570  03-2018
    ## 571  03-2018
    ## 572  03-2018
    ## 573  03-2018
    ## 574  03-2018
    ## 575  03-2018
    ## 576  03-2018
    ## 577  03-2018
    ## 578  04-2018
    ## 579  04-2018
    ## 580  04-2018
    ## 581  04-2018
    ## 582  04-2018
    ## 583  04-2018
    ## 584  04-2018
    ## 585  04-2018
    ## 586  04-2018
    ## 587  04-2018
    ## 588  04-2018
    ## 589  04-2018
    ## 590  04-2018
    ## 591  04-2018
    ## 592  04-2018
    ## 593  04-2018
    ## 594  04-2018
    ## 595  04-2018
    ## 596  04-2018
    ## 597  04-2018
    ## 598  04-2018
    ## 599  04-2018
    ## 600  04-2018
    ## 601  04-2018
    ## 602  04-2018
    ## 603  04-2018
    ## 604  04-2018
    ## 605  04-2018
    ## 606  04-2018
    ## 607  04-2018
    ## 608  04-2018
    ## 609  04-2018
    ## 610  04-2018
    ## 611  04-2018
    ## 612  04-2018
    ## 613  04-2018
    ## 614  04-2018
    ## 615  04-2018
    ## 616  04-2018
    ## 617  04-2018
    ## 618  04-2018
    ## 619  04-2018
    ## 620  04-2018
    ## 621  04-2018
    ## 622  04-2018
    ## 623  04-2018
    ## 624  04-2018
    ## 625  04-2018
    ## 626  04-2018
    ## 627  04-2018
    ## 628  04-2018
    ## 629  04-2018
    ## 630  04-2018
    ## 631  04-2018
    ## 632  04-2018
    ## 633  04-2018
    ## 634  04-2018
    ## 635  04-2018
    ## 636  04-2018
    ## 637  04-2018
    ## 638  04-2018
    ## 639  04-2018
    ## 640  04-2018
    ## 641  04-2018
    ## 642  04-2018
    ## 643  04-2018
    ## 644  04-2018
    ## 645  04-2018
    ## 646  04-2018
    ## 647  04-2018
    ## 648  04-2018
    ## 649  04-2018
    ## 650  04-2018
    ## 651  04-2018
    ## 652  04-2018
    ## 653  04-2018
    ## 654  04-2018
    ## 655  04-2018
    ## 656  04-2018
    ## 657  04-2018
    ## 658  04-2018
    ## 659  04-2018
    ## 660  04-2018
    ## 661  04-2018
    ## 662  04-2018
    ## 663  04-2018
    ## 664  04-2018
    ## 665  04-2018
    ## 666  04-2018
    ## 667  04-2018
    ## 668  04-2018
    ## 669  04-2018
    ## 670  04-2018
    ## 671  04-2018
    ## 672  04-2018
    ## 673  04-2018
    ## 674  04-2018
    ## 675  04-2018
    ## 676  04-2018
    ## 677  04-2018
    ## 678  04-2018
    ## 679  04-2018
    ## 680  04-2018
    ## 681  04-2018
    ## 682  04-2018
    ## 683  04-2018
    ## 684  04-2018
    ## 685  04-2018
    ## 686  04-2018
    ## 687  04-2018
    ## 688  04-2018
    ## 689  04-2018
    ## 690  04-2018
    ## 691  04-2018
    ## 692  04-2018
    ## 693  04-2018
    ## 694  04-2018
    ## 695  04-2018
    ## 696  04-2018
    ## 697  04-2018
    ## 698  04-2018
    ## 699  04-2018
    ## 700  04-2018
    ## 701  04-2018
    ## 702  04-2018
    ## 703  04-2018
    ## 704  04-2018
    ## 705  04-2018
    ## 706  04-2018
    ## 707  04-2018
    ## 708  04-2018
    ## 709  04-2018
    ## 710  04-2018
    ## 711  04-2018
    ## 712  04-2018
    ## 713  04-2018
    ## 714  04-2018
    ## 715  04-2018
    ## 716  04-2018
    ## 717  04-2018
    ## 718  04-2018
    ## 719  04-2018
    ## 720  04-2018
    ## 721  04-2018
    ## 722  04-2018
    ## 723  04-2018
    ## 724  04-2018
    ## 725  04-2018
    ## 726  04-2018
    ## 727  04-2018
    ## 728  04-2018
    ## 729  04-2018
    ## 730  04-2018
    ## 731  04-2018
    ## 732  04-2018
    ## 733  04-2018
    ## 734  04-2018
    ## 735  04-2018
    ## 736  04-2018
    ## 737  04-2018
    ## 738  04-2018
    ## 739  04-2018
    ## 740  04-2018
    ## 741  04-2018
    ## 742  04-2018
    ## 743  04-2018
    ## 744  04-2018
    ## 745  04-2018
    ## 746  04-2018
    ## 747  04-2018
    ## 748  04-2018
    ## 749  04-2018
    ## 750  04-2018
    ## 751  04-2018
    ## 752  04-2018
    ## 753  04-2018
    ## 754  04-2018
    ## 755  04-2018
    ## 756  04-2018
    ## 757  04-2018
    ## 758  04-2018
    ## 759  04-2018
    ## 760  04-2018
    ## 761  04-2018
    ## 762  04-2018
    ## 763  04-2018
    ## 764  04-2018
    ## 765  04-2018
    ## 766  04-2018
    ## 767  04-2018
    ## 768  04-2018
    ## 769  04-2018
    ## 770  04-2018
    ## 771  04-2018
    ## 772  04-2018
    ## 773  04-2018
    ## 774  05-2018
    ## 775  05-2018
    ## 776  05-2018
    ## 777  05-2018
    ## 778  05-2018
    ## 779  05-2018
    ## 780  05-2018
    ## 781  05-2018
    ## 782  05-2018
    ## 783  05-2018
    ## 784  05-2018
    ## 785  05-2018
    ## 786  05-2018
    ## 787  05-2018
    ## 788  05-2018
    ## 789  05-2018
    ## 790  05-2018
    ## 791  05-2018
    ## 792  05-2018
    ## 793  05-2018
    ## 794  05-2018
    ## 795  05-2018
    ## 796  05-2018
    ## 797  05-2018
    ## 798  05-2018
    ## 799  05-2018
    ## 800  05-2018
    ## 801  05-2018
    ## 802  05-2018
    ## 803  05-2018
    ## 804  05-2018
    ## 805  05-2018
    ## 806  05-2018
    ## 807  05-2018
    ## 808  05-2018
    ## 809  05-2018
    ## 810  05-2018
    ## 811  05-2018
    ## 812  05-2018
    ## 813  05-2018
    ## 814  05-2018
    ## 815  05-2018
    ## 816  05-2018
    ## 817  05-2018
    ## 818  05-2018
    ## 819  05-2018
    ## 820  05-2018
    ## 821  05-2018
    ## 822  05-2018
    ## 823  05-2018
    ## 824  05-2018
    ## 825  05-2018
    ## 826  05-2018
    ## 827  05-2018
    ## 828  05-2018
    ## 829  05-2018
    ## 830  05-2018
    ## 831  05-2018
    ## 832  05-2018
    ## 833  05-2018
    ## 834  05-2018
    ## 835  05-2018
    ## 836  05-2018
    ## 837  05-2018
    ## 838  05-2018
    ## 839  05-2018
    ## 840  05-2018
    ## 841  05-2018
    ## 842  05-2018
    ## 843  05-2018
    ## 844  05-2018
    ## 845  05-2018
    ## 846  05-2018
    ## 847  05-2018
    ## 848  05-2018
    ## 849  05-2018
    ## 850  05-2018
    ## 851  05-2018
    ## 852  05-2018
    ## 853  05-2018
    ## 854  05-2018
    ## 855  05-2018
    ## 856  05-2018
    ## 857  05-2018
    ## 858  05-2018
    ## 859  05-2018
    ## 860  05-2018
    ## 861  05-2018
    ## 862  05-2018
    ## 863  05-2018
    ## 864  05-2018
    ## 865  05-2018
    ## 866  05-2018
    ## 867  05-2018
    ## 868  05-2018
    ## 869  05-2018
    ## 870  05-2018
    ## 871  05-2018
    ## 872  05-2018
    ## 873  05-2018
    ## 874  05-2018
    ## 875  05-2018
    ## 876  05-2018
    ## 877  05-2018
    ## 878  05-2018
    ## 879  05-2018
    ## 880  05-2018
    ## 881  05-2018
    ## 882  05-2018
    ## 883  05-2018
    ## 884  05-2018
    ## 885  05-2018
    ## 886  05-2018
    ## 887  05-2018
    ## 888  05-2018
    ## 889  05-2018
    ## 890  05-2018
    ## 891  05-2018
    ## 892  05-2018
    ## 893  05-2018
    ## 894  05-2018
    ## 895  05-2018
    ## 896  05-2018
    ## 897  05-2018
    ## 898  05-2018
    ## 899  05-2018
    ## 900  05-2018
    ## 901  05-2018
    ## 902  05-2018
    ## 903  05-2018
    ## 904  05-2018
    ## 905  05-2018
    ## 906  05-2018
    ## 907  05-2018
    ## 908  05-2018
    ## 909  05-2018
    ## 910  05-2018
    ## 911  05-2018
    ## 912  05-2018
    ## 913  05-2018
    ## 914  05-2018
    ## 915  05-2018
    ## 916  05-2018
    ## 917  05-2018
    ## 918  05-2018
    ## 919  05-2018
    ## 920  05-2018
    ## 921  05-2018
    ## 922  05-2018
    ## 923  05-2018
    ## 924  05-2018
    ## 925  05-2018
    ## 926  05-2018
    ## 927  05-2018
    ## 928  05-2018
    ## 929  05-2018
    ## 930  05-2018
    ## 931  05-2018
    ## 932  05-2018
    ## 933  05-2018
    ## 934  05-2018
    ## 935  05-2018
    ## 936  05-2018
    ## 937  05-2018
    ## 938  05-2018
    ## 939  05-2018
    ## 940  05-2018
    ## 941  05-2018
    ## 942  05-2018
    ## 943  05-2018
    ## 944  05-2018
    ## 945  05-2018
    ## 946  05-2018
    ## 947  05-2018
    ## 948  05-2018
    ## 949  05-2018
    ## 950  05-2018
    ## 951  05-2018
    ## 952  05-2018
    ## 953  05-2018
    ## 954  05-2018
    ## 955  05-2018
    ## 956  05-2018
    ## 957  05-2018
    ## 958  05-2018
    ## 959  05-2018
    ## 960  05-2018
    ## 961  05-2018
    ## 962  05-2018
    ## 963  05-2018
    ## 964  05-2018
    ## 965  05-2018
    ## 966  05-2018
    ## 967  05-2018
    ## 968  05-2018
    ## 969  05-2018
    ## 970  05-2018
    ## 971  05-2018
    ## 972  05-2018
    ## 973  05-2018
    ## 974  05-2018
    ## 975  05-2018
    ## 976  05-2018
    ## 977  05-2018
    ## 978  05-2018
    ## 979  05-2018
    ## 980  05-2018
    ## 981  05-2018
    ## 982  05-2018
    ## 983  05-2018
    ## 984  05-2018
    ## 985  05-2018
    ## 986  05-2018
    ## 987  05-2018
    ## 988  05-2018
    ## 989  06-2018
    ## 990  06-2018
    ## 991  06-2018
    ## 992  06-2018
    ## 993  06-2018
    ## 994  06-2018
    ## 995  06-2018
    ## 996  06-2018
    ## 997  06-2018
    ## 998  06-2018
    ## 999  06-2018
    ## 1000 06-2018
    ## 1001 06-2018
    ## 1002 06-2018
    ## 1003 06-2018
    ## 1004 06-2018
    ## 1005 06-2018
    ## 1006 06-2018
    ## 1007 06-2018
    ## 1008 06-2018
    ## 1009 06-2018
    ## 1010 06-2018
    ## 1011 06-2018
    ## 1012 06-2018
    ## 1013 06-2018
    ## 1014 06-2018
    ## 1015 06-2018
    ## 1016 06-2018
    ## 1017 06-2018
    ## 1018 06-2018
    ## 1019 06-2018
    ## 1020 06-2018
    ## 1021 06-2018
    ## 1022 06-2018
    ## 1023 06-2018
    ## 1024 06-2018
    ## 1025 06-2018
    ## 1026 06-2018
    ## 1027 06-2018
    ## 1028 06-2018
    ## 1029 06-2018
    ## 1030 06-2018
    ## 1031 06-2018
    ## 1032 06-2018
    ## 1033 06-2018
    ## 1034 06-2018
    ## 1035 06-2018
    ## 1036 06-2018
    ## 1037 06-2018
    ## 1038 06-2018
    ## 1039 06-2018
    ## 1040 06-2018
    ## 1041 06-2018
    ## 1042 06-2018
    ## 1043 06-2018
    ## 1044 06-2018
    ## 1045 06-2018
    ## 1046 06-2018
    ## 1047 06-2018
    ## 1048 06-2018
    ## 1049 06-2018
    ## 1050 06-2018
    ## 1051 06-2018
    ## 1052 06-2018
    ## 1053 06-2018
    ## 1054 06-2018
    ## 1055 06-2018
    ## 1056 06-2018
    ## 1057 06-2018
    ## 1058 06-2018
    ## 1059 06-2018
    ## 1060 06-2018
    ## 1061 06-2018
    ## 1062 06-2018
    ## 1063 06-2018
    ## 1064 06-2018
    ## 1065 06-2018
    ## 1066 06-2018
    ## 1067 06-2018
    ## 1068 06-2018
    ## 1069 06-2018
    ## 1070 06-2018
    ## 1071 06-2018
    ## 1072 06-2018
    ## 1073 06-2018
    ## 1074 06-2018
    ## 1075 06-2018
    ## 1076 06-2018
    ## 1077 06-2018
    ## 1078 06-2018
    ## 1079 06-2018
    ## 1080 06-2018
    ## 1081 06-2018
    ## 1082 06-2018
    ## 1083 06-2018
    ## 1084 06-2018
    ## 1085 06-2018
    ## 1086 06-2018
    ## 1087 06-2018
    ## 1088 06-2018
    ## 1089 06-2018
    ## 1090 06-2018
    ## 1091 06-2018
    ## 1092 06-2018
    ## 1093 06-2018
    ## 1094 06-2018
    ## 1095 06-2018
    ## 1096 06-2018
    ## 1097 06-2018
    ## 1098 06-2018
    ## 1099 06-2018
    ## 1100 06-2018
    ## 1101 06-2018
    ## 1102 06-2018
    ## 1103 06-2018
    ## 1104 06-2018
    ## 1105 06-2018
    ## 1106 06-2018
    ## 1107 06-2018
    ## 1108 06-2018
    ## 1109 06-2018
    ## 1110 06-2018
    ## 1111 06-2018
    ## 1112 06-2018
    ## 1113 06-2018
    ## 1114 06-2018
    ## 1115 06-2018
    ## 1116 06-2018
    ## 1117 06-2018
    ## 1118 06-2018
    ## 1119 06-2018
    ## 1120 06-2018
    ## 1121 06-2018
    ## 1122 06-2018
    ## 1123 06-2018
    ## 1124 06-2018
    ## 1125 06-2018
    ## 1126 06-2018
    ## 1127 06-2018
    ## 1128 06-2018
    ## 1129 06-2018
    ## 1130 06-2018
    ## 1131 06-2018
    ## 1132 06-2018
    ## 1133 06-2018
    ## 1134 06-2018
    ## 1135 06-2018
    ## 1136 06-2018
    ## 1137 06-2018
    ## 1138 06-2018
    ## 1139 06-2018
    ## 1140 06-2018
    ## 1141 06-2018
    ## 1142 06-2018
    ## 1143 06-2018
    ## 1144 06-2018
    ## 1145 06-2018
    ## 1146 06-2018
    ## 1147 06-2018
    ## 1148 06-2018
    ## 1149 06-2018
    ## 1150 06-2018
    ## 1151 06-2018
    ## 1152 06-2018
    ## 1153 06-2018
    ## 1154 06-2018
    ## 1155 06-2018
    ## 1156 06-2018
    ## 1157 06-2018
    ## 1158 06-2018
    ## 1159 06-2018
    ## 1160 06-2018
    ## 1161 06-2018
    ## 1162 06-2018
    ## 1163 06-2018
    ## 1164 06-2018
    ## 1165 06-2018
    ## 1166 06-2018
    ## 1167 06-2018
    ## 1168 06-2018
    ## 1169 06-2018
    ## 1170 06-2018
    ## 1171 06-2018
    ## 1172 06-2018
    ## 1173 06-2018
    ## 1174 06-2018
    ## 1175 06-2018
    ## 1176 06-2018
    ## 1177 06-2018
    ## 1178 06-2018
    ## 1179 06-2018
    ## 1180 06-2018
    ## 1181 06-2018
    ## 1182 06-2018
    ## 1183 06-2018
    ## 1184 06-2018
    ## 1185 06-2018
    ## 1186 07-2018
    ## 1187 07-2018
    ## 1188 07-2018
    ## 1189 07-2018
    ## 1190 07-2018
    ## 1191 07-2018
    ## 1192 07-2018
    ## 1193 07-2018
    ## 1194 07-2018
    ## 1195 07-2018
    ## 1196 07-2018
    ## 1197 07-2018
    ## 1198 07-2018
    ## 1199 07-2018
    ## 1200 07-2018
    ## 1201 07-2018
    ## 1202 07-2018
    ## 1203 07-2018
    ## 1204 07-2018
    ## 1205 07-2018
    ## 1206 07-2018
    ## 1207 07-2018
    ## 1208 07-2018
    ## 1209 07-2018
    ## 1210 07-2018
    ## 1211 07-2018
    ## 1212 07-2018
    ## 1213 07-2018
    ## 1214 07-2018
    ## 1215 07-2018
    ## 1216 07-2018
    ## 1217 07-2018
    ## 1218 07-2018
    ## 1219 07-2018
    ## 1220 07-2018
    ## 1221 07-2018
    ## 1222 07-2018
    ## 1223 07-2018
    ## 1224 07-2018
    ## 1225 07-2018
    ## 1226 07-2018
    ## 1227 07-2018
    ## 1228 07-2018
    ## 1229 07-2018
    ## 1230 07-2018
    ## 1231 07-2018
    ## 1232 07-2018
    ## 1233 07-2018
    ## 1234 07-2018
    ## 1235 07-2018
    ## 1236 07-2018
    ## 1237 07-2018
    ## 1238 07-2018
    ## 1239 07-2018
    ## 1240 07-2018
    ## 1241 07-2018
    ## 1242 07-2018
    ## 1243 07-2018
    ## 1244 07-2018
    ## 1245 07-2018
    ## 1246 07-2018
    ## 1247 07-2018
    ## 1248 07-2018
    ## 1249 07-2018
    ## 1250 07-2018
    ## 1251 07-2018
    ## 1252 07-2018
    ## 1253 07-2018
    ## 1254 07-2018
    ## 1255 07-2018
    ## 1256 07-2018
    ## 1257 07-2018
    ## 1258 07-2018
    ## 1259 07-2018
    ## 1260 07-2018
    ## 1261 07-2018
    ## 1262 07-2018
    ## 1263 07-2018
    ## 1264 07-2018
    ## 1265 07-2018
    ## 1266 07-2018
    ## 1267 07-2018
    ## 1268 07-2018
    ## 1269 07-2018
    ## 1270 07-2018
    ## 1271 07-2018
    ## 1272 07-2018
    ## 1273 07-2018
    ## 1274 07-2018
    ## 1275 07-2018
    ## 1276 07-2018
    ## 1277 07-2018
    ## 1278 07-2018
    ## 1279 07-2018
    ## 1280 07-2018
    ## 1281 07-2018
    ## 1282 07-2018
    ## 1283 07-2018
    ## 1284 07-2018
    ## 1285 07-2018
    ## 1286 07-2018
    ## 1287 07-2018
    ## 1288 07-2018
    ## 1289 07-2018
    ## 1290 07-2018
    ## 1291 07-2018
    ## 1292 07-2018
    ## 1293 07-2018
    ## 1294 07-2018
    ## 1295 07-2018
    ## 1296 07-2018
    ## 1297 07-2018
    ## 1298 07-2018
    ## 1299 07-2018
    ## 1300 07-2018
    ## 1301 07-2018
    ## 1302 07-2018
    ## 1303 07-2018
    ## 1304 07-2018
    ## 1305 07-2018
    ## 1306 07-2018
    ## 1307 07-2018
    ## 1308 07-2018
    ## 1309 07-2018
    ## 1310 07-2018
    ## 1311 07-2018
    ## 1312 07-2018
    ## 1313 07-2018
    ## 1314 07-2018
    ## 1315 07-2018
    ## 1316 07-2018
    ## 1317 07-2018
    ## 1318 07-2018
    ## 1319 07-2018
    ## 1320 07-2018
    ## 1321 07-2018
    ## 1322 07-2018
    ## 1323 07-2018
    ## 1324 07-2018
    ## 1325 07-2018
    ## 1326 07-2018
    ## 1327 07-2018
    ## 1328 07-2018
    ## 1329 07-2018
    ## 1330 07-2018
    ## 1331 07-2018
    ## 1332 07-2018
    ## 1333 07-2018
    ## 1334 07-2018
    ## 1335 07-2018
    ## 1336 07-2018
    ## 1337 07-2018
    ## 1338 07-2018
    ## 1339 07-2018
    ## 1340 07-2018
    ## 1341 07-2018
    ## 1342 07-2018
    ## 1343 07-2018
    ## 1344 07-2018
    ## 1345 07-2018
    ## 1346 07-2018
    ## 1347 07-2018
    ## 1348 07-2018
    ## 1349 07-2018
    ## 1350 07-2018
    ## 1351 07-2018
    ## 1352 07-2018
    ## 1353 07-2018
    ## 1354 07-2018
    ## 1355 07-2018
    ## 1356 07-2018
    ## 1357 07-2018
    ## 1358 07-2018
    ## 1359 07-2018
    ## 1360 07-2018
    ## 1361 07-2018
    ## 1362 07-2018
    ## 1363 07-2018
    ## 1364 07-2018
    ## 1365 07-2018
    ## 1366 07-2018
    ## 1367 07-2018
    ## 1368 07-2018
    ## 1369 07-2018
    ## 1370 07-2018
    ## 1371 07-2018
    ## 1372 07-2018
    ## 1373 07-2018
    ## 1374 07-2018
    ## 1375 07-2018
    ## 1376 07-2018
    ## 1377 07-2018
    ## 1378 07-2018
    ## 1379 07-2018
    ## 1380 07-2018
    ## 1381 07-2018
    ## 1382 07-2018
    ## 1383 07-2018
    ## 1384 07-2018
    ## 1385 07-2018
    ## 1386 07-2018
    ## 1387 07-2018
    ## 1388 07-2018
    ## 1389 07-2018
    ## 1390 07-2018
    ## 1391 07-2018
    ## 1392 07-2018
    ## 1393 07-2018
    ## 1394 07-2018
    ## 1395 07-2018
    ## 1396 07-2018
    ## 1397 08-2018
    ## 1398 08-2018
    ## 1399 08-2018
    ## 1400 08-2018
    ## 1401 08-2018
    ## 1402 08-2018
    ## 1403 08-2018
    ## 1404 08-2018
    ## 1405 08-2018
    ## 1406 08-2018
    ## 1407 08-2018
    ## 1408 08-2018
    ## 1409 08-2018
    ## 1410 08-2018
    ## 1411 08-2018
    ## 1412 08-2018
    ## 1413 08-2018
    ## 1414 08-2018
    ## 1415 08-2018
    ## 1416 08-2018
    ## 1417 08-2018
    ## 1418 08-2018
    ## 1419 08-2018
    ## 1420 08-2018
    ## 1421 08-2018
    ## 1422 08-2018
    ## 1423 08-2018
    ## 1424 08-2018
    ## 1425 08-2018
    ## 1426 08-2018
    ## 1427 08-2018
    ## 1428 08-2018
    ## 1429 08-2018
    ## 1430 08-2018
    ## 1431 08-2018
    ## 1432 08-2018
    ## 1433 08-2018
    ## 1434 08-2018
    ## 1435 08-2018
    ## 1436 08-2018
    ## 1437 08-2018
    ## 1438 08-2018
    ## 1439 08-2018
    ## 1440 08-2018
    ## 1441 08-2018
    ## 1442 08-2018
    ## 1443 08-2018
    ## 1444 08-2018
    ## 1445 08-2018
    ## 1446 08-2018
    ## 1447 08-2018
    ## 1448 08-2018
    ## 1449 08-2018
    ## 1450 08-2018
    ## 1451 08-2018
    ## 1452 08-2018
    ## 1453 08-2018
    ## 1454 08-2018
    ## 1455 08-2018
    ## 1456 08-2018
    ## 1457 08-2018
    ## 1458 08-2018
    ## 1459 08-2018
    ## 1460 08-2018
    ## 1461 08-2018
    ## 1462 08-2018
    ## 1463 08-2018
    ## 1464 08-2018
    ## 1465 08-2018
    ## 1466 08-2018
    ## 1467 08-2018
    ## 1468 08-2018
    ## 1469 08-2018
    ## 1470 08-2018
    ## 1471 08-2018
    ## 1472 08-2018
    ## 1473 08-2018
    ## 1474 08-2018
    ## 1475 08-2018
    ## 1476 08-2018
    ## 1477 08-2018
    ## 1478 08-2018
    ## 1479 08-2018
    ## 1480 08-2018
    ## 1481 08-2018
    ## 1482 08-2018
    ## 1483 08-2018
    ## 1484 08-2018
    ## 1485 08-2018
    ## 1486 08-2018
    ## 1487 08-2018
    ## 1488 08-2018
    ## 1489 08-2018
    ## 1490 08-2018
    ## 1491 08-2018
    ## 1492 08-2018
    ## 1493 08-2018
    ## 1494 08-2018
    ## 1495 08-2018
    ## 1496 08-2018
    ## 1497 08-2018
    ## 1498 08-2018
    ## 1499 08-2018
    ## 1500 08-2018
    ## 1501 08-2018
    ## 1502 08-2018
    ## 1503 08-2018
    ## 1504 08-2018
    ## 1505 08-2018
    ## 1506 08-2018
    ## 1507 08-2018
    ## 1508 08-2018
    ## 1509 08-2018
    ## 1510 08-2018
    ## 1511 08-2018
    ## 1512 08-2018
    ## 1513 08-2018
    ## 1514 08-2018
    ## 1515 08-2018
    ## 1516 08-2018
    ## 1517 08-2018
    ## 1518 08-2018
    ## 1519 08-2018
    ## 1520 08-2018
    ## 1521 08-2018
    ## 1522 08-2018
    ## 1523 08-2018
    ## 1524 08-2018
    ## 1525 08-2018
    ## 1526 08-2018
    ## 1527 08-2018
    ## 1528 08-2018
    ## 1529 08-2018
    ## 1530 08-2018
    ## 1531 08-2018
    ## 1532 08-2018
    ## 1533 08-2018
    ## 1534 08-2018
    ## 1535 08-2018
    ## 1536 08-2018
    ## 1537 08-2018
    ## 1538 08-2018
    ## 1539 08-2018
    ## 1540 08-2018
    ## 1541 08-2018
    ## 1542 08-2018
    ## 1543 08-2018
    ## 1544 08-2018
    ## 1545 08-2018
    ## 1546 08-2018
    ## 1547 08-2018
    ## 1548 08-2018
    ## 1549 08-2018
    ## 1550 08-2018
    ## 1551 08-2018
    ## 1552 08-2018
    ## 1553 08-2018
    ## 1554 08-2018
    ## 1555 08-2018
    ## 1556 08-2018
    ## 1557 08-2018
    ## 1558 08-2018
    ## 1559 08-2018
    ## 1560 08-2018
    ## 1561 08-2018
    ## 1562 08-2018
    ## 1563 08-2018
    ## 1564 08-2018
    ## 1565 08-2018
    ## 1566 08-2018
    ## 1567 08-2018
    ## 1568 08-2018
    ## 1569 08-2018
    ## 1570 08-2018
    ## 1571 08-2018
    ## 1572 08-2018
    ## 1573 08-2018
    ## 1574 08-2018
    ## 1575 08-2018
    ## 1576 08-2018
    ## 1577 08-2018
    ## 1578 08-2018
    ## 1579 08-2018
    ## 1580 08-2018
    ## 1581 08-2018
    ## 1582 08-2018
    ## 1583 08-2018
    ## 1584 08-2018
    ## 1585 08-2018
    ## 1586 08-2018
    ## 1587 08-2018
    ## 1588 08-2018
    ## 1589 08-2018
    ## 1590 08-2018
    ## 1591 08-2018
    ## 1592 08-2018
    ## 1593 08-2018
    ## 1594 08-2018
    ## 1595 08-2018
    ## 1596 09-2018
    ## 1597 09-2018
    ## 1598 09-2018
    ## 1599 09-2018
    ## 1600 09-2018
    ## 1601 09-2018
    ## 1602 09-2018
    ## 1603 09-2018
    ## 1604 09-2018
    ## 1605 09-2018
    ## 1606 09-2018
    ## 1607 09-2018
    ## 1608 09-2018
    ## 1609 09-2018
    ## 1610 09-2018
    ## 1611 09-2018
    ## 1612 09-2018
    ## 1613 09-2018
    ## 1614 09-2018
    ## 1615 09-2018
    ## 1616 09-2018
    ## 1617 09-2018
    ## 1618 09-2018
    ## 1619 09-2018
    ## 1620 09-2018
    ## 1621 09-2018
    ## 1622 09-2018
    ## 1623 09-2018
    ## 1624 09-2018
    ## 1625 09-2018
    ## 1626 09-2018
    ## 1627 09-2018
    ## 1628 09-2018
    ## 1629 09-2018
    ## 1630 09-2018
    ## 1631 09-2018
    ## 1632 09-2018
    ## 1633 09-2018
    ## 1634 09-2018
    ## 1635 09-2018
    ## 1636 09-2018
    ## 1637 09-2018
    ## 1638 09-2018
    ## 1639 09-2018
    ## 1640 09-2018
    ## 1641 09-2018
    ## 1642 09-2018
    ## 1643 09-2018
    ## 1644 09-2018
    ## 1645 09-2018
    ## 1646 09-2018
    ## 1647 09-2018
    ## 1648 09-2018
    ## 1649 09-2018
    ## 1650 09-2018
    ## 1651 09-2018
    ## 1652 09-2018
    ## 1653 09-2018
    ## 1654 09-2018
    ## 1655 09-2018
    ## 1656 09-2018
    ## 1657 09-2018
    ## 1658 09-2018
    ## 1659 09-2018
    ## 1660 09-2018
    ## 1661 09-2018
    ## 1662 09-2018
    ## 1663 09-2018
    ## 1664 09-2018
    ## 1665 09-2018
    ## 1666 09-2018
    ## 1667 09-2018
    ## 1668 09-2018
    ## 1669 09-2018
    ## 1670 09-2018
    ## 1671 09-2018
    ## 1672 09-2018
    ## 1673 09-2018
    ## 1674 09-2018
    ## 1675 09-2018
    ## 1676 09-2018
    ## 1677 09-2018
    ## 1678 09-2018
    ## 1679 09-2018
    ## 1680 09-2018
    ## 1681 09-2018
    ## 1682 09-2018
    ## 1683 09-2018
    ## 1684 09-2018
    ## 1685 09-2018
    ## 1686 09-2018
    ## 1687 09-2018
    ## 1688 09-2018
    ## 1689 09-2018
    ## 1690 09-2018
    ## 1691 09-2018
    ## 1692 09-2018
    ## 1693 09-2018
    ## 1694 09-2018
    ## 1695 09-2018
    ## 1696 09-2018
    ## 1697 09-2018
    ## 1698 09-2018
    ## 1699 09-2018
    ## 1700 09-2018
    ## 1701 09-2018
    ## 1702 09-2018
    ## 1703 09-2018
    ## 1704 09-2018
    ## 1705 09-2018
    ## 1706 09-2018
    ## 1707 09-2018
    ## 1708 09-2018
    ## 1709 09-2018
    ## 1710 09-2018
    ## 1711 09-2018
    ## 1712 09-2018
    ## 1713 09-2018
    ## 1714 09-2018
    ## 1715 09-2018
    ## 1716 09-2018
    ## 1717 09-2018
    ## 1718 09-2018
    ## 1719 09-2018
    ## 1720 09-2018
    ## 1721 09-2018
    ## 1722 09-2018
    ## 1723 09-2018
    ## 1724 09-2018
    ## 1725 09-2018
    ## 1726 09-2018
    ## 1727 09-2018
    ## 1728 09-2018
    ## 1729 09-2018
    ## 1730 09-2018
    ## 1731 09-2018
    ## 1732 09-2018
    ## 1733 09-2018
    ## 1734 09-2018
    ## 1735 09-2018
    ## 1736 09-2018
    ## 1737 09-2018
    ## 1738 09-2018
    ## 1739 09-2018
    ## 1740 09-2018
    ## 1741 09-2018
    ## 1742 09-2018
    ## 1743 09-2018
    ## 1744 09-2018
    ## 1745 09-2018
    ## 1746 09-2018
    ## 1747 09-2018
    ## 1748 09-2018
    ## 1749 09-2018
    ## 1750 09-2018
    ## 1751 09-2018
    ## 1752 09-2018
    ## 1753 09-2018
    ## 1754 09-2018
    ## 1755 09-2018
    ## 1756 09-2018
    ## 1757 09-2018
    ## 1758 09-2018
    ## 1759 09-2018
    ## 1760 09-2018
    ## 1761 09-2018
    ## 1762 09-2018
    ## 1763 09-2018
    ## 1764 09-2018
    ## 1765 09-2018
    ## 1766 09-2018
    ## 1767 09-2018
    ## 1768 09-2018
    ## 1769 09-2018
    ## 1770 09-2018
    ## 1771 09-2018
    ## 1772 09-2018
    ## 1773 09-2018
    ## 1774 09-2018
    ## 1775 09-2018
    ## 1776 09-2018
    ## 1777 09-2018
    ## 1778 09-2018
    ## 1779 09-2018
    ## 1780 09-2018
    ## 1781 09-2018
    ## 1782 09-2018
    ## 1783 09-2018
    ## 1784 10-2018
    ## 1785 10-2018
    ## 1786 10-2018
    ## 1787 10-2018
    ## 1788 10-2018
    ## 1789 10-2018
    ## 1790 10-2018
    ## 1791 10-2018
    ## 1792 10-2018
    ## 1793 10-2018
    ## 1794 10-2018
    ## 1795 10-2018
    ## 1796 10-2018
    ## 1797 10-2018
    ## 1798 10-2018
    ## 1799 10-2018
    ## 1800 10-2018
    ## 1801 10-2018
    ## 1802 10-2018
    ## 1803 10-2018
    ## 1804 10-2018
    ## 1805 10-2018
    ## 1806 10-2018
    ## 1807 10-2018
    ## 1808 10-2018
    ## 1809 10-2018
    ## 1810 10-2018
    ## 1811 10-2018
    ## 1812 10-2018
    ## 1813 10-2018
    ## 1814 10-2018
    ## 1815 10-2018
    ## 1816 10-2018
    ## 1817 10-2018
    ## 1818 10-2018
    ## 1819 10-2018
    ## 1820 10-2018
    ## 1821 10-2018
    ## 1822 10-2018
    ## 1823 10-2018
    ## 1824 10-2018
    ## 1825 10-2018
    ## 1826 10-2018
    ## 1827 10-2018
    ## 1828 10-2018
    ## 1829 10-2018
    ## 1830 10-2018
    ## 1831 10-2018
    ## 1832 10-2018
    ## 1833 10-2018
    ## 1834 10-2018
    ## 1835 10-2018
    ## 1836 10-2018
    ## 1837 10-2018
    ## 1838 10-2018
    ## 1839 10-2018
    ## 1840 10-2018
    ## 1841 10-2018
    ## 1842 10-2018
    ## 1843 10-2018
    ## 1844 10-2018
    ## 1845 10-2018
    ## 1846 10-2018
    ## 1847 10-2018
    ## 1848 10-2018
    ## 1849 10-2018
    ## 1850 10-2018
    ## 1851 10-2018
    ## 1852 10-2018
    ## 1853 10-2018
    ## 1854 10-2018
    ## 1855 10-2018
    ## 1856 10-2018
    ## 1857 10-2018
    ## 1858 10-2018
    ## 1859 10-2018
    ## 1860 10-2018
    ## 1861 10-2018
    ## 1862 10-2018
    ## 1863 10-2018
    ## 1864 10-2018
    ## 1865 10-2018
    ## 1866 10-2018
    ## 1867 10-2018
    ## 1868 10-2018
    ## 1869 10-2018
    ## 1870 10-2018
    ## 1871 10-2018
    ## 1872 10-2018
    ## 1873 10-2018
    ## 1874 10-2018
    ## 1875 10-2018
    ## 1876 10-2018
    ## 1877 10-2018
    ## 1878 10-2018
    ## 1879 10-2018
    ## 1880 10-2018
    ## 1881 10-2018
    ## 1882 10-2018
    ## 1883 10-2018
    ## 1884 10-2018
    ## 1885 10-2018
    ## 1886 10-2018
    ## 1887 10-2018
    ## 1888 10-2018
    ## 1889 10-2018
    ## 1890 10-2018
    ## 1891 10-2018
    ## 1892 10-2018
    ## 1893 10-2018
    ## 1894 10-2018
    ## 1895 10-2018
    ## 1896 10-2018
    ## 1897 10-2018
    ## 1898 10-2018
    ## 1899 10-2018
    ## 1900 10-2018
    ## 1901 10-2018
    ## 1902 10-2018
    ## 1903 10-2018
    ## 1904 10-2018
    ## 1905 10-2018
    ## 1906 10-2018
    ## 1907 10-2018
    ## 1908 10-2018
    ## 1909 10-2018
    ## 1910 10-2018
    ## 1911 10-2018
    ## 1912 10-2018
    ## 1913 10-2018
    ## 1914 10-2018
    ## 1915 10-2018
    ## 1916 10-2018
    ## 1917 10-2018
    ## 1918 10-2018
    ## 1919 10-2018
    ## 1920 10-2018
    ## 1921 10-2018
    ## 1922 10-2018
    ## 1923 10-2018
    ## 1924 10-2018
    ## 1925 10-2018
    ## 1926 10-2018
    ## 1927 10-2018
    ## 1928 10-2018
    ## 1929 10-2018
    ## 1930 10-2018
    ## 1931 10-2018
    ## 1932 10-2018
    ## 1933 10-2018
    ## 1934 10-2018
    ## 1935 10-2018
    ## 1936 10-2018
    ## 1937 10-2018
    ## 1938 10-2018
    ## 1939 10-2018
    ## 1940 10-2018
    ## 1941 10-2018
    ## 1942 10-2018
    ## 1943 10-2018
    ## 1944 10-2018
    ## 1945 10-2018
    ## 1946 10-2018
    ## 1947 10-2018
    ## 1948 10-2018
    ## 1949 10-2018
    ## 1950 10-2018
    ## 1951 10-2018
    ## 1952 10-2018
    ## 1953 10-2018
    ## 1954 10-2018
    ## 1955 10-2018
    ## 1956 10-2018
    ## 1957 10-2018
    ## 1958 10-2018
    ## 1959 10-2018
    ## 1960 10-2018
    ## 1961 10-2018
    ## 1962 10-2018
    ## 1963 10-2018
    ## 1964 10-2018
    ## 1965 10-2018
    ## 1966 10-2018
    ## 1967 10-2018
    ## 1968 10-2018
    ## 1969 10-2018
    ## 1970 10-2018
    ## 1971 10-2018
    ## 1972 10-2018
    ## 1973 10-2018
    ## 1974 10-2018
    ## 1975 10-2018
    ## 1976 10-2018
    ## 1977 10-2018
    ## 1978 10-2018
    ## 1979 10-2018
    ## 1980 10-2018
    ## 1981 10-2018
    ## 1982 10-2018
    ## 1983 10-2018
    ## 1984 11-2018
    ## 1985 11-2018
    ## 1986 11-2018
    ## 1987 11-2018
    ## 1988 11-2018
    ## 1989 11-2018
    ## 1990 11-2018
    ## 1991 11-2018
    ## 1992 11-2018
    ## 1993 11-2018
    ## 1994 11-2018
    ## 1995 11-2018
    ## 1996 11-2018
    ## 1997 11-2018
    ## 1998 11-2018
    ## 1999 11-2018
    ## 2000 11-2018
    ## 2001 11-2018
    ## 2002 11-2018
    ## 2003 11-2018
    ## 2004 11-2018
    ## 2005 11-2018
    ## 2006 11-2018
    ## 2007 11-2018
    ## 2008 11-2018
    ## 2009 11-2018
    ## 2010 11-2018
    ## 2011 11-2018
    ## 2012 11-2018
    ## 2013 11-2018
    ## 2014 11-2018
    ## 2015 11-2018
    ## 2016 11-2018
    ## 2017 11-2018
    ## 2018 11-2018
    ## 2019 11-2018
    ## 2020 11-2018
    ## 2021 11-2018
    ## 2022 11-2018
    ## 2023 11-2018
    ## 2024 11-2018
    ## 2025 11-2018
    ## 2026 11-2018
    ## 2027 11-2018
    ## 2028 11-2018
    ## 2029 11-2018
    ## 2030 11-2018
    ## 2031 11-2018
    ## 2032 11-2018
    ## 2033 11-2018
    ## 2034 11-2018
    ## 2035 11-2018
    ## 2036 11-2018
    ## 2037 11-2018
    ## 2038 11-2018
    ## 2039 11-2018
    ## 2040 11-2018
    ## 2041 11-2018
    ## 2042 11-2018
    ## 2043 11-2018
    ## 2044 11-2018
    ## 2045 11-2018
    ## 2046 11-2018
    ## 2047 11-2018
    ## 2048 11-2018
    ## 2049 11-2018
    ## 2050 11-2018
    ## 2051 11-2018
    ## 2052 11-2018
    ## 2053 11-2018
    ## 2054 11-2018
    ## 2055 11-2018
    ## 2056 11-2018
    ## 2057 11-2018
    ## 2058 11-2018
    ## 2059 11-2018
    ## 2060 11-2018
    ## 2061 11-2018
    ## 2062 11-2018
    ## 2063 11-2018
    ## 2064 11-2018
    ## 2065 11-2018
    ## 2066 11-2018
    ## 2067 11-2018
    ## 2068 11-2018
    ## 2069 11-2018
    ## 2070 11-2018
    ## 2071 11-2018
    ## 2072 11-2018
    ## 2073 11-2018
    ## 2074 11-2018
    ## 2075 11-2018
    ## 2076 11-2018
    ## 2077 11-2018
    ## 2078 11-2018
    ## 2079 11-2018
    ## 2080 11-2018
    ## 2081 11-2018
    ## 2082 11-2018
    ## 2083 11-2018
    ## 2084 11-2018
    ## 2085 11-2018
    ## 2086 11-2018
    ## 2087 11-2018
    ## 2088 11-2018
    ## 2089 11-2018
    ## 2090 11-2018
    ## 2091 11-2018
    ## 2092 11-2018
    ## 2093 11-2018
    ## 2094 11-2018
    ## 2095 11-2018
    ## 2096 11-2018
    ## 2097 11-2018
    ## 2098 11-2018
    ## 2099 11-2018
    ## 2100 11-2018
    ## 2101 11-2018
    ## 2102 11-2018
    ## 2103 11-2018
    ## 2104 11-2018
    ## 2105 11-2018
    ## 2106 11-2018
    ## 2107 11-2018
    ## 2108 11-2018
    ## 2109 11-2018
    ## 2110 11-2018
    ## 2111 11-2018
    ## 2112 11-2018
    ## 2113 11-2018
    ## 2114 11-2018
    ## 2115 11-2018
    ## 2116 11-2018
    ## 2117 11-2018
    ## 2118 11-2018
    ## 2119 11-2018
    ## 2120 11-2018
    ## 2121 11-2018
    ## 2122 11-2018
    ## 2123 11-2018
    ## 2124 11-2018
    ## 2125 11-2018
    ## 2126 11-2018
    ## 2127 11-2018
    ## 2128 11-2018
    ## 2129 11-2018
    ## 2130 11-2018
    ## 2131 11-2018
    ## 2132 11-2018
    ## 2133 11-2018
    ## 2134 11-2018
    ## 2135 11-2018
    ## 2136 11-2018
    ## 2137 11-2018
    ## 2138 11-2018
    ## 2139 11-2018
    ## 2140 11-2018
    ## 2141 11-2018
    ## 2142 11-2018
    ## 2143 11-2018
    ## 2144 11-2018
    ## 2145 11-2018
    ## 2146 11-2018
    ## 2147 11-2018
    ## 2148 11-2018
    ## 2149 11-2018
    ## 2150 11-2018
    ## 2151 11-2018
    ## 2152 11-2018
    ## 2153 11-2018
    ## 2154 11-2018
    ## 2155 11-2018
    ## 2156 11-2018
    ## 2157 11-2018
    ## 2158 11-2018
    ## 2159 11-2018
    ## 2160 11-2018
    ## 2161 11-2018
    ## 2162 11-2018
    ## 2163 11-2018
    ## 2164 11-2018
    ## 2165 11-2018
    ## 2166 11-2018
    ## 2167 11-2018
    ## 2168 11-2018
    ## 2169 11-2018
    ## 2170 11-2018
    ## 2171 11-2018
    ## 2172 11-2018
    ## 2173 11-2018
    ## 2174 11-2018
    ## 2175 11-2018
    ## 2176 11-2018
    ## 2177 11-2018
    ## 2178 11-2018
    ## 2179 11-2018
    ## 2180 11-2018

``` r
# exportar excel
write_xlsx(final, "final_lab1.xls")
```

``` r
# problema 2 generando lista de 3 vectores con elementos "random"
generate_df <- function(x, tamanio){
  return(
    data.frame(
      a = sample(letters, size = tamanio, replace = TRUE),
      b = sample(1:10, size = tamanio, replace = TRUE),
      c = sample(letters, size = tamanio, replace = TRUE)
    )
  )
}

lista_3v <- lapply(1, generate_df, tamanio = 3)
lista_3v
```

    ## [[1]]
    ##   a b c
    ## 1 d 7 d
    ## 2 a 5 a
    ## 3 d 8 m

``` r
# obtener la moda
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

to_mode <- as.data.frame(lista_3v)

lapply(to_mode, Mode)
```

    ## $a
    ## [1] "d"
    ## 
    ## $b
    ## [1] 7
    ## 
    ## $c
    ## [1] "d"
