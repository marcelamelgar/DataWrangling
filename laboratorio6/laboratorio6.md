laboratorio6
================
2022-10-12

# Laboratorio 6

### Marcela Melgar, 20200487

``` r
library(stringr)
```

#### Validacion placas vehiculo particular de guatemala

``` r
placas <- c('P371RZJ', 'C459PEP','P472QDL', '829PSQ', 'P777JGF')
str_extract(placas, '^[P]{1}[0-9]{3}(?![AEIOUÑ])[A-Z]{3}$')
```

    ## [1] "P371RZJ" NA        "P472QDL" NA        "P777JGF"

#### Validacion archivo .pdf .jpg

``` r
archivos <- c('Ejemplo1.pdf', 'prueba2.PDF', 'respuestas_del_examen.jpg', 'amor.JPG', 'foto.png')
str_extract(archivos, '[.](JPG|PDF|jpg|pdf)$')
```

    ## [1] ".pdf" ".PDF" ".jpg" ".JPG" NA

#### Validacion contraseñas de correo, 8 caracteres, mayuscula, especial

``` r
contrasenas <- c('helloWorld2', 'marcela1', 'estaEscl4v3', 'Contrasen4', 'ESTA_NO')
str_extract(contrasenas, '^(?=.*[A-Z])(?=.*[a-z])(?=.*[&%@!$*.-_?])[A-Za-z\\d&%@!$*.-_?]{8,}$')
```

    ## [1] "helloWorld2" NA            "estaEscl4v3" "Contrasen4"  NA

#### Validacion de Carnes universidad Galileo segun ano de ingreso 01-30, 00, 1110-8970

``` r
carnes <- c('22006382','00573926','850126593','15005656','12007465')
str_extract(carnes,'^[0-30]\\d{2}[00][1110-8970]{4}')
```

    ## [1] "22006382" NA         NA         "15005656" "12007465"

#### Validacion de pit, spot, spate, slap two, respite que no encuentre pt,Pot,peat,part

``` r
palabras <- c('pt','Pot','peat','part','pit','spot','spate','slap two','respite')
str_extract(palabras, '.*p.t.*')
```

    ## [1] NA         NA         NA         NA         "pit"      "spot"     "spate"   
    ## [8] "slap two" "respite"

#### Validacion correos electronicos ufm

``` r
correos <- c('marcelamelgar@ufm.edu','josereyes@ufm.edu', 'estebancastillo@gmail.com', 'nickolasnolte@ufm.edu')
str_extract(correos, '.*@[ufm]+.(edu)$')
```

    ## [1] "marcelamelgar@ufm.edu" "josereyes@ufm.edu"     NA                     
    ## [4] "nickolasnolte@ufm.edu"
