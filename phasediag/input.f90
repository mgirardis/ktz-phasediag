module Input

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!
    !!!!
    !!!! HOW TO ADD AN INPUT PARAMETER TO THE SIMULATION
    !!!!
    !!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!
    !!!! Lets add a parameter called MYPARAM
    !!!!
    !!!! 1. add a "character parameter" below named par_MYPARAM
    !!!!
    !!!! 2. Modify the "PrintHelp" to include a description of the parameter in the help
    !!!!
    !!!! 3. Add it to "inputParam" struct
    !!!!
    !!!! 4. Initialize it in "inicializaParametros"
    !!!!
    !!!! 5. Add it to the output file header in "pegaStrParamEntrada"
    !!!!
    !!!! 6. if needed, add it to the output file name in "pegaNomeArqSaida"
    !!!!
    !!!! 7. Add a conditional statement to "ajustaParametros" to get the input from the user for the newly added par_MYPARAM
    !!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    use precision
    private
    character(len=20) , parameter :: par_parA          = "parA"
    character(len=20) , parameter :: par_parA1         = "parA1"
    character(len=20) , parameter :: par_parA2         = "parA2"
    character(len=20) , parameter :: par_nparA         = "nparA"
    character(len=20) , parameter :: par_parAScale     = "parAScale"
    character(len=20) , parameter :: par_parB          = "parB"
    character(len=20) , parameter :: par_parB1         = "parB1"
    character(len=20) , parameter :: par_parB2         = "parB2"
    character(len=20) , parameter :: par_nparB         = "nparB"
    character(len=20) , parameter :: par_parBScale     = "parBScale"
    character(len=20) , parameter :: par_tTransient    = "tTransient"
    character(len=20) , parameter :: par_tTotal        = "tTotal"
    character(len=20) , parameter :: par_K             = "K"
    character(len=20) , parameter :: par_T             = "T"
    character(len=20) , parameter :: par_d             = "d"
    character(len=20) , parameter :: par_l             = "l"
    character(len=20) , parameter :: par_xR            = "xR"
    character(len=20) , parameter :: par_H             = "H"
    character(len=20) , parameter :: par_Z             = "Z"
    character(len=20) , parameter :: par_x0            = "x0"
    character(len=20) , parameter :: par_y0            = "y0"
    character(len=20) , parameter :: par_z0            = "z0"
    character(len=20) , parameter :: par_model         = "model"
    character(len=20) , parameter :: par_measure       = "measure"
    character(len=20) , parameter :: par_xThreshold    = "xThreshold"
    character(len=20) , parameter :: par_writeOnRun    = "writeOnRun"
    character(len=20) , parameter :: par_correctISI    = "correctISI"
    character(len=20) , parameter :: par_outFileSuffix = "outFileSuffix"
    character(len=20) , parameter :: par_outFileDir    = "outFileDir"
    character(len=20) , parameter :: par_periodTol     = "periodTol"
    character(len=20) , parameter :: par_periodMethod  = "periodMethod"
    character(len=1)  , parameter :: tabchar           = char(9)
    character(len=1)  , parameter :: nlchar            = char(10)

    type inputParam
        real(kr8)          :: parB1, parB2, parA1, parA2, K, T, d, l, xR, H, Z
        real(kr8)          :: x0(3), xThreshold, periodTol
        integer(kind=4)    :: nparA, nparB, tTotal, tTransient
        character(len=1)   :: model
        character(len=2)   :: parA, parB
        character(len=3)   :: measure, parAScale, parBScale
        character(len=30)  :: lastOutputColName, intensityColName, measureColName
        character(len=30)  :: periodMethod
        character(len=100) :: outFileDir
        character(len=128) :: outFileSuffix
        logical            :: writeOnRun
        logical            :: correctISI
    end type inputParam

    type(inputParam) :: par

    public :: ajustaParametros, inicializaParametros
    public :: pegaStrParamEntrada, pegaNomeArqSaida
    public :: par, getOutFileHeader, getOutputFormatStr
    private :: PrintHelp, setDependentParameters

contains

    function getOutFileHeader() result(str)
        character(len=1024) :: str

        str = trim(trim(pegaStrParamEntrada())// &
                    "# "//trim(par%parB)//&
                    "     "//trim(par%parA)//&
                    "     "//trim(par%measureColName)//&
                    "     "//trim(par%intensityColName)//&
                    "     "//trim(par%lastOutputColName))
    end function getOutFileHeader

    function pegaStrParamEntrada() result (str)
        character(len=1024) :: str

        write(str,'(A,&
                    A,F16.8,A,&
                    A,F16.8,A,&
                    A,I0,A,&
                    A,&
                    A,&
                    A,F16.8,A,&
                    A,F16.8,A,&
                    A,I0,A,&
                    A,&
                    A,I0,A,&
                    A,I0,A,&
                    A,F16.8,A,&
                    A,F16.8,A,&
                    A,F16.8,A,&
                    A,F16.8,A,&
                    A,F16.8,A,&
                    A,F16.8,A,&
                    A,F16.8,A,&
                    A,&
                    A,&
                    A,&
                    A,3D17.8,A,&
                    A,F16.8,A,&
                    A,F16.8,A)')&
        '# '//trim(par_parA)//' = '//par%parA//nlchar,&
        '# '//trim(par_parA1)//' = ', par%parA1, nlchar,&
        '# '//trim(par_parA2)//' = ', par%parA2, nlchar,&
        '# '//trim(par_nparA)//' = ', par%nparA, nlchar,&
        '# '//trim(par_parAScale)//' = '//par%parAScale//nlchar,&
        '# '//trim(par_parB)//' = '//par%parB//nlchar,&
        '# '//trim(par_parB1)//' = ', par%parB1, nlchar,&
        '# '//trim(par_parB2)//' = ', par%parB2, nlchar,&
        '# '//trim(par_nparB)//' = ', par%nparB, nlchar,&
        '# '//trim(par_parBScale)//' = '//par%parBScale//nlchar,&
        '# '//trim(par_tTotal)//' = ', par%tTotal, nlchar,&
        '# '//trim(par_tTransient)//' = ', par%tTransient, nlchar,&
        '# '//trim(par_K)//' = ', par%K, nlchar,&
        '# '//trim(par_T)//' = ', par%T, nlchar,&
        '# '//trim(par_d)//' = ', par%d, nlchar,&
        '# '//trim(par_l)//' = ', par%l, nlchar,&
        '# '//trim(par_xR)//' = ', par%xR, nlchar,&
        '# '//trim(par_Z)//' = ', par%Z, nlchar,&
        '# '//trim(par_H)//' = ', par%H, nlchar,&
        '# '//trim(par_model)//' = '//par%model//nlchar,&
        '# '//trim(par_measure)//' = '//par%measure//nlchar,&
        '# '//trim(par_periodMethod)//' = '//par%periodMethod//nlchar,&
        '# '//trim(par_x0)//' = ', par%x0, nlchar,&
        '# '//trim(par_periodTol)//' = ', par%periodTol, nlchar,&
        '# '//trim(par_xThreshold)//' = ', par%xThreshold, nlchar
    end function pegaStrParamEntrada

    function pegaNomeArqSaida() result (nome)
        character(len=512) :: nome
        !write(*,*) 'c4'
        !write(*,*) trim(par%outFileSuffix)
        write (nome,'(A,F6.4,&
                      A,F6.4,&
                      A,F6.4,&
                      A,F6.4,&
                      A,F6.4,&
                      A,F6.4,&
                      A,F6.4,&
                      A,F6.4,&
                      A)') &
            trim(par%outFileDir)//'/'//trim(par%measure)//'_'//trim(par%parA)//'vs'//trim(par%parB)//&
            '_K',par%K,&
            '_T',par%T,&
            '_d',par%d,&
            '_l',par%l,&
            '_xR',par%xR,&
            '_Z',par%Z,&
            '_H',par%H,&
            '_th',par%xThreshold,&
            '_model'//trim(par%model)//'_'//trim(par%outFileSuffix)//'.dat'
        !write(*,*) 'c5'
    end function pegaNomeArqSaida

    subroutine inicializaParametros()
        par%parA              = "T"
        par%parAScale         = "LIN" ! LIN or LOG
        par%parA1             = 1.0D-3
        par%parA2             = 1.0D0
        par%nparA             = 1000
        par%parB              = "xR"
        par%parBScale         = "LIN" ! LIN or LOG
        par%parB1             = 0.0D0
        par%parB2             = -1.0D0
        par%nparB             = 100
        par%tTotal            = 10000
        par%tTransient        = 2000
        par%K                 = 0.6D0
        par%T                 = 0.3D0
        par%d                 = 0.001D0
        par%l                 = 0.001D0
        par%xR                = -0.5D0
        par%Z                 = 0.0D0
        par%H                 = 0.0D0
        par%model             = "L" ! L or T or 2
        par%measure           = "ISI" ! ISI or AMP or WIN
        par%periodMethod      = "aftersim1" ! onthefly OR aftersim1 OR aftersim2
        par%x0                = (/ 1.0, 1.0, 1.0 /)
        par%xThreshold        = 0.0D0
        par%periodTol         = 1.0D-8
        par%writeOnRun        = .false.
        par%correctISI        = .true.
        par%outFileSuffix     = ''
        par%outFileDir        = '.'
    end subroutine inicializaParametros
    
    subroutine setDependentParameters(ios)
        integer, intent(inout) :: ios

        if (trim(par%measure) == "ISI") then
            par%measureColName    = "ISI"
            par%intensityColName  = "intensity"
            par%lastOutputColName = "ISIPeriod"
        else if (trim(par%measure) == "WIN") then
            par%measureColName    = "WindingNum"
            par%intensityColName  = "Cycles"
            par%lastOutputColName = "Period"
        else if (trim(par%measure) == "AMP") then
            par%measureColName    = "Amplitude"
            par%intensityColName  = "intensity"
            par%lastOutputColName = "LyapExp"
        else
            write (*,*) "ERROR ::: UNKNOWN VALUE FOR PARAMETER measure = ",par%measure
            ios = -1
            return
        end if
        if (.not.((trim(par%periodMethod) == "onthefly")  .or. &
                  (trim(par%periodMethod) == "aftersim1") .or. &
                  (trim(par%periodMethod) == "aftersim2"))       ) then
            write (*,*) "ERROR ::: UNKNOWN VALUE FOR PARAMETER periodMethod = ",par%periodMethod
            ios = -1
            return
        end if
        if ((par%d == 0.0D0) .and. (par%l == 0.0D0)) then
            par%x0(3) = 0.0D0
            write (*,*) "WARNING ::: FORCING x0(3) = 0 BECAUSE delta=lambda=0 (SIMULATION of static KT model)"
        end if
    end subroutine setDependentParameters

    function getOutputFormatStr() result(str)
        character(len=256) :: str
        
        str = "(5D20.12)"
        return
        ! writing output as I20 (integer) messes up with the "Infinite" values generated in the simulation (1.0/0.0)
        if (trim(par%measure) == "ISI") then
            str = "(2D20.12, 1X, I20, 1X, D20.12, 1X, I20)" 
        else if (trim(par%measure) == "WIN") then
            str = "(3D20.12, 1X, I20, 1X, I20)"
        else ! par%measure == "AMP" or any other thing
            str = "(5D20.12)"
        end if
    end function getOutputFormatStr

    ! adjusts the values of the parameters defined in the file
    ! 01_varglob.f90
    ! according to the arguments passed to the program
    ! the arguments should have the following syntax:
    ! PARAM=VALUE
    ! where PARAM is the variable name and VALUE is its value
    subroutine ajustaParametros(args, nArgs, ios)
        use tokenizestr ! provide method to split strings
        use strings ! contains the functions to convert strings to numbers
        integer, intent(in) :: nArgs
        integer, intent(out) :: ios
        integer :: i, j, strLength, tempVal, temp_tTotal
        character(len=*), intent(in) :: args(nArgs)
        character(len=128), dimension(2) :: parVal
        type(tokenizer) :: splitStruct
        
        ios = 0 ! everything is ok
        temp_tTotal = -1

        ! preparing the split routine
        ! the constant token_empty is defined in the tokenizestr module
        call set_tokenizer(splitStruct, token_empty, '=', token_empty)
        do i = 1, nArgs

            if ((args(i) == "help") .or. (args(i) == "-h")) then
                call PrintHelp()
                ios = -1
                return
            end if

            parVal(1) = trim(first_token(splitStruct, args(i), strLength))
            parVal(2) = trim(next_token(splitStruct, args(i), strLength))

            if (len_trim(parVal(2)) == 0) then
                write (*,*) 'ERROR: unrecognized value for ', parVal(1), ' = ', parVal(2)
                write (*,*) 'parameters must be set: PAR=VALUE'
                stop
            end if

            write (*,'(2A)') "ajustando: ", trim(parVal(1))
            
            if (parVal(1) == par_parA) then
                par%parA = trim(parVal(2))
            else if (parVal(1) == par_parA1) then
                par%parA1 = strToDouble(parVal(2))
            else if (parVal(1) == par_parA2) then
                par%parA2 = strToDouble(parVal(2))
            else if (parVal(1) == par_parB) then
                par%parB = trim(parVal(2))
            else if (parVal(1) == par_parB1) then
                par%parB1 = strToDouble(parVal(2))
            else if (parVal(1) == par_parB2) then
                par%parB2 = strToDouble(parVal(2))
            else if (parVal(1) == par_K) then
                par%K = strToDouble(parVal(2))
            else if (parVal(1) == par_T) then
                par%T = strToDouble(parVal(2))
            else if (parVal(1) == par_d) then
                par%d = strToDouble(parVal(2))
            else if (parVal(1) == par_l) then
                par%l = strToDouble(parVal(2))
            else if (parVal(1) == par_xR) then
                par%xR = strToDouble(parVal(2))
            else if (parVal(1) == par_Z) then
                par%Z = strToDouble(parVal(2))
            else if (parVal(1) == par_H) then
                par%H = strToDouble(parVal(2))
            else if (parVal(1) == par_x0) then
                par%x0(1) = strToDouble(parVal(2))
            else if (parVal(1) == par_y0) then
                par%x0(2) = strToDouble(parVal(2))
            else if (parVal(1) == par_z0) then
                par%x0(3) = strToDouble(parVal(2))
            else if (parVal(1) == par_xThreshold) then
                par%xThreshold = strToDouble(parVal(2))
            else if (parVal(1) == par_periodTol) then
                par%periodTol = strToDouble(parVal(2))
            else if (parVal(1) == par_tTotal) then
                par%tTotal = strToInteger(parVal(2))
            else if (parVal(1) == par_tTransient) then
                par%tTransient = strToInteger(parVal(2))
            else if (parVal(1) == par_nparA) then
                par%nparA = strToInteger(parVal(2))
            else if (parVal(1) == par_nparB) then
                par%nparB = strToInteger(parVal(2))
            else if (parVal(1) == trim(par_model)) then
                par%model = trim(parVal(2))
            else if (parVal(1) == trim(par_measure)) then
                par%measure = trim(parVal(2))
            else if (parVal(1) == trim(par_periodMethod)) then
                par%periodMethod = trim(parVal(2))
            else if (parVal(1) == trim(par_parAScale)) then
                par%parAScale = trim(parVal(2))
            else if (parVal(1) == trim(par_parBScale)) then
                par%parBScale = trim(parVal(2))
            else if (parVal(1) == par_outFileSuffix) then
                par%outFileSuffix = trim(parVal(2))
            else if (parVal(1) == par_outFileDir) then
                par%outFileDir = trim(parVal(2))
            else if (parVal(1) == par_writeOnRun) then
                if (strToInteger(parVal(2)) == 1) then
                    par%writeOnRun = .true.
                else
                    par%writeOnRun = .false.
                end if
            else if (parVal(1) == par_correctISI) then
                if (strToInteger(parVal(2)) == 1) then
                    par%correctISI = .true.
                else
                    par%correctISI = .false.
                end if
            else
                write (*,*) "ERROR: unrecognized parameter: "//trim(parVal(1))
                ios = -1
                return
            end if
            
            write (*,*) "-> ", trim(parVal(1)), " = ", trim(parVal(2))

        end do

        call setDependentParameters(ios)

    end subroutine ajustaParametros

    subroutine PrintHelp()
        integer :: writeonrun_val, correctisi_val
        if (par%writeOnRun) then
            writeonrun_val = 1
        else
            writeonrun_val = 0
        end if
        if (par%correctISI) then
            correctisi_val = 1
        else
            correctisi_val = 0
        end if
        
        write (*,*) 'itera KTzLog, ou KTzTanh, ou K2Tz por tTotal passos de tempo, ignorando os tTransient primeiros passos'
        write (*,*) 'calcula ISI(parA,parB) e periodo[ISI](parA,parB), ou Amplitude(parA,parB) e Lyapunov(parA,parB)'
        write (*,*) 'onde parA e parB podem ser quaisquer dos parametros do modelo (K,T,d,l,xR,H,Z)'
        write (*,*) ' '
        write (*,*) '****************************** '
        write (*,*) ' '
        write (*,*) 'OBS: para calcular Z estático, usar delta=lambda=z0=0'
        write (*,*) ' '
        write (*,*) '****************************** '
        write (*,*) ' '
        write (*,*) 'o eixo x serah o parametro parA com nparA valores no intervalo [parA1,parA2]'
        write (*,*) 'o eixo y serah o parametro parB com nparB valores no intervalo [parB1,parB2]'
        write (*,*) 'em ambos os eixos, a escala pode ser'
        write (*,*) '   logaritmica [parAScale=LOG ou parBScale=LOG]'
        write (*,*) '   linear [parAScale=LIN ou parBScale=LIN]'
        write (*,*) ' '
        write (*,*) '****************************** '
        write (*,*) ' '
        write (*,*) '-'
        write (*,*) 'MEDIDAS: (parametro measure)'
        write (*,*) ' measure=ISI -> calcula o ISI e a periodicidade de cada ISI'
        write (*,*) '                ATENCAO: parametro correctISI torna todos os ISI inteiros,'
        write (*,*) '                         ou seja, faz com que 11.5 seja igual a 11 ou 12'
        write (*,*) '                         entao muda o resultado da periodicidade'
        write (*,*) ' '
        write (*,*) ' measure=AMP -> calcula a amplitude e o maior expoente de Lyapunov'
        write (*,*) ' '
        write (*,*) ' measure=WIN -> calcula o winding number do atrator (w = nro ciclos / periodo)'
        write (*,*) '                quantos ciclos ocorrem em um periodo'
        write (*,*) '                ATENCAO: o maximo periodo eh tEff = tTotal - tTrans'
        write (*,*) ' '
        write (*,*) '****************************** '
        write (*,*) ' '
        write (*,*) '-'
        write (*,*) 'EQUACOES: (parametro model)'
        write (*,*) ' model=T'
        write (*,*) '      x(t+1) = Tanh[ (x(t) - K*y(t) + z(t) + H)/T ]'
        write (*,*) '      y(t+1) = x(t)'
        write (*,*) '      z(t+1) = [1-d]*z(t) - l*[x(t) - xR]'
        write (*,*) '   ref -> Kinouchi Tragtenberg (1996) Intl J Bif Chaos 6: 2343-2360'
        write (*,*) '   ref -> Kuva et al. (2001) Neurocomputing 38-40: 255-261'
        write (*,*) ' model=L'
        write (*,*) '      x(t+1) = f[ (x(t) - K*y(t) + z(t) + H)/T ]'
        write (*,*) '               f(u) = u/(1+|u|)'
        write (*,*) '      y(t+1) = x(t)'
        write (*,*) '      z(t+1) = [1-d]*z(t) - l*[x(t) - xR]'
        write (*,*) '   ref -> DOI:10.1371/journal.pone.0174621'
        write (*,*) ' model=2'
        write (*,*) '      x(t+1) = Tanh[ (x(t) - K*y(t) + z(t) + Z)/T ]'
        write (*,*) '      y(t+1) = Tanh[ (x(t) + H)/T ]'
        write (*,*) '      z(t+1) = [1-d]*z(t) - l*[x(t) - xR]'
        write (*,*) '   ref -> DOI:10.1063/5.0202743'
        write (*,*) ' '
        write (*,*) '****************************** '
        write (*,*) ' '
        write (*,*) '-'
        write (*,*) 'EXEMPLO:'
        write (*,*) ' isi.exe parA=T nparA=100 parA1=0.001 parA2=0.6'
        write (*,*) '        parB=xR nparB=100 parB1=0 parB2=-0.7'
        write (*,*) '        K=0.6 d=0.001 l=0.001 model=L measure=ISI tTotal=10000 tTransient=2000'
        write (*,*) '        writeOnRun=1'
        write (*,*) ' '
        write (*,*) '-> o comando acima vai dividir o eixo T vs. xR em 100 valores Vs. 100 valores'
        write (*,*) '   nos intervalos de T em [0.001,0.6] e xR em [-0.7,0], vai rodar'
        write (*,*) '   o mapa pra cada combinacao de parametros nesse plano'
        write (*,*) '   por 10000 passos de tempo, ignorar os primeiros 2000 passos, e medir o ISI'
        write (*,*) '   os dados vao ser escritos a medida que os calculos sao feitos pra economizar'
        write (*,*) '   memoria'
        write (*,*) ' '
        write (*,*) '****************************** '
        write (*,*) ' '
        write (*,*) '-'
        write (*,*) 'COMO USAR:'
        write (*,*) ' '
        write (*,*) 'isi.exe &
                        parA=NOME_PAR &
                        parAScale=LOG_or_LIN &
                        parA1=VALOR_NUMERICO parA2=VALOR_NUMERICO &
                        nparA=VALOR_NUMERICO &
                        parB=NOME_PAR &
                        parBScale=LOG_or_LIN &
                        parB1=VALOR_NUMERICO parB2=VALOR_NUMERICO &
                        nparB=VALOR_NUMERICO & 
                        x0=VALOR_NUMERICO y0=VALOR_NUMERICO z0=VALOR_NUMERICO &
                        K=VALOR_NUMERICO T=VALOR_NUMERICO d=VALOR_NUMERICO l=VALOR_NUMERICO xR=VALOR_NUMERICO &
                        H=VALOR_NUMERICO Z=VALOR_NUMERICO &
                        tTotal=VALOR_NUMERICO tTransient=VALOR_NUMERICO &
                        xThreshold=VALOR_NUMERICO &
                        periodTol=VALOR_NUMERICO &
                        model=L_ou_T_ou_2 measure=ISI_ou_AMP_ou_WIN &
                        periodMethod=onthefly_OU_aftersim1_OU_aftersim2&
                        writeOnRun=0_ou_1 &
                        correctISI=0_ou_1 &
                        outFileSuffix=OUTPUT_FILE_NAME_SUFFIX &
                        outFileDir=OUTPUT_FILE_DIR'
        write (*,*) ' '
        write (*,*) '****************************** '
        write (*,*) ' '
        write (*,*) '-'
        write (*,*) 'ONDE:'
        write (*,*) ' '
        write (*,'(A20,A,A,A)'    ) trim(par_parA)           //tabchar, '-> [padrao: ',trim(par%parA)         ,'] nome do parametro pro eixo X do diag de fases'//nlchar//&
                                                                            tabchar//tabchar//tabchar//tabchar//'possiveis valores: K,T,d,l,xR,H,Z'
        write (*,'(A20,A,A,A)'    ) trim(par_parAScale)      //tabchar, '-> [padrao: ',trim(par%parAScale)    ,'] Possiveis valores variacao linear: LIN ; escala logaritmica: LOG'
        write (*,'(A20,A,F10.5,A)') trim(par_parA1)          //tabchar, '-> [padrao: ',par%parA1              ,'] menor valor de parA no intervalo [parA1;parA2]'
        write (*,'(A20,A,F10.5,A)') trim(par_parA2)          //tabchar, '-> [padrao: ',par%parA2              ,'] maior valor de parA no intervalo [parA1;parA2]'
        write (*,'(A20,A,I10.0,A)') trim(par_nparA)          //tabchar, '-> [padrao: ',par%nparA              ,'] qtd de parA no intervalo [parA1;parA2]'
        write (*,'(A20,A,A,A)'    ) trim(par_parB)           //tabchar, '-> [padrao: ',trim(par%parB)         ,'] nome do parametro pro eixo Y do diag de fases'//nlchar//&
                                                                            tabchar//tabchar//tabchar//tabchar//'possiveis valores: K,T,d,l,xR,H,Z'
        write (*,'(A20,A,A,A)'    ) trim(par_parBScale)      //tabchar, '-> [padrao: ',trim(par%parBScale)    ,'] Possiveis valores variacao linear: LIN ; escala logaritmica: LOG'
        write (*,'(A20,A,F10.5,A)') trim(par_parB1)          //tabchar, '-> [padrao: ',par%parB1              ,'] menor valor de parB no intervalo [parB1;parB2]'
        write (*,'(A20,A,F10.5,A)') trim(par_parB2)          //tabchar, '-> [padrao: ',par%parB2              ,'] maior valor de parB no intervalo [parB1;parB2]'
        write (*,'(A20,A,I10.0,A)') trim(par_nparB)          //tabchar, '-> [padrao: ',par%nparB              ,'] qtd de parB no intervalo [parB1;parB2]'
        write (*,'(A20,A,F10.5,A)') trim(par_x0)             //tabchar, '-> [padrao: ',par%x0(1)              ,'] cond inicial x'
        write (*,'(A20,A,F10.5,A)') trim(par_y0)             //tabchar, '-> [padrao: ',par%x0(2)              ,'] cond inicial y'
        write (*,'(A20,A,F10.5,A)') trim(par_z0)             //tabchar, '-> [padrao: ',par%x0(3)              ,'] cond inicial z (DEVE SER 0 para simular Z estático)'
        write (*,'(A20,A,F10.5,A)') trim(par_xThreshold)     //tabchar, '-> [padrao: ',par%xThreshold         ,'] limiar de x(t) para considerar um disparo'
        write (*,'(A20,A,F10.5,A)') trim(par_periodTol)      //tabchar, '-> [padrao: ',par%periodTol          ,'] tolerancia para medir o periodo do atrator para calcular o winding number'
        write (*,'(A20,A,F10.5,A)') trim(par_K)              //tabchar, '-> [padrao: ',par%K                  ,'] K do neuronio'
        write (*,'(A20,A,F10.5,A)') trim(par_T)              //tabchar, '-> [padrao: ',par%T                  ,'] T do neuronio'
        write (*,'(A20,A,F10.5,A)') trim(par_d)              //tabchar, '-> [padrao: ',par%d                  ,'] delta do neuronio'
        write (*,'(A20,A,F10.5,A)') trim(par_l)              //tabchar, '-> [padrao: ',par%l                  ,'] lambda do neuronio'
        write (*,'(A20,A,F10.5,A)') trim(par_xR)             //tabchar, '-> [padrao: ',par%xR                 ,'] xR do neuronio'
        write (*,'(A20,A,F10.5,A)') trim(par_Z)              //tabchar, '-> [padrao: ',par%Z                  ,'] Z do neuronio (campo externo de x(t) para 2-Tanh)'
        write (*,'(A20,A,F10.5,A)') trim(par_H)              //tabchar, '-> [padrao: ',par%H                  ,'] H do neuronio (campo externo de y(t) para 2-Tanh)'
        write (*,'(A20,A,I10.0,A)') trim(par_tTotal)         //tabchar, '-> [padrao: ',par%tTotal             ,'] total de passos de tempo'
        write (*,'(A20,A,I10.0,A)') trim(par_tTransient)     //tabchar, '-> [padrao: ',par%tTransient         ,'] qtd de passos de tempo transiente a ser ignorada'
        write (*,'(A20,A,A,A)'    ) trim(par_model)          //tabchar, '-> [padrao: ',trim(par%model)        ,'] Possiveis valores modelo logistico: L ; tanh: T ; 2-tanh: 2'
        write (*,'(A20,A,A,A)'    ) trim(par_measure)        //tabchar, '-> [padrao: ',trim(par%measure)      ,'] Possiveis valores medir o ISI: ISI ; medir a amplitude: AMP; medir o winding number: WIN'
        write (*,'(A20,A,A,A)'    ) trim(par_periodMethod)   //tabchar, '-> [padrao: ',trim(par%periodMethod) ,'] Metodo usado para medir o periodo do atrator.'//nlchar//&
                                                                            tabchar//tabchar//tabchar//tabchar//'Valores possiveis:'//nlchar//&
                                                                            tabchar//tabchar//tabchar//tabchar//'onthefly  -> mede durante os calculos (melhor para periodos pequenos)'//nlchar//&
                                                                            tabchar//tabchar//tabchar//tabchar//'aftersim1 -> mede apos a simulacao (mesmo metodo que onthefly, melhor para periodos longos)'//nlchar//&
                                                                            tabchar//tabchar//tabchar//tabchar//'aftersim2 -> mede apos a simulacao (melhor para periodos longos)'
        write (*,'(A20,A,I10.0,A)') trim(par_writeOnRun)     //tabchar, '-> [padrao: ',writeonrun_val         ,'] Possiveis valores: 0 ou 1.'//nlchar//&
                                                                            tabchar//tabchar//tabchar//tabchar//'Se for 1, escreve os arquivos de dados durante a simulacao'//nlchar//&
                                                                            tabchar//tabchar//tabchar//tabchar//'(previne erro por falta de memoria)'
        write (*,'(A20,A,I10.0,A)') trim(par_correctISI)     //tabchar, '-> [padrao: ',correctisi_val         ,'] Possiveis valores: 0 ou 1.'//nlchar//&
                                                                            tabchar//tabchar//tabchar//tabchar//'Se for 1, ISI dentro de +-1 sao considerados iguais: ISI+1=ISI-1=ISI'
        write (*,'(A20,A,A,A)'    ) trim(par_outFileSuffix)  //tabchar, '-> [padrao: ',trim(par%outFileSuffix),'] sufixo pro nome do arq de saida'
        write (*,'(A20,A,A,A)'    ) trim(par_outFileDir)     //tabchar, '-> [padrao: ',trim(par%outFileDir)   ,'] diretorio pro  arq de saida'
        write (*,'(A20,A)'        ) 'help,-h'                //tabchar, '-> mostra essa ajuda'
    end subroutine PrintHelp

end module Input
