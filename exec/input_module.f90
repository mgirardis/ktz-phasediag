module Input
    use precision
    private
    character(len=20), parameter :: par_parA = "parA"
    character(len=20), parameter :: par_parA1 = "parA1"
    character(len=20), parameter :: par_parA2 = "parA2"
    character(len=20), parameter :: par_nparA = "nparA"
    character(len=20), parameter :: par_parB = "parB"
    character(len=20), parameter :: par_parB1 = "parB1"
    character(len=20), parameter :: par_parB2 = "parB2"
    character(len=20), parameter :: par_nparB = "nparB"
    character(len=20), parameter :: par_tTransient = "tTransient"
    character(len=20), parameter :: par_tTotal =  "tTotal"
    character(len=20), parameter :: par_K = "K"
    character(len=20), parameter :: par_T = "T"
    character(len=20), parameter :: par_d = "d"
    character(len=20), parameter :: par_l = "l"
    character(len=20), parameter :: par_xR = "xR"
    character(len=20), parameter :: par_H = "H"
    character(len=20), parameter :: par_x0 = "x0"
    character(len=20), parameter :: par_y0 = "y0"
    character(len=20), parameter :: par_z0 = "z0"
    character(len=20), parameter :: par_model = "model"
    character(len=20), parameter :: par_measure = "measure"
    character(len=20), parameter :: par_xThreshold = "xThreshold"
    character(len=20), parameter :: par_writeOnRun = "writeOnRun"

    type inputParam
        real(kr8) :: parB1, parB2, parA1, parA2, K, T, d, l, xR, H
        real(kr8) :: x0(3), xThreshold
        integer(kind=4) :: nparA, nparB, tTotal, tTransient
        character(len=1) :: model
        character(len=2) :: parA, parB
        character(len=3) :: measure
        logical :: writeOnRun
    end type inputParam

    type(inputParam) :: par

    public :: ajustaParametros, inicializaParametros
    public :: pegaStrParamEntrada, pegaNomeArqSaida
    public :: par
    private :: PrintHelp

contains

    function pegaStrParamEntrada() result (str)
        character(len=1024) :: str

        write(str,'(A,&
                    A,F8.5,A,&
                    A,F8.5,A,&
                    A,I0,A,&
                    A,&
                    A,F8.5,A,&
                    A,F8.5,A,&
                    A,I0,A,&
                    A,I0,A,&
                    A,I0,A,&
                    A,F8.5,A,&
                    A,F8.5,A,&
                    A,F8.5,A,&
                    A,F8.5,A,&
                    A,F8.5,A,&
                    A,F8.5,A,&
                    A,&
                    A,&
                    A,3D17.8,A,&
                    A,F8.5,A)')&
        '# '//trim(par_parA)//' = '//par%parA//char(10),&
        '# '//trim(par_parA1)//' = ', par%parA1, char(10),&
        '# '//trim(par_parA2)//' = ', par%parA2, char(10),&
        '# '//trim(par_nparA)//' = ', par%nparA, char(10),&
        '# '//trim(par_parB)//' = '//par%parB//char(10),&
        '# '//trim(par_parB1)//' = ', par%parB1, char(10),&
        '# '//trim(par_parB2)//' = ', par%parB2, char(10),&
        '# '//trim(par_nparB)//' = ', par%nparB, char(10),&
        '# '//trim(par_tTotal)//' = ', par%tTotal, char(10),&
        '# '//trim(par_tTransient)//' = ', par%tTransient, char(10),&
        '# '//trim(par_K)//' = ', par%K, char(10),&
        '# '//trim(par_T)//' = ', par%T, char(10),&
        '# '//trim(par_d)//' = ', par%d, char(10),&
        '# '//trim(par_l)//' = ', par%l, char(10),&
        '# '//trim(par_xR)//' = ', par%xR, char(10),&
        '# '//trim(par_H)//' = ', par%H, char(10),&
        '# '//trim(par_model)//' = '//par%model//char(10),&
        '# '//trim(par_measure)//' = '//par%measure//char(10),&
        '# '//trim(par_x0)//' = ', par%x0, char(10),&
        '# '//trim(par_xThreshold)//' = ', par%xThreshold, char(10)
    end function pegaStrParamEntrada

    function pegaNomeArqSaida() result (nome)
        character(len=256) :: nome
        write (nome,'(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3,A)') &
            trim(par%measure)//'_'//trim(par%parA)//'vs'//trim(par%parB)//&
            '_K',par%K,'_T',par%T,'_d',par%d,'_l',par%l,'_xR',par%xR,'_H',par%H,&
            '_th',par%xThreshold,&
            '_'//trim(par%model)//'.dat'
    end function pegaNomeArqSaida

    subroutine inicializaParametros()
        par%parA = "T"
        par%parA1 = 1.0D-3
        par%parA2 = 1.0D0
        par%nparA = 1000
        par%parB = "xR"
        par%parB1 = 0.0D0
        par%parB2 = -1.0D0
        par%nparB = 100
        par%tTotal = 10000
        par%tTransient = 2000
        par%K = 0.6D0
        par%T = 0.3D0
        par%d = 0.001D0
        par%l = 0.001D0
        par%xR = -0.5D0
        par%H = 0.0D0
        par%model = "L"
        par%measure = "ISI"
        par%x0 = (/ 1.0, 1.0, 1.0 /)
        par%xThreshold = 0.0D0
        par%writeOnRun = .false.
    end subroutine inicializaParametros

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
        character(len=80), dimension(2) :: parVal
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
            else if (parVal(1) == par_tTotal) then
                par%tTotal = strToInteger(parVal(2))
            else if (parVal(1) == par_tTransient) then
                par%tTransient = strToInteger(parVal(2))
            else if (parVal(1) == par_nparA) then
                par%nparA = strToInteger(parVal(2))
            else if (parVal(1) == par_nparB) then
                par%nparB = strToInteger(parVal(2))
            else if (parVal(1) == par_model) then
                par%model = trim(parVal(2))
            else if (parVal(1) == par_measure) then
                par%measure = trim(parVal(2))
            else if (parVal(1) == par_writeOnRun) then
                if (strToInteger(parVal(2)) == 1) then
                    par%writeOnRun = .true.
                else
                    par%writeOnRun = .false.
                end if
            else
                write (*,*) "ERROR: unrecognized parameter: "//trim(parVal(1))
                ios = -1
                return
            end if
            
            write (*,*) "-> ", trim(parVal(1)), " = ", trim(parVal(2))

        end do
    end subroutine ajustaParametros

    subroutine PrintHelp()
        integer :: temp
        if (par%writeOnRun .eqv. .true.) then
            temp = 1
        else
            temp = 0
        end if
        write (*,*) 'itera KTzLog ou Tanh por tTotal e ignora tTransient passos de tempo'
        write (*,*) 'calcula ISI(parA,parB), onde parA e parB podem ser quaisquer dos'
        write (*,*) 'parametros do model (K,T,d,l,xR)'
        write (*,*) ' '
        write (*,*) 'o eixo x serah o parametro parA com nparA valores no intervalo [parA1,parA2]'
        write (*,*) 'o eixo y serah o parametro parB com nparB valores no intervalo [parB1,parB2]'
        write (*,*) ' '
        write (*,*) '-'
        write (*,*) ' '
        write (*,*) 'exemplo:'
        write (*,*) ' isi.exe parA=T nparA=100 parA1=0.001 parA2=0.6'
        write (*,*) '        parB=xR nparB=100 parB1=0 parB2=-0.7'
        write (*,*) '        K=0.6 d=0.001 l=0.001 model=L measure=ISI tTotal=10000 tTransient=2000'
        write (*,*) '        writeOnRun=1'
        write (*,*) ' '
        write (*,*) '-> esse comando vai dividir o eixo T vs. xR em 100 valores Vs. 100 valores'
        write (*,*) '   nos intervalos de T em [0.001,0.6] e xR em [-0.7,0], vai rodar'
        write (*,*) '   o mapa pra cada combinacao de parametros nesse plano'
        write (*,*) '   por 10000 passos de tempo, ignorar os primeiros 2000 passos, e medir o ISI'
        write (*,*) '   os dados vao ser escritos a medida que os calculos sao feitos pra economizar'
        write (*,*) '   memoria'
        write (*,*) ' '
        write (*,*) '-'
        write (*,*) ' '
        write (*,*) 'uso: $ isi.exe parA=PAR parA1=VALOR_NUMERICO parA2=VALOR_NUMERICO nparA=VALOR_NUMERICO &
                     parB=PAR parB1=VALOR_NUMERICO parB2=VALOR_NUMERICO nparB=VALOR_NUMERICO & 
                     K=VALOR_NUMERICO T=VALOR_NUMERICO d=VALOR_NUMERICO l=VALOR_NUMERICO xR=VALOR_NUMERICO &
                     H=VALOR_NUMERICO tTotal=VALOR_NUMERICO tTransient=VALOR_NUMERICO xThreshold=VALOR_NUMERICO &
                     model=L_OU_T measure=ISI_OU_AMP writeOnRun=0_OU_1'
        write (*,*) ' '
        write (*,*) '-'
        write (*,*) 'onde:'
        write (*,'(A10,A,A,A)') trim(par_parA),'-> (',trim(par%parA),') defines the X axis parameter to vary (K,T,d,l,xR,H)'
        write (*,'(A10,A,F8.5,A)') trim(par_parA1),'-> (',par%parA1,') menor valor de parA no intervalo [parA1;parA2]'
        write (*,'(A10,A,F8.5,A)') trim(par_parA2),'-> (',par%parA2,') maior valor de parA no intervalo [parA1;parA2]'
        write (*,'(A10,A,I0,A)') trim(par_nparA),'-> (',par%nparA,') qtd de parA no intervalo [parA1;parA2]'
        write (*,'(A10,A,A,A)') trim(par_parB),'-> (',trim(par%parB),') defines the Y axis parameter to vary (K,T,d,l,xR,H)'
        write (*,'(A10,A,F8.5,A)') trim(par_parB1),'-> (',par%parB1,') menor valor de parB no intervalo [parB1;parB2]'
        write (*,'(A10,A,F8.5,A)') trim(par_parB2),'-> (',par%parB2,') maior valor de parB no intervalo [parB1;parB2]'
        write (*,'(A10,A,I0,A)') trim(par_nparB),'-> (',par%nparB,') qtd de parB no intervalo [parB1;parB2]'
        write (*,'(A10,A,F8.5,A)') trim(par_x0),'-> (',par%x0(1),') cond inicial x'
        write (*,'(A10,A,F8.5,A)') trim(par_y0),'-> (',par%x0(2),') cond inicial y'
        write (*,'(A10,A,F8.5,A)') trim(par_z0),'-> (',par%x0(3),') cond inicial z'
        write (*,'(A10,A,F8.5,A)') trim(par_xThreshold),'-> (',par%xThreshold,') limiar de x(t) para considerar um disparo'
        write (*,'(A10,A,F8.5,A)') trim(par_K),'-> (',par%K,') K do neuronio'
        write (*,'(A10,A,F8.5,A)') trim(par_d),'-> (',par%d,') delta do neuronio'
        write (*,'(A10,A,F8.5,A)') trim(par_l),'-> (',par%l,') lambda do neuronio'
        write (*,'(A10,A,F8.5,A)') trim(par_H),'-> (',par%H,') H do neuronio'
        write (*,'(A10,A,I0,A)') trim(par_tTotal),'-> (',par%tTotal,') total de passos de tempo'
        write (*,'(A10,A,I0,A)') trim(par_tTransient),'-> (',par%tTransient,') qtd de passos de tempo transiente'
        write (*,'(A10,A,A,A)') trim(par_model),'-> (',trim(par%model),') modelo logistico (L) ou tanh (T)'
        write (*,'(A10,A,A,A)') trim(par_measure),'-> (',trim(par%measure),') measures ISI (ISI) or amplitude (AMP)'
        write (*,'(A10,A,I0,A)') trim(par_writeOnRun),'-> (',temp, &
            ') if 1, then writes data during simulation (prevent out of memory exception)'
        write (*,*) 'help,-h  -> mostra essa ajuda'
    end subroutine PrintHelp

end module Input
