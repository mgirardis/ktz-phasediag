program main
    use precision
    use Input
    use Output
    use Simulation
    implicit none
    !external EscreveArquivo
    character(len=80), allocatable :: args(:)
    integer(kind=4) :: nArgs, i, ios
    real(kr8), allocatable :: isiData(:), parAData(:), parBData(:), intData(:)
    character(len=1024) :: headerSaida
    
    call inicializaParametros()

    nArgs = command_argument_count()
    if (nArgs >= 1) then
        allocate(args(1:nArgs))
        do i = 1, nArgs
            call get_command_argument(i, args(i))
        end do
        write (*, '(A)') 'arguments: '
        write (*, '(A)') (trim(args(i)),i=1,nArgs)
        call ajustaParametros(args, nArgs, ios)
        if (ios == -1) then
            ! error occurred during the processing of the parameters
            stop
        end if
    end if

    write (*,*) 'Simulando ...'
    call Simula(parBData, parAData, isiData, intData)

    if (.not.par%writeOnRun) then
        headerSaida = trim(pegaStrParamEntrada())// &
                      "# "//trim(par%parB)//"     "//trim(par%parA)//&
                      "     "//trim(par%measure)//"     intensity"
        call EscreveArquivo(parBData, &
                            parAData, &
                            isiData, &
                            intData, &
                            "(4D17.8)", &
                            headerSaida, &
                            pegaNomeArqSaida())
    end if

end program main
