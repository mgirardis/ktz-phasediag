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
    real :: sim_time
    real, dimension(2) :: timearray
    
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
    call dtime(timearray,sim_time)
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

    call ellapsed_time()

end program main

subroutine ellapsed_time()
    real :: sim_time
    real, dimension(2) :: timearray
    integer :: days,hours,minutes,seconds,mseconds
    
    ! using keyword arguments
    call dtime(timearray,sim_time)

    mseconds = int((sim_time - floor(sim_time))*1000.0)
    days     = int(floor(floor(sim_time)/(3600.0*24.0)))
    hours    = int(floor((floor(sim_time)-real(days*3600*24))/3600.0))
    minutes  = int(floor((floor(sim_time)-real(days*3600*24)-real(hours*3600))/60.0))
    seconds  = int(floor(sim_time)) - days*(3600*24) - hours*3600 - minutes*60

    write (*,*) ' '
    write (*,*) ' '
    write (*,*) ' - '
    write (*,*) ' '
    write (*,*) ' '
    write (*,'(A,I5,A,I2,A,I2,A,I2,A,I3,A)') 'ellapsed simulation time = ',days,' days   ',hours,' hours   ',minutes,' min   ',seconds,' s   ', mseconds,' ms'

end subroutine ellapsed_time