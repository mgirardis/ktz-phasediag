program main
    use precision
    use Input
    use Output
    use Simulation
    use IFPORT
    !use Chaos

    implicit none
    !external EscreveArquivo
    character(len=80), allocatable :: args(:)
    integer(kind=4) :: nArgs, i, ios
    real(kr8), allocatable :: isiData(:), parAData(:), parBData(:), intData(:), llisiperData(:)
    character(len=1024) :: headerSaida
    character(len=30)   :: lastOutputName
    real(8) :: sim_time
    real, dimension(2) :: timearray
    !real(kr8) :: lambda(3)
    !real(kr8), dimension(3,3) :: A,L,U,B
    !real(kr8), allocatable :: x(:), isi(:)
    !real(kr8) :: pi
    !integer :: t, n
    
    sim_time = dclock()

    !n = 1000
    !allocate(x(1:n))
    !pi = dacos(-1.0D0)
    !x(1:n) = (/ (dcos( dble(t)*2.0D0*pi/100.0D0 ) , t=1,n) /)
    !call findISI(x, isi, 0.0D0)
    !write(*,*) 'x(1:10)='
    !write(*,*) x(1:10)
    !write(*,*) ' '
    !write(*,*) ' '
    !write(*,*) ' '
    !write(*,*) 'isi='
    !write(*,*) isi
    !call exit(1)

    ! (row,col)
    !A(1,1) = 10.0
    !A(2,1) = -3.0
    !A(3,1) =  5.0
    !A(1,2) = -7.0
    !A(2,2) =  2.0
    !A(3,2) = -1.0
    !A(1,3) =  0.0
    !A(2,3) =  6.0
    !A(3,3) =  5.0
!
    !B(1,:) = (/ 1,2,3 /)
    !B(2,:) = (/ 4,5,6 /)
    !B(3,:) = (/ 7,8,9 /)
    !write(*,*) transpose(B)
!
    !call LU_decomp(A,L,U)
    !
    !write(*,*) 'A='
    !write(*,*) transpose(A)
    !write(*,*) ' '
    !write(*,*) ' '
    !write(*,*) ' '
    !write(*,*) 'L*U='
    !write(*,*) transpose(matmul(L,U))
    !write(*,*) ' '
    !write(*,*) ' '
    !write(*,*) ' '
    !write(*,*) 'L='
    !write(*,*) transpose(L)
    !write(*,*) ' '
    !write(*,*) ' '
    !write(*,*) ' '
    !write(*,*) 'U='
    !write(*,*) transpose(U)
    !write(*,*) ' '
    !write(*,*) ' '
    !write(*,*) ' '
    !write(*,*) 'diagonal of A'
    !write(*,*) diag(A)
    !
    !call exit(1)
    
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

    
    !par%K      = 0.6D0
    !par%T      = 0.1D0
    !par%d      = 0.001D0
    !par%l      = 0.001D0
    !par%xR     = -0.414335233523352D0
    !par%H      = 0.0D0
    !par%x0     = (/ 1.0D0, 1.0D0, 1.0D0 /)
    !par%tTotal = 10000000
    !
    !write(*,'(E23.16)') par%xR
    !
    !call CalcLyapunovExp(lambda)
    !write(*,*) lambda
    !call exit(1)

    write (*,*) 'Simulando ...'
    call Simula(parBData, parAData, isiData, intData, llisiperData)

    if (.not.par%writeOnRun) then
        if (trim(par%measure) == "ISI") then
            lastOutputName = "ISIPeriod"
        else
            lastOutputName = "LyapExp"
        end if
        headerSaida = trim(pegaStrParamEntrada())// &
                      "# "//trim(par%parB)//"     "//trim(par%parA)//&
                      "     "//trim(par%measure)//"     intensity     "//&
                      trim(lastOutputName)
        call EscreveArquivo(parBData, &
                            parAData, &
                            isiData, &
                            intData, &
                            llisiperData, &
                            "(4D20.12)", &
                            headerSaida, &
                            pegaNomeArqSaida())
    end if

    call ellapsed_time(sim_time)

end program main

subroutine ellapsed_time(start_time)
    use IFPORT
    real(8) :: sim_time,start_time
    real, dimension(2) :: timearray
    integer :: days,hours,minutes,seconds,mseconds
    
    ! using keyword arguments
    !call dtime(timearray,sim_time)
    sim_time = dclock() - start_time
    !print *, 'total time = ', sim_time

    mseconds = int((sim_time - floor(sim_time))*1000.0)
    days     = int(floor(floor(sim_time)/(3600.0*24.0)))
    hours    = int(floor((floor(sim_time)-real(days*3600*24))/3600.0))
    minutes  = int(floor((floor(sim_time)-real(days*3600*24)-real(hours*3600))/60.0))
    seconds  = int(floor(sim_time)) - days*(3600*24) - hours*3600 - minutes*60

    write (*,*) ' - '
    write (*,*) 'ellapsed simulation time = ',days,' days   ',hours,' hours   ',minutes,' min   ',seconds,' s   ', mseconds,' ms'

end subroutine ellapsed_time
