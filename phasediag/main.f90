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
    real(8) :: sim_time
    real, dimension(2) :: timearray
    logical :: isISI
    !real(kr8) :: lambda(3)
    !real(kr8), dimension(3,3) :: A,L,U,B
    !real(kr8), allocatable :: x1(:), x2(:), isi(:)
    !real(kr8) :: pi, x_period
    !integer :: t, n
    !real(kr8), allocatable :: x1(:,:)
    !integer :: x_period
    
    
    sim_time = dclock()
    
    !type(KTzParam) :: neuPar
    !real(kr8), allocatable :: x_values(:,:), x_only(:)
    !integer :: Q
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Debugging periodicity
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !neuPar = GetKTzParamStruct(1.0D0, 0.2D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0)
    !call SimulaMapa(neuPar, "L", (/ -0.5D0, -0.5D0, 0.0D0 /), 10000, 12000, x_values)
    !allocate(x_only(1:size(x_values,1)))
    !x_only = x_values(1:,1)
    !Q = findPeriod(x_only, 1.0D-8)
    !write(*,*) "Period of 1/6: Q=",Q
    !
    !deallocate(x_only,x_values)
    !write(*,*) "----------------"
    !
    !neuPar = GetKTzParamStruct(0.6D0, 0.35D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0)
    !call SimulaMapa(neuPar, "L", (/ -0.5D0, -0.5D0, 0.0D0 /), 10000, 20000, x_values)
    !allocate(x_only(1:size(x_values,1)))
    !x_only = x_values(1:,1)
    !Q = findPeriod(x_only, 1.0D-8)
    !write(*,*) "Period of K=0.6 T=0.35: Q=",Q
    !
    !deallocate(x_only,x_values)
    !write(*,*) "----------------"
    !
    !neuPar = GetKTzParamStruct(0.6D0, 0.234386400000D+00, 0.001D0, 0.001D0, -0.194405192616D+00, 0.0D0, 0.0D0)
    !call SimulaMapa(neuPar, "L", (/ -0.5D0, -0.5D0, 0.0D0 /), 10000, 20000, x_values)
    !allocate(x_only(1:size(x_values,1)))
    !x_only = x_values(1:,1)
    !Q = findPeriod(x_only, 1.0D-8)
    !write(*,*) "Period of weird result: Q=",Q
    !call exit(1)


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! Debugging periodicity
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !write (*,*) " ********** TESTING 0/1"
    !call calculate_and_find_period_test("0/1")
    !write (*,*) " ********** TESTING 1/4"
    !call calculate_and_find_period_test("1/4")
    !write (*,*) " ********** TESTING 1/6"
    !call calculate_and_find_period_test("1/6")
    !write (*,*) " ********** TESTING 1/8"
    !call calculate_and_find_period_test("1/8")
    !write (*,*) " ********** TESTING 2/14"
    !call calculate_and_find_period_test("2/14")
    !write (*,*) " ********** TESTING 3/16"
    !call calculate_and_find_period_test("3/16")
    !write (*,*) " ********** TESTING 2/10"
    !call calculate_and_find_period_test("2/10")
    !write (*,*) " ********** TESTING 3/14"
    !call calculate_and_find_period_test("3/14")
    !write (*,*) " ********** TESTING 4/18"
    !call calculate_and_find_period_test("4/18")
    !write (*,*) " ********** TESTING aper"
    !call calculate_and_find_period_test("aper")
    !call exit(1)
    

    !n = 1000
    !allocate(x1(1:n),x2(1:n))
    !pi = dacos(-1.0D0)
    !x1(1:n) = (/ (dcos( dble(t)*2.0D0*pi/100.0D0 ) , t=1,n) /)
    !call findISI(x1, isi, 0.0D0)
    !write(*,*) 'x(1:10)='
    !write(*,*) x1(1:10)
    !write(*,*) ' '
    !write(*,*) ' expected period = 100 '
    !write(*,*) ' '
    !write(*,*) 'isi='
    !write(*,*) isi
    !write(*,*) ' '
    !write(*,*) ' '
    !write(*,*) ' '
    !x2(1:n) = (/ (dcos( dble(t)*2.0D0*pi/20.0D0 )  , t=1,n) /)
    !x_period = findPeriod(x2,1D-5)
    !write(*,*) 'x(1:20)='
    !write(*,*) x2(1:10)
    !write(*,*) ' '
    !write(*,*) ' expected period = 20'
    !write(*,*) ' '
    !write(*,*) 'P='
    !write(*,*) x_period
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
        isISI = (trim(par%measure) == "ISI") ! makes the output break two lines if it is ISI
        call EscreveArquivo(parBData, &
                            parAData, &
                            isiData, &
                            intData, &
                            llisiperData, &
                            trim(getOutputFormatStr()), & !"(5D20.12)", &
                            trim(getOutFileHeader()), &
                            pegaNomeArqSaida(),&
                            isISI)
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
