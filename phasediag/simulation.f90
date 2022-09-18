module Simulation

    use Input
    use Output
    use Chaos
    use precision
    
    type KTzParam
        real(kr8) :: K, T, d, l, xR, H, Z
    end type KTzParam

    abstract interface
        function KTzIterator(neuPar, xAnt)
            import KTzParam, kr8
            real(kr8) :: xAnt(3)
            type(KTzParam) :: neuPar
            real(kr8), dimension(3) :: KTzIterator
        end function KTzIterator

        function KTzJacobMatrix(neuPar, x)
            import KTzParam, kr8
            real(kr8)      :: x(3)
            type(KTzParam) :: neuPar
            real(kr8), dimension(3,3) :: KTzJacobMatrix
        end function KTzJacobMatrix
    end interface

    !abstract interface
    !    function KTzJacobMatrix(neuPar, x)
    !        import KTzParam, kr8
    !        real(kr8)      :: x(3)
    !        type(KTzParam) :: neuPar
    !        real(kr8), dimension(3,3) :: KTzJacobMatrix
    !    end function KTzJacobMatrix
    !end interface

    private :: SimulaPara_xR_T_ISI, SimulaPara_xR_T_AMP, KTzLogIter, KTzTanhIter, K2TzIter, KTzIterator
    private :: KTzLogJacob, KTzTanhJacob, K2TzJacob, KTzJacobMatrix, findFirstLoc, findTwoConsecLoc, CalcISIPeriod
    private :: unique, findISI, logisticFunc, GetKTzParams, GetKTzParamValue, GetInputKTzParams
    public :: Simula, CalcLyapunovExp
contains

    subroutine CalcLyapunovExp(lambda)
        use Input
        implicit none
        real(kr8), intent(inout) :: lambda(:)
        real(kr8) :: x(3), D(3,3), Tr(3,3), J(3,3)
        integer   :: t
        type(KTzParam) :: neuPar
        procedure(KTzIterator),    pointer :: KTzFunc
        procedure(KTzJacobMatrix), pointer :: KTzJac

        if (par%model == "L") then
            KTzFunc => KTzLogIter
            KTzJac  => KTzLogJacob
        else if (par%model == "T") then
            KTzFunc => KTzTanhIter
            KTzJac  => KTzTanhJacob
        else if (par%model == "2") then
            KTzFunc => K2TzIter
            KTzJac  => K2TzJacob
        else
            write (*,*) 'ERROR! Unrecognized model'
            stop
        end if
        
        neuPar    = GetInputKTzParams()
        lambda(:) = 0.0D0
        x         = par%x0
        D         = identitymat(3)
        Tr(:,:)   = 0.0D0
        
        do t = 1, par%tTotal
            x = KTzFunc(neuPar, x)
            J = KTzJac(neuPar,x)
            call LyapExpEckmannRuelleIter(J,D,Tr,lambda)
        end do
        
        lambda = lambda / par%tTotal

    end subroutine CalcLyapunovExp

    subroutine Simula(parBData, parAData, isiData, intData, llisiperData)
        use Input
        use Output
        implicit none
        !external AbreArquivo
        real(kr8) :: dparB, dparA, parA, parB
        real(kr8), allocatable, intent(inout) :: parBData(:), parAData(:),&
                                                 isiData(:), intData(:)
        real(kr8), allocatable :: isi(:), parBDataTemp(:), parADataTemp(:)
        real(kr8), allocatable :: ll_or_isiper(:), llisiperData(:), llisiperDataTemp(:)
        real(kr8), allocatable :: isiDataTemp(:), intDataTemp(:), intensity(:)
        character(len=512) :: nomeArqSaida
        character(len=30)  :: lastOutputName
        integer :: i, j, k, m, n, u
        logical :: isISI
        type(KTzParam) :: neuPar
        procedure(KTzIterator),    pointer :: KTzFunc
        procedure(KTzJacobMatrix), pointer :: KTzJac

        isISI = .false.
        if (trim(par%measure) == "ISI") then
            isISI = .true.
        end if

        if (isISI) then
            lastOutputName = "ISIPeriod"
        else
            lastOutputName = "LyapExp"
        end if
        
        if (par%model == "L") then
            KTzFunc => KTzLogIter
            KTzJac  => KTzLogJacob
        else if (par%model == "T") then
            KTzFunc => KTzTanhIter
            KTzJac  => KTzTanhJacob
        else if (par%model == "2") then
            KTzFunc => K2TzIter
            KTzJac  => K2TzJacob
        else
            write (*,*) 'ERROR! Unrecognized model'
            stop
        end if

        dparA = 0.0D0
        dparB = 0.0D0
        if (par%nparA > 1) then
            dparA = (par%parA2 - par%parA1) / dble(par%nparA - 1)
        end if
        if (par%nparB > 1) then
            dparB = (par%parB2 - par%parB1) / dble(par%nparB - 1)
        end if


        if (par%writeOnRun) then
            nomeArqSaida = trim(pegaNomeArqSaida())
            u = AbreArquivo("replace", &
                            "write", &
                            trim(trim(pegaStrParamEntrada())// &
                                "# "//trim(par%parB)//"     "//trim(par%parA)//&
                                "     "//trim(par%measure)//&
                                "     intensity     "//trim(lastOutputName)),nomeArqSaida)
            do i = 1, par%nparB
                parB = par%parB1 + dparB * dble(i-1)
                write (*,*) 'Simulando para '//trim(par%parB)//'=',parB
                do j = 1, par%nparA
                    parA = par%parA1 + dparA * dble(j-1)
                    !write (*,*) 'Simulando para '//trim(par%parB)//'=',parB,'; '//trim(par%parA)//'=', parA
                    neuPar = GetKTzParams(parA,parB)
                    if (isISI) then
                        call SimulaPara_xR_T_ISI(neuPar, KTzFunc, isi, intensity, ll_or_isiper)
                    else
                        call SimulaPara_xR_T_AMP(neuPar, KTzFunc, KTzJac, isi, intensity, ll_or_isiper)
                    end if
                    n = size(isi, 1)
                    do k = 1, n
                        write (u, "(5D20.12)") parB, parA, isi(k), intensity(k), ll_or_isiper(k)
                    end do
                end do
                write (u,*)
                write (u,*)
            end do
            close(unit=u)
            write (*,'(A,A)') 'Arquivo escrito: ', trim(nomeArqSaida)
        else
            m = 10 * par%nparB * par%nparA ! assumindo que ha em torno de, no max, 3 ISI simult por par xR,T
            allocate(parBDataTemp(1:m))
            allocate(parADataTemp(1:m))
            allocate(isiDataTemp(1:m))
            allocate(intDataTemp(1:m))
            allocate(llisiperDataTemp(1:m))
            m = 0
            do i = 1, par%nparB
                parB = par%parB1 + dparB * dble(i-1)
                write (*,*) 'Simulando para '//trim(par%parB)//'=',parB
                do j = 1, par%nparA
                    parA = par%parA1 + dparA * dble(j-1)
                    !write (*,*) 'Simulando para '//trim(par%parB)//'=',parB,'; '//trim(par%parA)//'=', parA
                    neuPar = GetKTzParams(parA,parB)
                    if (isISI) then
                        call SimulaPara_xR_T_ISI(neuPar, KTzFunc, isi, intensity, ll_or_isiper)
                    else
                        call SimulaPara_xR_T_AMP(neuPar, KTzFunc, KTzJac, isi, intensity, ll_or_isiper)
                    end if
                    n = size(isi, 1)
                    do k = 1, n
                        m = m + 1
                        parBDataTemp(m)     = parB
                        parADataTemp(m)     = parA
                        isiDataTemp(m)      = isi(k)
                        intDataTemp(m)      = intensity(k)
                        llisiperDataTemp(m) = ll_or_isiper(k)
                    end do
                end do
            end do

            allocate(parBData(1:m))
            parBData     = parBDataTemp(1:m)
            deallocate(parBDataTemp)
            allocate(parAData(1:m))
            parAData     = parADataTemp(1:m)
            deallocate(parADataTemp)
            allocate(isiData(1:m))
            isiData      = isiDataTemp(1:m)
            deallocate(isiDataTemp)
            allocate(intData(1:m))
            intData      = intDataTemp(1:m)
            deallocate(intDataTemp)
            allocate(llisiperData(1:m))
            llisiperData = llisiperDataTemp(1:m)
            deallocate(llisiperDataTemp)
        end if
    end subroutine Simula
    
    function GetInputKTzParams() result (neuPar)
        use Input
        implicit none
        type(KTzParam) :: neuPar
        neuPar%K  = par%K
        neuPar%T  = par%T
        neuPar%d  = par%d
        neuPar%l  = par%l
        neuPar%xR = par%xR
        neuPar%H  = par%H
        neuPar%Z  = par%Z
    end function GetInputKTzParams

    function GetKTzParams(pA,pB) result (neuPar)
        use Input
        implicit none
        real(kr8) :: pA, pB
        type(KTzParam) :: neuPar
        neuPar%K  = GetKTzParamValue("K ",pA,pB)
        neuPar%T  = GetKTzParamValue("T ",pA,pB)
        neuPar%d  = GetKTzParamValue("d ",pA,pB)
        neuPar%l  = GetKTzParamValue("l ",pA,pB)
        neuPar%xR = GetKTzParamValue("xR",pA,pB)
        neuPar%H  = GetKTzParamValue("H ",pA,pB)
        neuPar%Z  = GetKTzParamValue("Z ",pA,pB)
    end function GetKTzParams
    
    function GetKTzParamValue(p,pA,pB) result (v)
        use Input
        implicit none
        character(len=2) :: p
        real(kr8) :: pA, pB, v
        
        if (trim(p) == par%parA) then
            v = pA
        else if (trim(p) == par%parB) then
            v = pB
        else
            if (trim(p) == "K") then
                v = par%K
            else if (trim(p) == "T") then
                v = par%T
            else if (trim(p) == "d") then
                v = par%d
            else if (trim(p) == "l") then
                v = par%l
            else if (trim(p) == "xR") then
                v = par%xR
            else if (trim(p) == "H") then
                v = par%H
            else
                v = par%Z
            end if
        end if
    end function GetKTzParamValue

    subroutine SimulaPara_xR_T_ISI(neuPar, KTzFunc, isi, intensity, isiper)
        use Input
        implicit none
        real(kr8), allocatable, intent(inout) :: isi(:), intensity(:), isiper(:)
        real(kr8) :: x(3) ! x(1) = x, x(2) = y, x(3) = z
        !real(kr8), intent(in) :: K, T, d, l, xR, H
        real(kr8), allocatable :: isiData(:)
        real(kr8) :: t1, t2, ts, tsA, xAnt
        integer :: tt, tEff
        integer :: i, k, iplus, crossCounter
        logical :: isFP
        type(KTzParam), intent(in) :: neuPar
        procedure(KTzIterator),    pointer, intent(in) :: KTzFunc

        isFP = .false.

        tEff = par%tTotal - par%tTransient
        
        if (.not.allocated(isiData)) then
            allocate(isiData(1:tEff)) ! alocando tEff valores para os isi
        end if
        
        !neuPar%K = K
        !neuPar%T = T
        !neuPar%d = d
        !neuPar%l = l
        !neuPar%xR = xR
        !neuPar%H = H

        x = par%x0
        do tt = 1,par%tTransient
            xAnt = x(1)
            x = KTzFunc(neuPar, x)
            if (dabs(x(1) - xAnt) <= 1.0e-8) then
                isFP = .true.
                exit
            end if
        end do
        if (.not.isFP) then
            do while (x(1) > par%xThreshold) ! garantindo que o loop principal começara com x(i)<0
                xAnt = x(1);
                x = KTzFunc(neuPar, x)
                if (dabs(x(1) - xAnt) <= 1.0e-8) then
                    isFP = .true.
                    exit
                end if
            end do
        end if
        t1 = 0
        t2 = 0
        ts = 0
        tsA = 0
        crossCounter = 0
        xAnt = x(1)
        k = 0
        if (.not.isFP) then
            do tt = 2,tEff ! loop principal
                x = KTzFunc(neuPar, x)
                if (dabs(x(1) - xAnt) <= 1.0e-8) then
                    isFP = .true.
                    exit
                end if
                if ((xAnt - par%xThreshold) * (x(1) - par%xThreshold) < 0.0D0) then ! cruzou o eixo x = xThreshold
                    if (modulo(crossCounter,2) == 0) then ! como começou abaixo de x = 0, crossCounter par significa subindo
                        t1 = (dble(tt - 1) + dble(tt)) / 2.0D0
                    else ! crossCounter impar significa descendo
                        t2 = (dble(tt - 1) + dble(tt)) / 2.0D0
                        ts = (t1 + t2) / 2.0D0 ! o instante do disparo eh a media entre tempo subida (t1) e descida (t2)
                        k = k + 1 ! k = qtd de ISI achados
                        isiData(k) = floor(ts - tsA)
                        tsA = ts
                    end if
                    crossCounter = crossCounter + 1
                end if
                xAnt = x(1)
            end do
        end if
        
        if (allocated(isi)) then
            deallocate(isi, intensity, isiper)
        end if
        if (isFP.or.(k == 0).or.(k == 1)) then ! nenhum ISI encontrado
            allocate(isi(1:1), intensity(1:1), isiper(1:1))
            isi(1)       = 0.0D0
            intensity(1) = 1.0D0
            isiper(1)    = 0.0D0
        else
            call unique(isiData(2:k), isi, intensity)
            call CalcISIPeriod(isiData(2:k), isi, isiper)
        end if
    end subroutine  SimulaPara_xR_T_ISI

    subroutine SimulaPara_xR_T_AMP(neuPar, KTzFunc, KTzJac, amp, intensity, lambda_lyapm)
        use Input
        implicit none
        real(kr8), allocatable, intent(inout) :: amp(:), intensity(:), lambda_lyapm(:)
        real(kr8) :: x(3) ! x(1) = x, x(2) = y, x(3) = z
        !real(kr8), intent(in) :: K, T, d, l, xR, H
        real(kr8) :: D_lyap(3,3), Tr_lyap(3,3), lambda_lyap(3)
        real(kr8) :: xMin, xMax
        integer :: tt, tEff
        type(KTzParam), intent(in) :: neuPar
        procedure(KTzIterator),    pointer, intent(in) :: KTzFunc
        procedure(KTzJacobMatrix), pointer, intent(in) :: KTzJac

        if (.not.allocated(amp)) then
            allocate(amp(1:1), intensity(1:1), lambda_lyapm(1:1))
        end if
        
        intensity = 0.0D0
        
        tEff = par%tTotal - par%tTransient
        
        !neuPar%K = K
        !neuPar%T = T
        !neuPar%d = d
        !neuPar%l = l
        !neuPar%xR = xR
        !neuPar%H = H

        ! initial conditions for the Lyapunov Exponent calculation
        lambda_lyap(:) = 0.0D0
        D_lyap         = identitymat(3)
        Tr_lyap(:,:)   = 0.0D0

        ! map initial condition
        x = par%x0

        do tt = 1,par%tTransient
            x = KTzFunc(neuPar, x)
            call LyapExpEckmannRuelleIter(KTzJac(neuPar,x),D_lyap,Tr_lyap,lambda_lyap)
        end do
        xMax = - huge(xMax)
        xMin = huge(xMin)
        do tt = 1,tEff ! loop principal
            x = KTzFunc(neuPar, x)
            if (x(1) < xMin) then
                xMin = x(1)
            end if
            if (x(1) > xMax) then
                xMax = x(1)
            end if
            call LyapExpEckmannRuelleIter(KTzJac(neuPar,x),D_lyap,Tr_lyap,lambda_lyap)
        end do
        amp          = xMax - xMin
        lambda_lyapm = maxval(lambda_lyap / par%tTotal)
    end subroutine  SimulaPara_xR_T_AMP

    ! intensity eh a qtd de vezes que um valor de x se repete, em relacao ao total de valores de x
    ! tal que sum(intensity) = 1
    subroutine unique(x, xUn, intensity)
        implicit none
        real(kr8), intent(in) :: x(:)
        real(kr8), allocatable, intent(inout) :: xUn(:), intensity(:)
        real(kr8) :: res(size(x,1))
        real(kr8) :: intTemp(size(x,1))
        integer :: k                   ! The number of unique elements
        integer :: i, j

        intTemp = 1.0D0

        k = 1
        res(1) = x(1)
        outer: do i=2,size(x)
            do j=1,k
                if (res(j) == x(i)) then
                    ! Found a match so start looking again
                    intTemp(j) = intTemp(j) + 1.0D0
                    cycle outer
                end if
            end do
            ! No match found so add it to the output
            k = k + 1
            res(k) = x(i)
        end do outer

        allocate(xUn(1:k), intensity(1:k))
        xUn = res(1:k)
        intensity = intTemp(1:k) / dble(size(x,1))
    end subroutine unique

    subroutine CalcISIPeriod(isi_all, isi_un, isiper)
        implicit none
        real(kr8), intent(in)                 :: isi_all(:), isi_un(:)
        real(kr8), allocatable, intent(inout) :: isiper(:)
        real(kr8) :: I
        integer   :: k, n, n_un, s(2) !, s1, s2
        n_un = size(isi_un)
        if (.not.allocated(isiper)) then
            allocate(isiper(1:n_un))
        end if
        n = size(isi_all)
        do k = 1,n_un
            s = findTwoConsecLoc(isi_all,isi_un(k))
            isiper(k) = s(2) - s(1)
            if ( (s(1) == -1) .or. (s(2) == -1) ) then
                isiper(k) = 1.0/0.0
            end if
            !write(*,*) isiper(k)
            !s1 = findFirstLoc(isi_all,isi_un(k))
            !if (s1 == n) then
            !    isiper(k) = -99999999
            !else
            !    s2 = findFirstLoc(isi_all((s1+1):n),isi_un(k))
            !    if (s2 == -1) then
            !        isiper(k) = -99999999
            !    else
            !        write(*,*) 's1=',s1,'   s2=',s2
            !        isiper(k) = s2 - s1
            !    end if
            !end if
        end do

    end subroutine CalcISIPeriod
    
    function findTwoConsecLoc(X,value) result(k)
        implicit none
        real(kr8) :: X(:)
        real(kr8) :: value
        integer   :: k(2), n, m, i
        n = size(X)
        m = 0
        do i = 1,n
            if (X(i) == value) then
                m = m + 1
                k(m) = i
                if (m == 2) then
                    return
                end if
            end if
        end do
        if (m == 0) then
            k = (/ -1, -1 /) ! not found
        else if (m == 1) then
            k(2) = -1
        end if
    end function findTwoConsecLoc

    function findFirstLoc(X,value) result(k)
        implicit none
        real(kr8) :: X(:)
        real(kr8) :: value
        integer :: k, n
        n = size(X)
        do k = 1,n
            if (X(k) == value) then
                return
            end if
        end do
        k = -1 ! not found
    end function findFirstLoc

    ! o algoritmo abaixo esta incorporado na rotina
    ! SimulaPara_xR_T e, portanto, nao esta sendo usado
    ! neste programa por enquanto
    subroutine findISI(x, isi)
        real*8, intent(in) :: x(:)
        real*8, allocatable, intent(out) :: isi(:)
        real*8, allocatable :: isiData(:)
        integer :: i, k, iplus, crossCounter
        real*8 :: t1, t2, ts, tsA

        t1 = 0
        t2 = 0
        ts = 0
        tsA = 0
        crossCounter = 0
        n = size(x, 1) - 1
        allocate(isiData(1:n)) ! alocando n valores para os isi
        i = 1
        do while (x(i) > 0.0D0) ! garantindo que o loop principal começara com x(i)<0
            i = i + 1
        end do
        k = 1
        do while (i <= n) ! loop principal
            iplus = i + 1
            if (x(i) * x(iplus) < 0) then ! cruzou o eixo x = 0
                if (modulo(crossCounter,2) == 0) then ! como começou abaixo de x = 0, crossCounter par significa subindo
                    t1 = (dble(i) + dble(iplus)) / 2.0D0
                else ! crossCounter impar significa descendo
                    t2 = (dble(i) + dble(iplus)) / 2.0D0
                    ts = (t1 + t2) / 2.0D0
                    isiData(k) = ts - tsA
                    tsA = ts
                    k = k + 1
                end if
                crossCounter = crossCounter + 1
            end if
            i = i + 1
        end do
        k = k - 1 ! k = total de ISI encontrados
        allocate(isi(1:(k-1)))
        isi = isiData(2:k)
    end subroutine findISI

    function KTzTanhIter(neuPar, xAnt) result (x)
        implicit none
        real(kr8) :: x(3), xAnt(3)
        type(KTzParam) :: neuPar
        x(2) = xAnt(1)
        x(3) = (1.0D0 - neuPar%d) * xAnt(3) - neuPar%l * (xAnt(1) - neuPar%xR)
        x(1) = dtanh((xAnt(1) - neuPar%K * xAnt(2) + xAnt(3) + neuPar%H) / neuPar%T)
    end function KTzTanhIter
    
    function KTzLogIter(neuPar, xAnt) result (x)
        implicit none
        real(kr8) :: x(3), xAnt(3)
        type(KTzParam) :: neuPar
        x(2) = xAnt(1)
        x(3) = (1.0D0 - neuPar%d) * xAnt(3) - neuPar%l * (xAnt(1) - neuPar%xR)
        x(1) = logisticFunc((xAnt(1) - neuPar%K * xAnt(2) + xAnt(3) + neuPar%H) / neuPar%T)
    end function KTzLogIter

    function K2TzIter(neuPar, xAnt) result (x)
        implicit none
        real(kr8) :: x(3), xAnt(3)
        type(KTzParam) :: neuPar
        x(2) = dtanh( (xAnt(1) + neuPar%H) / neuPar%T )
        x(3) = (1.0D0 - neuPar%d) * xAnt(3) - neuPar%l * (xAnt(1) - neuPar%xR)
        x(1) = dtanh((xAnt(1) - neuPar%K * xAnt(2) + xAnt(3) + neuPar%Z) / neuPar%T)
    end function K2TzIter

    function KTzLogJacob(neuPar, x) result(J)
        implicit none
        real(kr8) :: x(3), a, J1, J(3,3)
        type(KTzParam) :: neuPar
        a = dabs(x(1)-neuPar%K*x(2)+x(3)) !KTzLog
        J1 = neuPar%T / ((neuPar%T+a)*(neuPar%T+a))
        J(1,:) = (/ J1, -neuPar%K*J1, J1 /)
        J(2,:) = (/ 1.0D0, 0.0D0, 0.0D0 /)
        J(3,:) = (/ -neuPar%l, 0.0D0, (1.0D0-neuPar%d) /)
    end function KTzLogJacob

    function KTzTanhJacob(neuPar, x) result(J)
        implicit none
        real(kr8) :: x(3), a, J(3,3)
        type(KTzParam) :: neuPar
        a = (dsech(   (x(1)-neuPar%K*x(2)+x(3)+neuPar%H)/neuPar%T )**2.0D0)/neuPar%T !KTzLog
        J(1,:) = (/ a, -neuPar%K*a, a /)
        J(2,:) = (/ 1.0D0, 0.0D0, 0.0D0 /)
        J(3,:) = (/ -neuPar%l, 0.0D0, (1.0D0-neuPar%d) /)
    end function KTzTanhJacob

    function K2TzJacob(neuPar, x) result(J)
        implicit none
        real(kr8) :: x(3), a, b, J(3,3)
        type(KTzParam) :: neuPar
        a = (dsech(   (x(1)-neuPar%K*x(2)+x(3)+neuPar%Z)/neuPar%T  )**2.0D0)/neuPar%T !KTzLog
        b = (dsech(  (neuPar%H + x(1))/neuPar%T  )**2.0D0)/neuPar%T
        J(1,:) = (/ a, -neuPar%K*a, a /)
        J(2,:) = (/ 1.0D0, 0.0D0, 0.0D0 /)
        J(3,:) = (/ -neuPar%l, 0.0D0, (1.0D0-neuPar%d) /)
    end function K2TzJacob

    function logisticFunc(x) result(y)
        implicit none
        real(kr8) :: x, y
        y = x / (1.0D0 + dabs(x))
    end function logisticFunc

    function dsech(x) result(y)
        implicit none
        real(kr8) :: x,y
        y = 2.0D0 / (dexp(x)+dexp(-x))
    end function dsech

end module Simulation 
