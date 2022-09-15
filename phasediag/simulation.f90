module Simulation

    use Input
    use Output
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
    end interface

    private :: SimulaPara_xR_T, KTzLogIter, KTzTanhIter, K2TzIter, KTzIterator
    private :: unique, findISI, logisticFunc, GetKTzParams, GetKTzParamValue
    public :: Simula
contains

    subroutine Simula(parBData, parAData, isiData, intData)
        use Input
        use Output
        implicit none
        !external AbreArquivo
        real(kr8) :: dparB, dparA, parA, parB
        real(kr8), allocatable, intent(inout) :: parBData(:), parAData(:),&
                                                 isiData(:), intData(:)
        real(kr8), allocatable :: isi(:), parBDataTemp(:), parADataTemp(:)
        real(kr8), allocatable :: isiDataTemp(:), intDataTemp(:), intensity(:)
        character(len=512) :: nomeArqSaida
        integer :: i, j, k, m, n, u
        logical :: isISI
        type(KTzParam) :: neuPar
        procedure(KTzIterator), pointer :: KTzFunc

        isISI = .false.
        if (trim(par%measure) == "ISI") then
            isISI = .true.
        end if
        
        if (par%model == "L") then
            KTzFunc => KTzLogIter
        else if (par%model == "T") then
            KTzFunc => KTzTanhIter
        else if (par%model == "2") then
            KTzFunc => K2TzIter
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
                                "     intensity"),nomeArqSaida)
            do i = 1, par%nparB
                parB = par%parB1 + dparB * dble(i-1)
                write (*,*) 'Simulando para '//trim(par%parB)//'=',parB
                do j = 1, par%nparA
                    parA = par%parA1 + dparA * dble(j-1)
                    !write (*,*) 'Simulando para '//trim(par%parB)//'=',parB,'; '//trim(par%parA)//'=', parA
                    neuPar = GetKTzParams(parA,parB)
                    if (isISI) then
                        call SimulaPara_xR_T_ISI(neuPar, KTzFunc, isi, intensity)
                    else
                        call SimulaPara_xR_T_AMP(neuPar, KTzFunc, isi, intensity)
                    end if
                    n = size(isi, 1)
                    do k = 1, n
                        write (u, "(4D17.8)") parB, parA, isi(k), intensity(k)
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

            m = 0
            do i = 1, par%nparB
                parB = par%parB1 + dparB * dble(i-1)
                write (*,*) 'Simulando para '//trim(par%parB)//'=',parB
                do j = 1, par%nparA
                    parA = par%parA1 + dparA * dble(j-1)
                    !write (*,*) 'Simulando para '//trim(par%parB)//'=',parB,'; '//trim(par%parA)//'=', parA
                    neuPar = GetKTzParams(parA,parB)
                    if (isISI) then
                        call SimulaPara_xR_T_ISI(neuPar, KTzFunc, isi, intensity)
                    else
                        call SimulaPara_xR_T_AMP(neuPar, KTzFunc, isi, intensity)
                    end if
                    n = size(isi, 1)
                    do k = 1, n
                        m = m + 1
                        parBDataTemp(m) = parB
                        parADataTemp(m) = parA
                        isiDataTemp(m) = isi(k)
                        intDataTemp(m) = intensity(k)
                    end do
                end do
            end do

            allocate(parBData(1:m), parAData(1:m), isiData(1:m), intData(1:m))
            parBData = parBDataTemp(1:m)
            parAData = parADataTemp(1:m)
            isiData = isiDataTemp(1:m)
            intData = intDataTemp(1:m)
        end if
    end subroutine Simula
    
    function GetKTzParams(pA,pB) result (neuPar)
        use Input
        implicit none
        real(kr8) :: pA, pB
        type(KTzParam) :: neuPar
        neuPar%K = GetKTzParamValue("K ",pA,pB)
        neuPar%T = GetKTzParamValue("T ",pA,pB)
        neuPar%d = GetKTzParamValue("d ",pA,pB)
        neuPar%l = GetKTzParamValue("l ",pA,pB)
        neuPar%xR = GetKTzParamValue("xR",pA,pB)
        neuPar%H = GetKTzParamValue("H ",pA,pB)
        neuPar%Z = GetKTzParamValue("Z ",pA,pB)
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

    subroutine SimulaPara_xR_T_ISI(neuPar, KTzFunc, isi, intensity)
        use Input
        implicit none
        real(kr8), allocatable, intent(inout) :: isi(:), intensity(:)
        real(kr8) :: x(3) ! x(1) = x, x(2) = y, x(3) = z
        !real(kr8), intent(in) :: K, T, d, l, xR, H
        real(kr8), allocatable :: isiData(:)
        real(kr8) :: t1, t2, ts, tsA, xAnt
        integer :: tt, tEff
        integer :: i, k, iplus, crossCounter
        logical :: isFP
        type(KTzParam), intent(in) :: neuPar
        procedure(KTzIterator), pointer, intent(in) :: KTzFunc

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
                        isiData(k) = ts - tsA
                        tsA = ts
                    end if
                    crossCounter = crossCounter + 1
                end if
                xAnt = x(1)
            end do
        end if
        
        if (allocated(isi)) then
            deallocate(isi, intensity)
        end if
        if (isFP.or.(k == 0).or.(k == 1)) then ! nenhum ISI encontrado
            allocate(isi(1:1), intensity(1:1))
            isi(1) = 0.0D0
            intensity(1) = 1.0D0
        else
            call unique(isiData(2:k), isi, intensity)
        end if
    end subroutine  SimulaPara_xR_T_ISI

    subroutine SimulaPara_xR_T_AMP(neuPar, KTzFunc, amp, intensity)
        use Input
        implicit none
        real(kr8), allocatable, intent(inout) :: amp(:), intensity(:)
        real(kr8) :: x(3) ! x(1) = x, x(2) = y, x(3) = z
        !real(kr8), intent(in) :: K, T, d, l, xR, H
        real(kr8) :: xMin, xMax
        integer :: tt, tEff
        type(KTzParam), intent(in) :: neuPar
        procedure(KTzIterator), pointer, intent(in) :: KTzFunc

        if (.not.allocated(amp)) then
            allocate(amp(1:1), intensity(1:1))
        end if
        
        intensity = 0.0D0
        
        tEff = par%tTotal - par%tTransient
        
        !neuPar%K = K
        !neuPar%T = T
        !neuPar%d = d
        !neuPar%l = l
        !neuPar%xR = xR
        !neuPar%H = H

        x = par%x0
        do tt = 1,par%tTransient
            x = KTzFunc(neuPar, x)
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
        end do
        amp = xMax - xMin
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
        k = k - 1 ! k = total de ISI encontraldos
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

    function logisticFunc(x) result(y)
        implicit none
        real(kr8) :: x, y
        y = x / (1.0D0 + dabs(x))
    end function logisticFunc

end module Simulation 
