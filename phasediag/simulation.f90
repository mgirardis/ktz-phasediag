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

    private :: SimulaPara_phaseDiag_ISI, SimulaPara_phaseDiag_AMP, KTzLogIter, KTzTanhIter, K2TzIter, KTzIterator
    private :: SimulaPara_phaseDiag_WIN, check_periodicity, GetJacobFunc, GetMapFunc
    private :: KTzLogJacob, KTzTanhJacob, K2TzJacob, KTzJacobMatrix, findFirstLoc, findTwoConsecLoc, CalcISIPeriod
    private :: unique, logisticFunc, UpdateInputKTzParams, UpdateInputKTzParamSpecific, GetInputKTzParams, logspace, linspace
    public  :: Simula, CalcLyapunovExp, findISI, findPeriod, calculate_and_find_period_test, SimulaMapa
    public  :: GetKTzParamStruct
contains

    subroutine GetMapFunc(model,KTzFunc)
        character(len=1)               , intent(in)   :: model
        procedure(KTzIterator), pointer, intent(out)  :: KTzFunc
        
        if (model == "L") then
            KTzFunc => KTzLogIter
        else if (model == "T") then
            KTzFunc => KTzTanhIter
        else if (model == "2") then
            KTzFunc => K2TzIter
        else
            write (*,*) 'ERROR! Unrecognized model'
            stop
        end if
    end subroutine GetMapFunc
    
    subroutine GetJacobFunc(model, KTzJac)
        character(len=1)                  , intent(in)  :: model
        procedure(KTzJacobMatrix), pointer, intent(out) :: KTzJac
        
        if (model == "L") then
            KTzJac  => KTzLogJacob
        else if (model == "T") then
            KTzJac  => KTzTanhJacob
        else if (model == "2") then
            KTzJac  => K2TzJacob
        else
            write (*,*) 'ERROR! Unrecognized model'
            stop
        end if
    end subroutine GetJacobFunc

    subroutine CalcLyapunovExp(lambda)
        use Input
        use Chaos
        implicit none
        real(kr8), intent(inout) :: lambda(:)
        real(kr8) :: x(3), D(3,3), Tr(3,3), J(3,3)
        integer   :: t
        type(KTzParam) :: neuPar
        procedure(KTzIterator),    pointer :: KTzFunc
        procedure(KTzJacobMatrix), pointer :: KTzJac

        call GetMapFunc(  par%model, KTzFunc)
        call GetJacobFunc(par%model, KTzJac )
        !if (par%model == "L") then
        !    KTzFunc => KTzLogIter
        !    KTzJac  => KTzLogJacob
        !else if (par%model == "T") then
        !    KTzFunc => KTzTanhIter
        !    KTzJac  => KTzTanhJacob
        !else if (par%model == "2") then
        !    KTzFunc => K2TzIter
        !    KTzJac  => K2TzJacob
        !else
        !    write (*,*) 'ERROR! Unrecognized model'
        !    stop
        !end if
        
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
        real(kr8), allocatable :: isi_or_amp_or_w(:), parBDataTemp(:), parADataTemp(:)
        real(kr8), allocatable :: ll_or_isiper_or_per(:), llisiperData(:), llisiperDataTemp(:)
        real(kr8), allocatable :: isiDataTemp(:), intDataTemp(:), intensity_or_cycles(:)
        real(kr8), allocatable :: parA_values(:), parB_values(:)
        character(len=512) :: nomeArqSaida
        character(len=30)  :: lastOutputName
        integer :: i, j, k, m, n, u
        logical :: isISI, isWIN, isAMP
        type(KTzParam) :: neuPar
        procedure(KTzIterator),    pointer :: KTzFunc
        procedure(KTzJacobMatrix), pointer :: KTzJac

        isISI = .false.
        isWIN = .false.
        isAMP = .false.
        if (trim(par%measure) == "ISI") then
            isISI = .true.
        else if (trim(par%measure) == "WIN") then
            isWIN = .true.
        else
            isAMP = .true.
        end if

        !if (isISI) then
        !    lastOutputName = "ISIPeriod"
        !else if (isAMP) then
        !    lastOutputName = "LyapExp"
        !else ! isWIN for winding number
        !    lastOutputName = "Period"
        !end if
        lastOutputName = trim(par%lastOutputColName)

        call GetMapFunc(  par%model, KTzFunc)
        call GetJacobFunc(par%model, KTzJac )
        
        !if (par%model == "L") then
        !    KTzFunc => KTzLogIter
        !    KTzJac  => KTzLogJacob
        !else if (par%model == "T") then
        !    KTzFunc => KTzTanhIter
        !    KTzJac  => KTzTanhJacob
        !else if (par%model == "2") then
        !    KTzFunc => K2TzIter
        !    KTzJac  => K2TzJacob
        !else
        !    write (*,*) 'ERROR! Unrecognized model'
        !    stop
        !end if

        !dparA = 0.0D0
        !dparB = 0.0D0
        !if (par%nparA > 1) then
        !    dparA = (par%parA2 - par%parA1) / dble(par%nparA - 1)
        !end if
        !if (par%nparB > 1) then
        !    dparB = (par%parB2 - par%parB1) / dble(par%nparB - 1)
        !end if

        allocate(parA_values(1:par%nparA),parB_values(1:par%nparB))

        if (par%parAScale == "LOG") then
            parA_values(1:par%nparA) = logspace(par%parA1,par%parA2,par%nparA)
        else
            parA_values(1:par%nparA) = linspace(par%parA1,par%parA2,par%nparA)
        endif

        if (par%parBScale == "LOG") then
            parB_values(1:par%nparB) = logspace(par%parB1,par%parB2,par%nparB)
        else
            parB_values(1:par%nparB) = linspace(par%parB1,par%parB2,par%nparB)
        endif



        if (par%writeOnRun) then
            nomeArqSaida = trim(pegaNomeArqSaida())
            u = AbreArquivo("replace","write",trim(getOutFileHeader()),nomeArqSaida)
            do i = 1, par%nparB
                parB = parB_values(i) !par%parB1 + dparB * dble(i-1)
                write (*,*) 'Simulando para '//trim(par%parB)//'=',parB,&
                            '['//trim(int_to_str(i,'I0'))//'/'//trim(int_to_str(par%nparB,'I0'))//'; '// &
                            trim(double_to_str(100.0D0*dble(i)/dble(par%nparB),'F6.1'))//'% ]'
                do j = 1, par%nparA
                    parA = parA_values(j) !parA = par%parA1 + dparA * dble(j-1)
                    !write (*,*) 'Simulando para '//trim(par%parB)//'=',parB,'; '//trim(par%parA)//'=', parA
                    neuPar = UpdateInputKTzParams(parA,parB)
                    if (isISI) then
                        call SimulaPara_phaseDiag_ISI(neuPar, KTzFunc, isi_or_amp_or_w, intensity_or_cycles, ll_or_isiper_or_per)
                    else if (isAMP) then
                        call SimulaPara_phaseDiag_AMP(neuPar, KTzFunc, KTzJac, isi_or_amp_or_w, intensity_or_cycles, ll_or_isiper_or_per)
                    else ! isWIN for winding number
                        call SimulaPara_phaseDiag_WIN(neuPar, KTzFunc, isi_or_amp_or_w, intensity_or_cycles, ll_or_isiper_or_per)
                    end if
                    n = size(isi_or_amp_or_w, 1)
                    do k = 1, n
                        !write (u, "(5D20.12)") parB, parA, isi_or_amp_or_w(k), intensity_or_cycles(k), ll_or_isiper_or_per(k)
                        write (u, trim(getOutputFormatStr())) parB, parA, isi_or_amp_or_w(k), intensity_or_cycles(k), ll_or_isiper_or_per(k)
                    end do
                end do
                if (isISI) then
                    write (u,*)
                    write (u,*)
                end if
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
                parB = parB_values(i) ! par%parB1 + dparB * dble(i-1)
                write (*,*) 'Simulando para '//trim(par%parB)//'=',parB,&
                            '['//trim(int_to_str(i,'I0'))//'/'//trim(int_to_str(par%nparB,'I0'))//'; '// &
                            trim(double_to_str(100.0D0*dble(i)/dble(par%nparB),'F6.1'))//'% ]'
                do j = 1, par%nparA
                    parA = parA_values(j) ! par%parA1 + dparA * dble(j-1)
                    !write (*,*) 'Simulando para '//trim(par%parB)//'=',parB,'; '//trim(par%parA)//'=', parA
                    neuPar = UpdateInputKTzParams(parA,parB)
                    if (isISI) then
                        call SimulaPara_phaseDiag_ISI(neuPar, KTzFunc, isi_or_amp_or_w, intensity_or_cycles, ll_or_isiper_or_per)
                    else if (isAMP) then
                        call SimulaPara_phaseDiag_AMP(neuPar, KTzFunc, KTzJac, isi_or_amp_or_w, intensity_or_cycles, ll_or_isiper_or_per)
                    else ! isWIN for winding number
                        call SimulaPara_phaseDiag_WIN(neuPar, KTzFunc, isi_or_amp_or_w, intensity_or_cycles, ll_or_isiper_or_per)
                    end if
                    n = size(isi_or_amp_or_w, 1)
                    do k = 1, n
                        m = m + 1
                        parBDataTemp(m)     = parB
                        parADataTemp(m)     = parA
                        isiDataTemp(m)      = isi_or_amp_or_w(k)
                        intDataTemp(m)      = intensity_or_cycles(k)
                        llisiperDataTemp(m) = ll_or_isiper_or_per(k)
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

    function GetKTzParamStruct(K,T,d,l,xR,H,Z) result(neuPar)
        implicit none
        real(kr8) :: K,T,d,l,xR,H,Z
        type(KTzParam) :: neuPar
        
        neuPar = GetInputKTzParams()
        neuPar%K  = K
        neuPar%T  = T
        neuPar%d  = d
        neuPar%l  = l
        neuPar%xR = xR
        neuPar%H  = H
        neuPar%Z  = Z
    end function GetKTzParamStruct

    function UpdateInputKTzParams(pA,pB) result (neuPar)
        use Input
        implicit none
        real(kr8) :: pA, pB
        type(KTzParam) :: neuPar
        neuPar%K  = UpdateInputKTzParamSpecific("K ",pA,pB)
        neuPar%T  = UpdateInputKTzParamSpecific("T ",pA,pB)
        neuPar%d  = UpdateInputKTzParamSpecific("d ",pA,pB)
        neuPar%l  = UpdateInputKTzParamSpecific("l ",pA,pB)
        neuPar%xR = UpdateInputKTzParamSpecific("xR",pA,pB)
        neuPar%H  = UpdateInputKTzParamSpecific("H ",pA,pB)
        neuPar%Z  = UpdateInputKTzParamSpecific("Z ",pA,pB)
    end function UpdateInputKTzParams
    
    function UpdateInputKTzParamSpecific(p,pA,pB) result (v)
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
            else if (trim(p) == "Z") then
                v = par%Z
            else
                v = 1.0/0.0
                write (*,*) "UpdateInputKTzParamSpecific:::ERROR ... unknown parameter ",p
            end if
        end if
    end function UpdateInputKTzParamSpecific

    subroutine SimulaPara_phaseDiag_ISI(neuPar, KTzFunc, isi, intensity, isiper)
        use Input
        implicit none
        real(kr8), allocatable, intent(inout) :: isi(:), intensity(:), isiper(:)
        real(kr8) :: x(3) ! x(1) = x, x(2) = y, x(3) = z
        !real(kr8), intent(in) :: K, T, d, l, xR, H
        real(kr8), allocatable :: isiData(:)
        real(kr8) :: t0, xAnt ! , t2, ts, tsA
        integer :: t, tEff, is_fp_counter, k
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
        do t = 1,par%tTransient
            !xAnt = x(1)
            !write(*,*) x
            x = KTzFunc(neuPar, x)
            ! if the dynamics is too slow, the system can lurk around a FP of the fast subsystem for too long, 
            ! fooling this simple method to detect FP... so we are not checking for FP during transient time
            !if (dabs(x(1) - xAnt) <= 1.0e-8) then
            !    isFP = .true.
            !    exit
            !end if
        end do
        !if (.not.isFP) then
        !    t  = 0 ! time counter
        !    tt = tEff / 2 ! tolerance time to start looking for FP
        ! garantindo que o loop principal começara com x(i)<0
        !do while (x(1) > par%xThreshold)
        !    !xAnt = x(1)
        !    !write(*,*) x
        !    x = KTzFunc(neuPar, x)
        !    !if ((dabs(x(1) - xAnt) <= 1.0e-8) .and. (t > tt)) then
        !    !    ! if the dynamics is too slow, the system can lurk around a FP of the fast subsystem for too long, 
        !    !    ! fooling this simple method to detect FP...
        !    !    ! thus, we wait a period tt to start checking if we are at a fixed point
        !    !    isFP = .true.
        !    !    exit
        !    !end if
        !end do
        !end if

        ! calculating ISI
        is_fp_counter = 0
        k             = 0 ! number of ISI that were found
        t0            = 0
        do t = 1,tEff ! loop principal
            xAnt = x(1)
            x    = KTzFunc(neuPar, x)

            ! checking if this is a FP
            if ((dabs(x(1) - xAnt) < 1.0e-8)) then !.and. (t > (tEff/2))) then
                is_fp_counter = is_fp_counter + 1
            else
                is_fp_counter = 0
            end if
            if (is_fp_counter > (tEff/2)) then ! this is a fixed point if the attractor stayed equal for too long
                isFP = .true.
                exit
            end if

            ! this is not a FP point...
            if ((xAnt - par%xThreshold) * (x(1) - par%xThreshold) < 0.0D0) then ! crossed x==xThreshold
                if ((dabs(xAnt - par%xThreshold)>1.0D-10).and.(xAnt < x(1))) then ! the curve is climbing, and the previous x cannot be the threshold
                    k          = k + 1
                    isiData(k) = t - t0 + 1
                    t0         = t
                end if
            end if
        end do
        !end if
        
        if (allocated(isi)) then
            deallocate(isi, intensity, isiper)
        end if
        if (isFP.or.(k == 0).or.(k == 1)) then ! nenhum ISI encontrado
            allocate(isi(1:1), intensity(1:1), isiper(1:1))
            isi(1)       = 0.0D0
            intensity(1) = 1.0D0
            isiper(1)    = 0.0D0
        else
            call unique(isiData(2:k), par%correctISI, isi, intensity)
            call CalcISIPeriod(isiData(2:k), isi, isiper)
        end if
    end subroutine  SimulaPara_phaseDiag_ISI

    subroutine SimulaPara_phaseDiag_AMP(neuPar, KTzFunc, KTzJac, amp, intensity, lambda_lyapm)
        use Input
        use Chaos
        implicit none
        real(kr8), allocatable, intent(inout) :: amp(:), intensity(:), lambda_lyapm(:)
        real(kr8) :: x(3) ! x(1) = x, x(2) = y, x(3) = z
        !real(kr8), intent(in) :: K, T, d, l, xR, H
        real(kr8) :: D_lyap(3,3), Tr_lyap(3,3), lambda_lyap(3)
        real(kr8) :: xMin, xMax, xAnt
        integer :: tt, tEff, tTotal, is_fp_counter
        logical :: isFP
        type(KTzParam), intent(in) :: neuPar
        procedure(KTzIterator),    pointer, intent(in) :: KTzFunc
        procedure(KTzJacobMatrix), pointer, intent(in) :: KTzJac

        if (.not.allocated(amp)) then
            allocate(amp(1:1), intensity(1:1), lambda_lyapm(1:1))
        end if
        
        intensity = 0.0D0
        tTotal    = par%tTotal
        tEff      = par%tTotal - par%tTransient
        isFP      = .false.
        
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
        is_fp_counter = 0
        do tt = 1,par%tTransient
            xAnt = x(1)
            x    = KTzFunc(neuPar, x)
            if ((dabs(x(1) - xAnt) < 1.0e-8)) then !.and. (t > (tEff/2))) then
                is_fp_counter = is_fp_counter + 1
            else
                is_fp_counter = 0
            end if
            if (is_fp_counter > (par%tTransient/2)) then ! this is a fixed point if the attractor stayed equal for too long
                isFP = .true.
                exit
            end if
            call LyapExpEckmannRuelleIter(KTzJac(neuPar,x),D_lyap,Tr_lyap,lambda_lyap)
        end do
        xMax = -huge(xMax)
        xMin = huge(xMin)
        if (.not.isFP) then
            is_fp_counter = 0
            do tt = 1,tEff ! loop principal
                xAnt = x(1)
                x = KTzFunc(neuPar, x)
                if (x(1) < xMin) then
                    xMin = x(1)
                end if
                if (x(1) > xMax) then
                    xMax = x(1)
                end if
                if ((dabs(x(1) - xAnt) < 1.0e-8)) then !.and. (t > (tEff/2))) then
                    is_fp_counter = is_fp_counter + 1
                else
                    is_fp_counter = 0
                end if
                if (is_fp_counter > (tEff/2)) then ! this is a fixed point if the attractor stayed equal for too long
                    isFP = .true.
                    exit
                end if
                call LyapExpEckmannRuelleIter(KTzJac(neuPar,x),D_lyap,Tr_lyap,lambda_lyap)
            end do
        end if
        if (isFP) then
            amp          = 0.0D0
        else
            amp          = xMax - xMin
        end if
        lambda_lyapm = maxval(lambda_lyap / tTotal)
    end subroutine  SimulaPara_phaseDiag_AMP

    subroutine SimulaPara_phaseDiag_WIN(neuPar, KTzFunc, windingnum, cycles, period)
        use Input
        implicit none
        real(kr8), allocatable, intent(inout) :: windingnum(:), cycles(:), period(:)
        real(kr8), allocatable :: x_values(:,:)
        real(kr8) :: x(3) ! x(1) = x, x(2) = y, x(3) = z
        !real(kr8), intent(in) :: K, T, d, l, xR, H
        real(kr8) :: xAnt, tol ! , t2, ts, tsA
        integer :: t, tEff, is_fp_counter, k, max_period, min_period
        integer :: period_int, cycles_int
        logical :: isFP, period_found
        type(KTzParam), intent(in) :: neuPar
        procedure(KTzIterator),    pointer, intent(in) :: KTzFunc

        if (.not.allocated(windingnum)) then
            allocate(period(1:1), cycles(1:1), windingnum(1:1))
        end if

        isFP         = .false.
        tEff         = par%tTotal - par%tTransient
        max_period   = tEff    ! maximum possible period
        min_period   = 1       ! minimum possible period (1=FP)
        period       = -1      ! Initialize as -1 to indicate no period found
        cycles       = 0       ! number of cycles within a period
        tol          = 1.0D-8  ! Tolerance for floating-point comparison
        period_found = .false. ! is it found
        
        if (.not.allocated(x_values)) then
            allocate(x_values(1:tEff,1:3)) ! alocando tEff valores para os isi
        end if
        
        x = par%x0
        do t = 1,par%tTransient
            x = KTzFunc(neuPar, x)
        end do

        do t = 1,tEff ! loop principal
            xAnt            = x(1)
            x               = KTzFunc(neuPar, x)
            x_values(t,1:3) = x
            
            ! checking if this is a FP
            if ((dabs(x(1) - xAnt) < 1.0e-8)) then !.and. (t > (tEff/2))) then
                is_fp_counter = is_fp_counter + 1
            else
                is_fp_counter = 0
            end if
            if (is_fp_counter > (tEff/2)) then ! this is a fixed point if the attractor stayed equal for too long
                isFP   = .true.
                period_int = 1
                cycles_int = 0
                !write(*,*) "DEBUG ::: isFP == .true."
                exit
            end if

            ! this is not a FP point...
            ! Check for periodicity by calling the function
            period_int = check_periodicity(x, x_values, t, tol, max_period, min_period)
            !write(*,*) "period_int = ", period_int
            if (period_int > 0) then
                !write(*,*) "DEBUG ::: found period"
                period_found = .true.
                cycles_int   = count_cycles(x_values(1:t,1),period_int,t,0.0D0)
                exit
            end if
        end do

        !write(*,*) "DEBUG ::: NOT found period"
        if ((.not.isFP) .and. (.not.period_found)) then
            cycles_int = count_cycles(x_values(1:,1),max_period,tEff,0.0D0)
        end if

        if ((.not.isFP) .and. (.not.period_found)) then
            ! period was not found
            period = 1.0/0.0
        else
            period = dble(period_int)
        end if
        cycles     = dble(cycles_int)
        windingnum = cycles / period

    end subroutine  SimulaPara_phaseDiag_WIN

    subroutine SimulaMapa(neuPar, model, x0, t_trans, t_total, x_values)
        implicit none
        character(len=1), intent(in)  :: model
        type(KTzParam)  , intent(in)  :: neuPar
        real(kr8)       , intent(in)  :: x0(3)
        integer         , intent(in)  :: t_trans, t_total
        real(kr8)       , allocatable, intent(inout) :: x_values(:,:)
        real(kr8) :: x(3) ! x(1) = x, x(2) = y, x(3) = z
        integer :: t, tEff
        procedure(KTzIterator),    pointer :: KTzFunc

        call GetMapFunc(model, KTzFunc)
        tEff    =  t_total - t_trans
        
        if (.not.allocated(x_values)) then
            allocate(x_values(1:tEff,1:3)) ! alocando tEff valores para os isi
        end if
        
        x = x0
        do t = 1,t_trans
            x = KTzFunc(neuPar, x)
        end do

        x_values(1,1:3) = x ! initial condition
        do t = 2,tEff ! loop principal
            x               = KTzFunc(neuPar, x)
            x_values(t,1:3) = x
        end do

    end subroutine SimulaMapa

    ! intensity eh a qtd de vezes que um valor de x se repete, em relacao ao total de valores de x
    ! tal que sum(intensity) = 1
    subroutine unique(x, correct_pm1_val, xUn, intensity)
        implicit none
        real(kr8), intent(in) :: x(:)
        real(kr8), allocatable, intent(inout) :: xUn(:), intensity(:)
        real(kr8) :: res(size(x,1))
        real(kr8) :: intTemp(size(x,1))
        integer :: k                   ! The number of unique elements
        integer :: i, j
        logical :: correct_pm1_val

        intTemp = 1.0D0

        k = 1
        res(1) = x(1)
        outer: do i=2,size(x)
            do j=1,k
                if (is_equal_ISI(res(j),x(i),correct_pm1_val)) then
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

    function is_equal_ISI(isi1,isi2,correct_isi) result (r)
        implicit none
        real(kr8) :: isi1, isi2
        logical   :: correct_isi, r
        !integer   :: i1,i2
        !if (correct_isi) then
        !i1 = int(floor(isi1))
        !i2 = int(floor(isi2))
        !    r = (i1 == i2) .or. (i1 == (i2-1)) .or. (i1 == (i2+1))
        !else
        r = int(floor(isi1)) == int(floor(isi2))
        !end if
    end function

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
            if (is_equal_ISI(X(i),value,.true.)) then !if (X(i) == value) then
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
    subroutine findISI(x, isi, xThreshold)
        implicit none
        real(kr8), intent(in) :: x(:)
        real(kr8), allocatable, intent(out) :: isi(:)
        real(kr8), allocatable :: isiData(:)
        real(kr8)  :: xThreshold
        integer :: k, t0, t, tnext, n!, crossCounter
        !real*8 :: t1, t2, ts, tsA

        n = size(x) - 1
        allocate(isiData(1:n)) ! alocando n valores para os isi
        t  = 1
        k  = 0
        t0 = 0
        do while (t <= n) ! loop principal
            tnext = t + 1
            if ((x(t) - xThreshold) * (x(tnext) - xThreshold) < 0.0D0) then ! crossed x==xThreshold
                if ((dabs(x(t) - xThreshold)>1.0D-10).and.(x(t) < x(tnext))) then ! the curve is climbing, and the previous x cannot be the threshold
                    !print *, 'crossed'
                    k          = k + 1
                    isiData(k) = t - t0 + 1
                    t0         = tnext
                end if
            end if
            t = t + 1
        end do
        allocate(isi(2:k))
        isi = isiData(2:k)
    end subroutine findISI

    function findPeriod(x_values, epsilon) result(period)
        implicit none
        real(kr8), allocatable, intent(in) :: x_values(:)    ! Input array of values
        real(kr8),              intent(in) :: epsilon        ! Tolerance for periodicity
        real(kr8), allocatable             :: differences(:) ! Array to hold differences
        integer :: period, T, i, n                           ! Output: period T if found, -1 otherwise
    
        !write (*,*) "TEST: 1"
        period = -1                       ! Initialize period as -1 (not found)
        n      = size(x_values)
        allocate(differences(1:(n-1)))
        !write (*,*) "TEST: 2"
        ! Iterate over possible periods T from 1 to n/2
        do T = 1, n / 2
            ! Calculate absolute differences for the current T
            differences(:n-T) = abs(x_values(:n-T) - x_values(T+1:))
            !write (*,*) "TEST: 3"
            ! Check if all differences are within the tolerance epsilon
            if (all(differences(:n-T) <= epsilon)) then
                period = T
                !write (*,*) "TEST: 4"
                return
            end if
        end do
        !write (*,*) "TEST: 5"
        deallocate(differences)                 ! Deallocate differences array
        !write (*,*) "TEST: 6"
    end function findPeriod

    function count_cycles(x,Q,Tmax,xThreshold) result(k)
        ! counts number of zero crossings (going up) between x(t) and x(t+Q)
        implicit none
        real(kr8), intent(in) :: x(:)
        real(kr8), intent(in) :: xThreshold
        integer  , intent(in) :: Q
        integer               :: k, t, tnext, Tmax
        
        !xThreshold = 0.0D0
        t  = 1
        do while (x(t)<=xThreshold)
            t = t + 1 ! make sure we start from positive x
        end do
        k  = 0
        Tmax  = min(t + Q,Tmax)
        do while (t <= Tmax) ! loop principal
            tnext = t + 1
            if ((x(t) - xThreshold) * (x(tnext) - xThreshold) < 0.0D0) then ! crossed x==xThreshold
                if ((dabs(x(t) - xThreshold)>1.0D-10).and.(x(t) < x(tnext))) then ! the curve is climbing, and the previous x cannot be the threshold
                    !print *, 'crossed'
                    k = k + 1
                end if
            end if
            t = t + 1
        end do
    end function count_cycles

    function KTzTanhIter(neuPar, xAnt) result (x)
        implicit none
        real(kr8) :: x(3), xAnt(3)
        type(KTzParam) :: neuPar
        x(2) = xAnt(1)
        x(3) = (1.0D0 - neuPar%d) * xAnt(3) - neuPar%l * (xAnt(1) - neuPar%xR)
        x(1) = my_tanh((xAnt(1) - neuPar%K * xAnt(2) + xAnt(3) + neuPar%H) / neuPar%T)
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
        x(2) = my_tanh((xAnt(1) + neuPar%H) / neuPar%T )
        x(3) = (1.0D0 - neuPar%d) * xAnt(3) - neuPar%l * (xAnt(1) - neuPar%xR)
        x(1) = my_tanh((xAnt(1) - neuPar%K * xAnt(2) + xAnt(3) + neuPar%Z) / neuPar%T)
    end function K2TzIter

    function KTzLogJacob(neuPar, x) result(J)
        implicit none
        real(kr8) :: x(3), a, J1, J(3,3)
        type(KTzParam) :: neuPar
        a = dabs(x(1)-neuPar%K*x(2)+x(3)+neuPar%H) !KTzLog
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
        y = 2.0D0 / (my_exp(x)+my_exp(-x))
    end function dsech

    function my_exp(x) result(res)
        implicit none
        real(kr8), intent(in) :: x
        real(kr8) :: res

        if (x < 709.782712893384D0) then
            res = dexp(x)
        else
            res = huge(x)
        end if
    end function my_exp

    function my_tanh(x) result(res)
        implicit none
        real(kr8), intent(in) :: x
        real(kr8) :: res
        res = 2.0D0 / (1.0D0 + my_exp(-2.0D0*x)) - 1.0D0
    end function my_tanh
      

    function logspace(a,b,n) result(r)
        ! returns n log-spaced base-10 values between a and b
        implicit none
        integer   :: n
        real(kind=8) :: r(n), a, b
        if (a*b < 0.0D0) then ! a and b have different signs
            write(*,*) 'logscale run-time WARNING: a and b have to have the same signs'
        end if
        if ((a == 0.0D0) .or. (b == 0.0D0)) then ! a and b have different signs
            write(*,*) 'logscale run-time WARNING: a or b are zero, expect weird results?'
        end if
        r = 10.0D0**linspace(dlog10(dabs(a)),dlog10(dabs(b)),n)
        if ((a < 0.0D0) .and. (b < 0.0D0)) then
            r = -r
        end if
    end function logspace
    
    function linspace(a,b,n) result(r)
        ! returns n values between a and b
        implicit none
        integer   :: n,i
        real(kind=8) :: r(n), a, b, dx
        if (n == 0) then
            n = 1
        end if
        if (n == 1) then
            r(1) = a
            return
        end if
        if (n < 0) then
            n = -n
        end if
        dx = (b-a)/dble(n-1)
        r = (/(a + dx*dble(i-1), i=1,n)/)
    end function linspace

    function int_to_str(inum,fmt) result(str)
        ! Writes single precision integer inum to string str using format fmt
        implicit none
        integer :: inum
        character(len=*) :: fmt
        character(len=80) :: formt,str
        
        formt='('//trim(fmt)//')'
        write(str,formt) inum
        str=adjustl(str)
    
    end function int_to_str

    function double_to_str(inum,fmt) result(str)
        ! Writes single precision integer inum to string str using format fmt
        implicit none
        real(kr8) :: inum
        character(len=*) :: fmt
        character(len=80) :: formt,str
        
        formt='('//trim(fmt)//')'
        write(str,formt) inum
        str=adjustl(str)
    
    end function double_to_str
        
    subroutine calculate_and_find_period_test(fase)
        implicit none
        integer :: t, Q, C, max_period, min_period, t_total, t_trans
        character(len=*), intent(in) :: fase
        logical :: period_found
        real(kr8) :: tol
        real(kr8) :: x(3)
        real(kr8), allocatable  :: x_values(:,:)
        type(KTzParam) :: neuPar

        t_total   = 20000
        t_trans   = 10000

        allocate(x_values(1:t_total,1:3))
    
        neuPar    = GetInputKTzParams()
        if ((trim(fase) == "0/1").or.(trim(fase) == "fp")) then
            ! fase 0/1 (Fixed point)
            neuPar%K  = 1.0D0
            neuPar%T  = 1.5D0
        else if (trim(fase) == "1/4") then
            ! fase 1/4
            neuPar%K  = 1.7D0
            neuPar%T  = 0.1D0
        else if (trim(fase) == "1/6") then
            ! fase 1/6
            neuPar%K  = 1.0D0
            neuPar%T  = 0.2D0
        else if (trim(fase) == "1/8") then
            ! 1/8
            neuPar%K = 0.796176
            neuPar%T = 0.211788
        else if (trim(fase) == "2/14") then
            ! 2/14
            neuPar%K = 0.857635
            neuPar%T = 0.235568 
        else if (trim(fase) == "3/16") then
            ! 3/16
            neuPar%K = 1.19426
            neuPar%T = 0.319157  
        else if (trim(fase) == "2/10") then
            ! 2/10
            neuPar%K = 1.31428
            neuPar%T = 0.34606   
        else if (trim(fase) == "3/14") then
            ! 3/14
            neuPar%K = 1.44899
            neuPar%T = 0.340557  
        else if (trim(fase) == "4/18") then
            ! 4/18
            neuPar%K = 1.71812
            neuPar%T = 0.527192  
        else if (trim(fase) == "aper") then
            neuPar%K = 0.6D0
            neuPar%T = 0.35D0  
        else
            write (*,*) "TEST ERROR ::: UNKNOWN fase IN calculate_and_find_period_test"
            return
        end if

        ! other params
        neuPar%d  = 0.0D0
        neuPar%l  = 0.0D0
        neuPar%xR = 0.0D0
        neuPar%H  = 0.0D0
        neuPar%Z  = 0.0D0

        write(*,*) neuPar

        x = (/ -0.5D0, -0.5D0, 0.0D0 /)

        max_period   = 10000  !
        min_period   = 1
        tol          = 1e-8  ! Tolerance for floating-point comparison
        period_found = .false.
        Q            = -1  ! Initialize as -1 to indicate no period found
        C            = 0
    
        do t = 1, t_total
            x               = KTzTanhIter(neuPar, x)                               ! Update x with the function F
            x_values(t,1:3) = x
            
            ! Check for periodicity by calling the function
            if ((t>t_trans) .and. (.not. period_found)) then
                Q = check_periodicity(x, x_values, t, tol, max_period, min_period)
                write (*,*) "out Q=",Q
                if (Q > 0) then
                    period_found = .true.
                    C            = count_cycles(x_values(1:t,1),Q,t,0.0D0)
                    exit
                end if
            end if
        end do
    
        if (period_found) then
            print *, "Period found:", Q
            print *, "Number of cycles:", C
            print *, "w = ", C, "/", Q
        else
            print *, "No period found within t_total"
        end if
    end subroutine calculate_and_find_period_test
    
    
    
    ! Function to check if x matches any previous value within tolerance
    function check_periodicity(x, x_values, t, tol, max_period, min_period) result(Q)
        implicit none
        real(kr8), intent(in) :: x(3)
        real(kr8), allocatable, intent(in) :: x_values(:,:)
        real(kr8), intent(in) :: tol
        integer, intent(in) :: t
        integer, intent(in) :: max_period, min_period
        integer :: i, period_limit, Q
    
        Q = -1  ! Initialize as -1 (no period found)
    
        if (.not.allocated(x_values)) return
    
        ! Set the maximum number of previous steps to check
        period_limit = max(1, t - max_period)
    
        !write (*,*) 'ref (t,x) = ', t, x(1:2)
        !write (*,*) 'ref check (t,x_v) = ', t, x_values(t,1:2)

        do i = t - min_period, period_limit, -1
            !write (*,*) 'test (i,x) = ', i,x_values(i, 1:2)
            ! Check all elements only if the first element matches
            if (all(abs(x - x_values(i, 1:3)) < tol)) then
                !write (*,*) 'found (i,x) = ', i,x_values(i, 1:2)
                !write (*,*) 't-i=',t-i
                Q = t - i
                !write (*,*) 'Q=',Q
                ! t,     i       ,     i         ,     i         ,     i         ,     i         ,     i       
                ! t, t-min_period, t-min_period-1, t-min_period-2, t-min_period-3, t-min_period-4, t-min_period-5
                ! min_period = 1
                ! t = 1001
                !  t  ,   i   ,  i    ,  i    ,  i    ,  i    ,  i    ,  i    
                ! 1001, 1001-1, 1001-2, 1001-3, 1001-4, 1001-5, 1001-6, 1001-7
                !         1000,    999,    998,    997,    996,    995,    994
                ! t-i =      1,      2,      3,      4,      5,      6,      7
                return
            end if
        end do
    end function check_periodicity

end module Simulation 
