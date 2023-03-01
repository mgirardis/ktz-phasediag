module Chaos

    use precision

    public :: LU_decomp, LyapExpEckmannRuelleIter, diag, identitymat

contains

    subroutine LyapExpEckmannRuelleIter(J,D,T,lambda)
        implicit none
        real(kr8), intent(in)    :: J(:,:)
        real(kr8), intent(inout) :: lambda(:), D(:,:), T(:,:)

        ! J      -> Jacobian matrix for the current time step, J(t)
        ! lambda -> value of the Lyapunov exponent up until the previous time step, lambda(t-1)
        ! D      -> lower triangular matrix, such that D*T = J(t-1)
        ! we want to find a new matrix T = J(t) * D
        call LU_decomp(matmul(J,D),D,T)!,3) ! decomposes the product J*D into a new matrix D and T
        lambda = lambda + dlog(dabs( diag(T) ))
    end subroutine LyapExpEckmannRuelleIter

    function identitymat(n) result(eye)
        implicit none
        real(kr8), allocatable :: eye(:,:)
        integer :: n,i,j
        allocate(eye(1:n,1:n))
        do i = 1,n
            do j = 1,n
                if (i == j) then
                    eye(i,j) = 1.0D0
                else
                    eye(i,j) = 0.0D0
                end if
            end do
        end do
    end function identitymat

    function diag(A) result(d)
        implicit none
        real(kr8)              :: A(:,:)
        real(kr8), allocatable :: d(:)
        integer                :: i,n
        n = min(size(A,1),size(A,2))
        allocate(d(1:n))
        do i = 1,n
            d(i) = A(i,i)
        end do
    end function diag

    subroutine LU_decomp(A, L, U)!, n)
        implicit none
        ! adapted from https://www.geeksforgeeks.org/doolittle-algorithm-lu-decomposition/
        !integer, intent(in) :: n
        !real(kr8), dimension(n*n), intent(in) :: A
        !real(kr8), dimension(n*n), intent(inout) :: L
        !real(kr8), dimension(n*n), intent(inout) :: U
        real(kr8),  intent(in)    :: A(:,:)
        real(kr8),  intent(inout) :: L(:,:)
        real(kr8),  intent(inout) :: U(:,:)
        real(kr8)                 :: s
        integer                   :: i, j, k, n

        n = size(A,1)

        L(:,:) = 0.0
        U(:,:) = 0.0

        do i = 1,n
            do j = i,n
                s = 0.0
                do k = 1,i-1
                    s = s + L(i,k) * U(k,j) !L((k-1)*n+i) * U((j-1)*n+k)
                end do
                U(i,j) = A(i,j) - s !U((j-1)*n+i) = A((j-1)*n+i) - s
            end do
            do j = i,n
                if (i == j) then
                    L(i,j) = 1.0 !L((j-1)*n+i) = 1.0 ! main diagonal is 1
                else
                    s = 0.0
                    do k = 1,i-1
                        s = s + L(j,k) * U(k,i) !s = s + L((k-1)*n+j) * U((i-1)*n+k)
                    end do
                    L(j,i) = (A(j,i) - s) / U(i,i) !L((i-1)*n+j) = (A((i-1)*n+j) - s) / U((i-1)*n+i)
                end if
            end do
        end do
    end subroutine LU_decomp


end module Chaos
