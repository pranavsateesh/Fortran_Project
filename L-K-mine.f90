subroutine fn(t, x, n, dx)
    implicit none
    integer :: n
    real :: t, prey_b, pred_b, prey_d, pred_d
    real, dimension(n) :: x
    real, dimension(n), intent(out) :: dx
    !Initialize
    prey_b = 2.0
    pred_b = 0.01
    prey_d = 0.02
    pred_d = 1.0
    !Define first order DEs for return from the L-V Model
    dx(1) = prey_b*x(1) - prey_d*x(1)*x(2)
    dx(2) = -pred_d*x(2) + pred_b*x(1)*x(2)

end subroutine
subroutine rk(t0, tn, x0, xn, n)
    implicit none
    integer :: i, n
    real :: t0, tn, h, t
    real, dimension(n) :: x0, xn, x, dx, k1, k2, k3, k4

    !Define Step
    h = tn - t0
    !Initialize time
    t = t0

    !First weighted increment K1
    call fn(t, x0, n, dx)
    do i=1,n
        k1(i) = h*dx(i)
        x(i)  = x0(i) + k1(i)/2.0
    end do

    !Second weighted increment K2
    call fn(t+h/2.0, x, n, dx)
    do i=1,n
        k2(i) = h*dx(i)
        x(i)  = x0(i) + k2(i)/2.0
    end do

    !Third weighted increment K3
    call fn(t+h/2.0, x, n, dx)
    do i=1,n
        k3(i) = h*dx(i)
        x(i)  = x0(i) + k3(i)
    end do

    !Final weighted increment K4
    call fn(t+h, x, n, dx)
    do i=1,n
        k4(i) = h*dx(i)
        xn(i) = x0(i) + k1(i)/6.0+k2(i)/3.0+k3(i)/3.0+k4(i)/6.0
    end do
end subroutine

program LV
    implicit none
    integer :: n, i
    real :: t0, tn, max_t, dt
    real, dimension(2) :: x0, xn

    open(10, file="Data.txt", status="unknown", action="write")

    !Initialise data
    t0 = 0
    x0(1) = 100
    x0(2) = 15
    n = 2
    dt = 0.2
    max_t = 10

    write(10,*), " Predator-Prey Ecology. Lotka-Volterra Model "
    write(10,*), "      Method: Fourth Order R-K       "
    write(10,1)
    write(10,2), t0, x0(1), x0(2)

    !Integration
    do while(t0 <= max_t)
        tn = t0 + dt
        call rk(t0, tn, x0, xn, n)
        write(10,2), tn, xn(1), xn(2)

        !Step
        t0 = tn
        do i=1,n
            x0(i) = xn(i)
        end do
    end do
1 format(5x,'T',11x,'Prey',11x,'Pred')
2 format(3(1pe12.3))
end program
