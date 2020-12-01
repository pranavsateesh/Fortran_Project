subroutine fn(t, x, n, dx)
    implicit none
    integer :: n
    real :: t, animal1_deathrate,animal1_birthrate,animal2_deathrate,animal2_birthrate,animal3_deathrate,animal3_birthrate
    real :: animal1to2_deathrate,animal1to3_deathrate,animal2to1_deathrate,animal2to3_deathrate,animal3to1_deathrate
    real :: animal3to2_deathrate, inhibhillfunc11,inhibhillfunc21,inhibhillfunc31
    real :: inhibhillfunc12,inhibhillfunc22,inhibhillfunc32
    real :: inhibhillfunc1,inhibhillfunc2,inhibhillfunc3
    real, dimension(n) :: x
    real, dimension(n), intent(out) :: dx
    !Initialize
    animal1_birthrate = 2.0
    animal2_birthrate = 0.01
    animal3_birthrate = 0.5
    animal1to2_deathrate = 0.02
    animal1to3_deathrate = 1.0
    animal2to1_deathrate = 0.5
    animal2to3_deathrate = 1.0
    animal3to1_deathrate = 0.5
    animal3to2_deathrate = 1.0
    animal1_deathrate = 1.0
    animal2_deathrate = 1.0
    animal3_deathrate = 1.0
    
    !Define first order DEs for return from the L-V Model
    inhibhillfunc11 = (animal1to2_deathrate**2/(animal1to2_deathrate**2 + x(2)**2))
    inhibhillfunc12 = (animal1to3_deathrate**2/(animal1to3_deathrate**2 + x(3)**2))
    inhibhillfunc21 = (animal2to1_deathrate**2/(animal2to1_deathrate**2 + x(1)**2))
    inhibhillfunc22 = (animal2to3_deathrate**2/(animal2to3_deathrate**2 + x(3)**2))
    inhibhillfunc31 = (animal3to1_deathrate**2/(animal3to1_deathrate**2 + x(1)**2))
    inhibhillfunc32 = (animal3to2_deathrate**2/(animal3to2_deathrate**2 + x(2)**2))
    inhibhillfunc1 = inhibhillfunc11*inhibhillfunc12
    inhibhillfunc2 = inhibhillfunc21*inhibhillfunc22
    inhibhillfunc3 = inhibhillfunc31*inhibhillfunc32
    dx(1) = x(1)*(animal1_birthrate*(inhibhillfunc1)-animal1_deathrate*x(1))
    dx(2) = x(2)*(animal2_birthrate*(inhibhillfunc2)-animal2_deathrate*x(2))
    dx(3) = x(3)*(animal3_birthrate*(inhibhillfunc3)-animal3_deathrate*x(3))
    

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

    open(10, file="animals.txt", status="unknown", action="write")

    !Initialise data
    t0 = 0
    x0(1) = 100
    x0(2) = 15
    x0(3) = 130
    n = 2
    dt = 0.2
    max_t = 1000

    write(10,*), " Predator-Prey Ecology. Lotka-Volterra Model "
    write(10,*), "      Method: Fourth Order R-K       "
    write(10,1)
    write(10,2), t0, x0(1), x0(2), x0)(3)  

    !Integration
    do while(t0 <= max_t)
        tn = t0 + dt
        call rk(t0, tn, x0, xn, n)
        write(10,2), tn, xn(1), xn(2), xn(3)

        !Step
        t0 = tn
        do i=1,n
            x0(i) = xn(i)
        end do
    end do
1 format(5x,'T',11x,'Prey',11x,'Pred')
2 format(3(1pe12.3))
end program
