program imager
    implicit none
    real, dimension(450,450) :: image, mask, s
    integer :: i, j
    open(10, file="deet.txt", status="unknown", action="read")
    do i=1, 450
        do j = 1,450
            image(i,j) = 0
            mask(i,j) = 0
            s(i,j) = 0
        end do
    end do
    !Read the image matrix into image
    do i = 1,400
        read(10,*), image(i,:)
    end do

    do i = 1, 400
        print *, image(:,i)
    end do
    !mask check
    do i=1, 400
        do j= 1, 400
            if (image(i,j) > 255) then
                mask(i,j) = 0
            else
                mask(i,j) = 1
            end if
        end do
    end do
    do i=2, 399
        do j=2, 399
            if(mask(i,j) == 0) then
                s(i,j) = 0.25*(s(i+1,j) + s(i-1,j) + s(i,j+1) + s(i,j-1))
            end if
        end do
    end do
    open(20, file="restore.txt", status="unknown", action="write")
    do i=1, 450
        write(20,*), s(i,:)
    end do
end program
