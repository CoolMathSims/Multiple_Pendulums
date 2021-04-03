program dpen
use params
use sub

  REAL :: particles(0:7,0:3000)
  !7 is num particles*dimension -1
! so 2 particles * 4 columns per particle gives 8 columns
!but we are starting from 0 instead of 1 so it needs to be (particles*columns)-1

  OPEN(10,file ='coord1.dat',form='formatted')
  do j = 0,7,4

    t1 = Pi/4.!45.*PI/180.
    t2 = Pi/(j+1.0)!45.*PI/180.

    CALL pendulum

    do i =0,3000
      particles(j,i) = x1(i)
      particles(j+1,i) = y1(i)
      particles(j+2,i) = x2(i)
      particles(j+3,i) = y2(i)
    end do
  end do

  DO i = 0,3000
    WRITE(10,'(101F12.6)')(particles(:,i))
  end do

end program dpen
