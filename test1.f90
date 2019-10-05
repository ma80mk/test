program readwritefile
implicit none 
integer, parameter :: nH =8, nCells =2,nhh=22
integer ::i,j,l,kk,nn,Cells(1:nCells, 1:3),iCell
real(8) ::h(1:nCells,1:nH,1:nH),hh(1:nhh*nH*nH),vv(1:nhh*nH*nH)
character(100) ::filename1,filename2
!Load files to read basic hamiltonian data
 Cells = reshape((/&
	& 0, 0, 0, &
	& 1, 0, 0 &
	&/), (/nCells, 3/), order = (/ 2, 1 /))
kk=1
do nn=1,nhh
   do iCell = 1,nCells
      write(filename1,'("C",I0,"_H(", I0, ",", I0, ",", I0, ").dat")') nn,Cells(iCell,1) ,Cells(iCell,2) ,Cells(iCell,3)
      open(unit=100, file=filename1, action='read')
      do i=1,nH
         Read(100,*)(h(iCell,i,j) , j=1,nH)
      end do
      close(100)
   end do       
   do i=1,nH
      do j=1,nH
         hh(kk)=h(1,i,j)
         vv(kk)=h(2,i,j)
         kk=kk+1
      end do
   end do
end do
do i=1,nH*nH
   l=1
   write(filename1,'("H",I0,".dat")') i
   write(filename2,'("V",I0,".dat")') i
   open(unit=i, file=filename1, action='write')
   open(unit=i+105, file=filename2, action='write')
   do j=i,nhh*nH*nH,64
      write(i,*)((2.37+0.01*(l-1))/2.4791)-1,hh(j)
      write(i+105,*)((2.37+0.01*(l-1))/2.4791)-1,vv(j)
      l=l+1
   end do
   close(i);close(i+105)
end do

end program readwritefile
