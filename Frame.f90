program Frame_grid
  use makeslice 
  implicit none 
  integer N
  real(8) tol
  real(8),dimension(:),allocatable:: cut
  character dim
  character(100):: name
  name="surfacenew.dat"
  N=1
  tol=0.13
  dim='Y'
  allocate(cut(N))
  cut = (/25/)
  call slice(name,dim,cut,N,tol)
end program Frame_grid
    
