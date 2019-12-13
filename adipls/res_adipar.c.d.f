      subroutine res_adipar(i_param,
     * el, els1, dels, dfsig1, dfsig2, sig1, sig2, dfsig,
     * eltrw1, eltrw2, sgtrw1, sgtrw2,
     * nsel, nsig1, nsig2, itrsig, nsig, istsig,
     * inomd1, iscan, 
     * iper, ivarf, irotkr, nprtkr, ispcpr, ! added by me 
     * iem, irotsl, nsem, idems,  ! added by me 
     * omega0)  ! added by me 
c
c
c  set or reset parameters to be iterated in calls of adipls
c
c  For i_param = 1, store parameters in para_el, etc. from 
c  parameters in arguments. 
c
c  For i_param = 2, reset parameters in arguments from para_el, ...
c
c  Original version: 8/7/05
c
      implicit double precision(a-h, o-z)

!      implicit none 
c
c
c  commons for parameter transmission
!  set implicit none, declare all parameters in commons
!  declare both 'before convert' (ipara_*) and 'after convert' (*) parameters
c
      common/cadr_param/
     *  para_el, para_els1, para_dels, para_dfsig1, para_dfsig2,
     *  para_sig1, para_sig2, para_dfsig, para_eltrw1, para_eltrw2,
     *  para_sgtrw1, para_sgtrw2
!      integer ::  &
!         para_el, para_els1, para_dels, &       
!         el, els1, dels
!      real(dp) :: &
!         para_dfsig1, para_dfsig2, para_sig1, para_sig2, para_dfsig, &
!         para_eltrw1, para_eltrw2, para_sgtrw1, para_sgtrw2, &
!         dfsig1, dfsig2, sig1, sig2, dfsig, eltrw1, eltrw2, sgtrw1, &
!         sgtrw2
c
      common/cadi_param/
     *  ipara_nsel, ipara_nsig1, ipara_nsig2, ipara_itrsig, ipara_nsig, 
     *  ipara_istsig, ipara_inomd1, ipara_iscan
!      integer :: & 
!        ipara_nsel, ipara_nsig1, ipara_nsig2, ipara_itrsig, ipara_nsig, &
!        ipara_istsig, ipara_inomd1, ipara_iscan, &
!        nsel, nsig1, nsig2, itrsig, nsig, istsig, inomd1, iscan
c
      common/rotdat/
     *  ipara_iem, ipara_irotsl, ipara_nsem, ipara_idems    ! added by me
!      integer :: 
!     *  ipara_em, ipara_irotsl, ipara_nsem, ipara_dems, 
!     *  em, irotsl, nsem, dems
c
      common/rotvel/ para_omega0   ! added by me 
!      real(dp) :: 
!     *  para_omega0, omega0
c
      common/coutpt/ ipara_nout, ipara_nprcen, ipara_iper, ipara_irotkr, 
     *  ipara_nprtkr, ipara_igm1kr, ipara_npgmkr, ipara_nfmode, 
     *  ipara_nfmesh, ipara_ispcpr, ipara_npout, ipara_nobs_stmx, ipara_nfmscn  ! added by me 
!      integer :: &
!        ipara_nout, ipara_nprcen, ipara_iper, ipara_irotkr, &
!        ipara_nprtkr, ipara_igm1kr, ipara_npgmkr, ipara_nfmode, &
!        ipara_nfmesh, ipara_ispcpr, ipara_npout, ipara_nobs_stmx, ipara_nfmscn, &
!        nout, nprcen, iper, irotkr, nprtkr, igm1kr, npgmkr, nfmode, &
!        nfmesh, ispcpr, npout, nobs_stmx, nfmscn
c
      common/varcon/ ipara_ivarf, ipara_npvarf, ipara_kvarfc  ! added by me 
!      integer :: &
!        ipara_ivarf, ipara_npvarf, ipara_kvarfc, &
!        ivarf, npvarf, kvarfc 
c
c  common defining standard input and output, standard error
c
      common/cstdio/ istdin, istdou, istdpr, istder
!      integer :: istdin, istdou, istdpr, istder 
c 
      save
c
      write(*,*) 'Subroutine res_adipar called'

      if(istdpr.gt.0) 
     *  write(istdpr,*) 'Enter res_adipar with i_param =',i_param
c
c  test for storing original parameters in common /cadr_param/, /cadi_param/, /rotdat/, /coutpt/, /varcon/, /rotvel/
c
      if(i_param.eq.1) then
        para_el = el
        para_els1 = els1
        para_dels = dels
        para_dfsig1 = dfsig1
        para_dfsig2 = dfsig2
        para_sig1 = sig1
        para_sig2 = sig2
        para_dfsig = dfsig
        para_eltrw1 = eltrw1
        para_eltrw2 = eltrw2
        para_sgtrw1 = sgtrw1
        para_sgtrw2 = sgtrw2
c
        ipara_nsel = nsel
        ipara_nsig1 = nsig1
        ipara_nsig2 = nsig2
        ipara_itrsig = itrsig
        ipara_nsig = nsig
        ipara_istsig = istsig
        ipara_inomd1 = inomd1
        ipara_iscan = iscan
c
	ipara_iem = iem
	ipara_irotsl = irotsl 
	ipara_nsem = nsem 
	ipara_idems = idems
c
	para_omega0 = omega0 
c
	ipara_iper = iper 
	ipara_ivarf = ivarf 
	ipara_irotkr = irotkr 
	ipara_nprtkr = nprtkr 
	ipara_ispcpr = ispcpr

	write(*,*) 'Show me iem, irotsl, irotkr, omega0 in res_adipar'
	write(*,*) iem, irotsl, irotkr, omega0
	
c
c  test for setting parameters from /cadr_param/ into internal 
c  parameters
c
      else if(i_param.eq.2) then
        el = para_el
        els1 = para_els1
        dels = para_dels
        dfsig1 = para_dfsig1
        dfsig2 = para_dfsig2
        sig1 = para_sig1
        sig2 = para_sig2
        dfsig = para_dfsig
        eltrw1 = para_eltrw1
        eltrw2 = para_eltrw2
        sgtrw1 = para_sgtrw1
        sgtrw2 = para_sgtrw2
c
        nsel = ipara_nsel
        nsig1 = ipara_nsig1
        nsig2 = ipara_nsig2
        itrsig = ipara_itrsig
        nsig = ipara_nsig
        istsig = ipara_istsig
        inomd1 = ipara_inomd1
        iscan = ipara_iscan
c
	iem = ipara_iem   
	irotsl = ipara_irotsl 
	nsem = ipara_nsem 
	idems = ipara_idems
c
	omega0 = para_omega0 
c
	iper = ipara_iper 
	ivarf = ipara_ivarf 
        irotkr = ipara_irotkr
	nprtkr = ipara_nprtkr
	ispcpr = ipara_ispcpr 

	write(*,*) 'Show me ipara_iem, ipara_irotsl, ipara_irotkr,'
	write(*,*) 'para_omega0 in res_adipar'
	write(*,*) ipara_iem, ipara_irotsl, ipara_irotkr, para_omega0
c
      end if
      return 
      end
