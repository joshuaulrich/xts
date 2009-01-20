      subroutine pprodz(bp,lbp,ia,lia,ret)
c     Usage:
c      
c     bp   index of breakpoints
c     lbp  length of breakpoints index
c     ia   input array of double precision
c     lia  length of ia vector
c     ret  return array of values
c
      implicit none

      integer lbp,lia
      integer i,j
      integer bp(lbp),pos
      double precision ps(lbp)
      double precision ia(lia), ret(*)

c
c     pos keeps track of location in col-major array
c
      pos = 1
c
c     data must be OHLC or value,volume
c     offsets if incoming OHLC data
       
      do 10 i=1,(lbp-1)
c      
c     step through each period of bp
c
        ps(i) = 1.0D0
        do 20 j=(bp(i)+1),bp(i+1)
c
c       step through each obs. and note values
          ps(i) = ps(i) * ia(j)

   20   continue

c
c       after each period - create entry in ret array
c
        ret(pos) = ps(i)
c
c       increment position by 1
c
        pos = i+1

   10 continue
      end
