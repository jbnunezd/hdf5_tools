!==================================================================================================!
PROGRAM HDF5_TUTORIAL
!--------------------------------------------------------------------------------------------------!
#ifdef MPI
USE MPI_F08
#endif
!--------------------------------------------------------------------------------------------------!
USE HDF5
USE MOD_HDF5_Tools
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER            :: ii, jj
INTEGER            :: nVar
INTEGER            :: nDims
INTEGER            :: nElemsX
INTEGER            :: nElemsY
REAL               :: tEnd
REAL               :: MESH_X0(2)
REAL               :: MESH_X1(2)
REAL               :: MESH_DX(2)
REAL               :: MESH_SX(2)
REAL               :: x(2), xm(2), xc(2), r
REAL,ALLOCATABLE   :: MeshNodes(:,:,:)
REAL,ALLOCATABLE   :: U(:,:,:)
REAL,ALLOCATABLE   :: Uin(:,:,:)
CHARACTER(LEN=256) :: filename
CHARACTER(LEN=256) :: StringVariable
CHARACTER(LEN=256),ALLOCATABLE :: VarNames(:)
CHARACTER(LEN=256),ALLOCATABLE :: VarNames2(:)
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T)     :: file_id
INTEGER(HID_T)     :: mygroup_id
INTEGER            :: myrank
INTEGER            :: mydims(3)
INTEGER            :: iError
!--------------------------------------------------------------------------------------------------!
#ifdef MPI
INTEGER            :: iRank
INTEGER            :: nProcs
INTEGER            :: myComm
LOGICAL            :: MPIROOT
#endif
!--------------------------------------------------------------------------------------------------!
#ifdef MPI
INTEGER(HSIZE_T)   :: dimsf(3)
INTEGER(HSIZE_T)   :: offset(3)
#endif
!--------------------------------------------------------------------------------------------------!

! Initialize MPI
#ifdef MPI
CALL MPI_INIT(&
  ierror = iError)

CALL MPI_COMM_RANK(&
  comm   = MPI_COMM_WORLD,&
  rank   = iRank,&
  ierror = iError)

CALL MPI_COMM_SIZE(&
  comm   = MPI_COMM_WORLD,&
  size   = nProcs,&
  ierror = iError)

MPIROOT = (iRank .EQ. 0)
#endif

! Set Array Dimensions
nVar  = 5
nDims = 2
nElemsX = 1000
nElemsY = 1000

MESH_X0 = (/0.0,0.0/)
MESH_X1 = (/1.0,1.0/)

! Allocate Variables
ALLOCATE(U(1:nVar,1:nElemsX,1:nElemsY))
ALLOCATE(Uin(1:nVar,1:nElemsX,1:nElemsY))
ALLOCATE(MeshNodes(1:nDims,1:nElemsX,1:nElemsY))
ALLOCATE(VarNames(1:nVar))
ALLOCATE(VarNames2(1:nVar))

VarNames(1) = "Density"
VarNames(2) = "VelocityX"
VarNames(3) = "VelocityY"
VarNames(4) = "VelocityZ"
VarNames(5) = "Pressure"

! Build Mesh
MESH_SX = ABS(MESH_X1-MESH_X0)
MESH_DX(1) = ABS(MESH_SX(1))/(REAL(nElemsX-1))
MESH_DX(2) = ABS(MESH_SX(2))/(REAL(nElemsY-1))
DO jj=1,nElemsY
  DO ii=1,nElemsX
    MeshNodes(1:nDims,ii,jj) = Mesh_X0(1:nDims) + (/REAL(ii-1),REAL(jj-1)/)*MESH_DX(1:nDims)
  END DO
END DO

! Fill Initial Condition
DO jj=1,nElemsY
  DO ii=1,nElemsX
    x     = MeshNodes(1:nDims,ii,jj)
    xm    = MESH_X0+0.5*MESH_SX
    xc    = x-xm
    r     = SQRT(xc(1)**2 + xc(2)**2)

    U(1,ii,jj) = 1.0
    U(2,ii,jj) = 0.0
    U(3,ii,jj) = 0.0
    U(4,ii,jj) = 0.0
    U(5,ii,jj) = 1.0
    IF (r .LT. 0.3) THEN
      U(2,ii,jj) = -1.0E-03*x(2)/r
      U(3,ii,jj) = +1.0E-03*x(1)/r
      U(4,ii,jj) = 0.0
    END IF
  END DO
END DO

!--------------------------------------------------------------------------------------------------!
filename = "Problem_Solution.h5"
!--------------------------------------------------------------------------------------------------!
#ifdef MPI
mycomm = MPI_COMM_WORLD%MPI_VAL
CALL HDF5_OpenFile(file_id,filename,status="NEW",action="WRITE",comm=mycomm)
#else
CALL HDF5_OpenFile(file_id,filename,status="NEW",action="WRITE")
#endif
!--------------------------------------------------------------------------------------------------!
! ! ! CALL HDF5_CreateDataSet(file_id,"SimulationTime",(/1/),dset_type="REAL")
CALL HDF5_WriteDataSet(file_id,"CoordinateX",MeshNodes(1,:,1))
CALL HDF5_WriteDataSet(file_id,"CoordinateY",MeshNodes(2,1,:))
CALL HDF5_WriteDataSet(file_id,"Mesh",MeshNodes(:,:,:))
#ifdef MPI
dimsf(1) = nVar
dimsf(2) = nProcs*nElemsX
dimsf(3) = nElemsY
offset(1) = 0
offset(2) = iRank*nElemsX
offset(3) = 0
CALL HDF5_WriteDataSet(file_id,"Solution",U(:,:,:),dimsf,offset)
#else
CALL HDF5_WriteDataSet(file_id,"Solution",U(:,:,:))
#endif
CALL HDF5_WriteAttribute(file_id,"","VarNames",VarNames)
CALL HDF5_WriteAttribute(file_id,"","REAL",1.23456789)
CALL HDF5_WriteAttribute(file_id,"","INTEGER",1234)
CALL HDF5_WriteAttribute(file_id,"","CHARACTER","BashAttribute")
CALL HDF5_ReadAttribute(file_id,"","CHARACTER",StringVariable)

CALL HDF5_CreateGroup(file_id,"FVSolution")
CALL HDF5_CreateGroup(file_id,"LocalData")
CALL HDF5_OpenGroup(file_id,"LocalData",mygroup_id)
CALL HDF5_GetRank(file_id,"Solution",myrank)
CALL HDF5_GetDims(file_id,"Solution",mydims)
CALL HDF5_CloseGroup(mygroup_id)

CALL HDF5_CloseFile(file_id)

CALL HDF5_OpenFile(file_id,filename,status="OLD",action="READ")
CALL HDF5_ReadDataSet(file_id,"Solution",Uin(:,:,:))
CALL HDF5_ReadAttribute(file_id,"","VarNames",VarNames2)
CALL HDF5_CloseFile(file_id)

#ifdef MPI
CALL MPI_FINALIZE(&
  ierror = iError)
#endif

!==================================================================================================!
END PROGRAM HDF5_TUTORIAL
!--------------------------------------------------------------------------------------------------!
