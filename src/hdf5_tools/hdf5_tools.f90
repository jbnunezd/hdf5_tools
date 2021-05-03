!==================================================================================================!
MODULE MOD_HDF5_Tools
!--------------------------------------------------------------------------------------------------!
#ifdef MPI
USE MPI_F08
#endif
!--------------------------------------------------------------------------------------------------!
! HDF5 uses MPI integers (as in module MPI) instead of MPI handles (as in module MPI_F08)
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
PRIVATE
!--------------------------------------------------------------------------------------------------!
INTERFACE HDF5_OpenFile
  MODULE PROCEDURE HDF5_OpenFile
END INTERFACE

INTERFACE HDF5_CloseFile
  MODULE PROCEDURE HDF5_CloseFile
END INTERFACE

INTERFACE HDF5_CreateDataSet
  MODULE PROCEDURE HDF5_CreateDataSet
END INTERFACE

INTERFACE HDF5_WriteDataSet
  MODULE PROCEDURE HDF5_WriteDataSet_REAL_rank0
  MODULE PROCEDURE HDF5_WriteDataSet_REAL_rank1
  MODULE PROCEDURE HDF5_WriteDataSet_REAL_rank2
  MODULE PROCEDURE HDF5_WriteDataSet_REAL_rank3
  MODULE PROCEDURE HDF5_WriteDataSet_REAL_rank4
  MODULE PROCEDURE HDF5_WriteDataSet_REAL_rank5
  MODULE PROCEDURE HDF5_WriteDataSet_REAL_rank6
  MODULE PROCEDURE HDF5_WriteDataSet_REAL_rank7
  MODULE PROCEDURE HDF5_WriteDataSet_INTEGER_rank0
  MODULE PROCEDURE HDF5_WriteDataSet_INTEGER_rank1
  MODULE PROCEDURE HDF5_WriteDataSet_INTEGER_rank2
  MODULE PROCEDURE HDF5_WriteDataSet_INTEGER_rank3
  MODULE PROCEDURE HDF5_WriteDataSet_INTEGER_rank4
  MODULE PROCEDURE HDF5_WriteDataSet_INTEGER_rank5
  MODULE PROCEDURE HDF5_WriteDataSet_INTEGER_rank6
  MODULE PROCEDURE HDF5_WriteDataSet_INTEGER_rank7
END INTERFACE

INTERFACE HDF5_ReadDataSet
  MODULE PROCEDURE HDF5_ReadDataSet_REAL_rank0
  MODULE PROCEDURE HDF5_ReadDataSet_REAL_rank1
  MODULE PROCEDURE HDF5_ReadDataSet_REAL_rank2
  MODULE PROCEDURE HDF5_ReadDataSet_REAL_rank3
  MODULE PROCEDURE HDF5_ReadDataSet_REAL_rank4
  MODULE PROCEDURE HDF5_ReadDataSet_REAL_rank5
  MODULE PROCEDURE HDF5_ReadDataSet_REAL_rank6
  MODULE PROCEDURE HDF5_ReadDataSet_REAL_rank7
  MODULE PROCEDURE HDF5_ReadDataSet_INTEGER_rank0
  MODULE PROCEDURE HDF5_ReadDataSet_INTEGER_rank1
  MODULE PROCEDURE HDF5_ReadDataSet_INTEGER_rank2
  MODULE PROCEDURE HDF5_ReadDataSet_INTEGER_rank3
  MODULE PROCEDURE HDF5_ReadDataSet_INTEGER_rank4
  MODULE PROCEDURE HDF5_ReadDataSet_INTEGER_rank5
  MODULE PROCEDURE HDF5_ReadDataSet_INTEGER_rank6
  MODULE PROCEDURE HDF5_ReadDataSet_INTEGER_rank7
END INTERFACE

INTERFACE HDF5_WriteAttribute
  MODULE PROCEDURE HDF5_WriteAttribute_REAL_rank0
  MODULE PROCEDURE HDF5_WriteAttribute_REAL_rank1
  MODULE PROCEDURE HDF5_WriteAttribute_INTEGER_rank0
  MODULE PROCEDURE HDF5_WriteAttribute_INTEGER_rank1  
  MODULE PROCEDURE HDF5_WriteAttribute_CHARACTER_rank0
  MODULE PROCEDURE HDF5_WriteAttribute_CHARACTER_rank1
END INTERFACE

INTERFACE HDF5_ReadAttribute
  MODULE PROCEDURE HDF5_ReadAttribute_REAL_rank0
  MODULE PROCEDURE HDF5_ReadAttribute_REAL_rank1
  MODULE PROCEDURE HDF5_ReadAttribute_INTEGER_rank0
  MODULE PROCEDURE HDF5_ReadAttribute_INTEGER_rank1
  MODULE PROCEDURE HDF5_ReadAttribute_CHARACTER_rank0
  MODULE PROCEDURE HDF5_ReadAttribute_CHARACTER_rank1
END INTERFACE

INTERFACE HDF5_CHECKPATHS
  MODULE PROCEDURE HDF5_CHECKPATHS
END INTERFACE

INTERFACE HDF5_CreateGroup
  MODULE PROCEDURE HDF5_CreateGroup
END INTERFACE

INTERFACE HDF5_OpenGroup
  MODULE PROCEDURE HDF5_OpenGroup
END INTERFACE

INTERFACE HDF5_CloseGroup
  MODULE PROCEDURE HDF5_CloseGroup
END INTERFACE

INTERFACE HDF5_GetRank
  MODULE PROCEDURE HDF5_GetRank
END INTERFACE

INTERFACE HDF5_GetDims
  MODULE PROCEDURE HDF5_GetDims
END INTERFACE
!--------------------------------------------------------------------------------------------------!
PUBLIC :: HDF5_OpenFile
PUBLIC :: HDF5_CloseFile
PUBLIC :: HDF5_CreateDataSet
PUBLIC :: HDF5_WriteDataSet
PUBLIC :: HDF5_ReadDataSet
PUBLIC :: HDF5_WriteAttribute
PUBLIC :: HDF5_ReadAttribute
PUBLIC :: HDF5_CHECKPATHS
PUBLIC :: HDF5_CreateGroup
PUBLIC :: HDF5_OpenGroup
PUBLIC :: HDF5_CloseGroup
PUBLIC :: HDF5_GetRank
PUBLIC :: HDF5_GetDims
!--------------------------------------------------------------------------------------------------!
!
!
!
!==================================================================================================!
CONTAINS
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_OpenFile(file_id,filename,status,action,comm)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(OUT)  :: file_id
CHARACTER(LEN=*),INTENT(IN) :: filename
CHARACTER(LEN=*),INTENT(IN) :: status
CHARACTER(LEN=*),INTENT(IN) :: action
INTEGER,INTENT(IN),OPTIONAL :: comm
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T)     :: plist_id
INTEGER            :: STAT_FILE
INTEGER            :: UNIT_FILE
INTEGER            :: iError
!--------------------------------------------------------------------------------------------------!

CALL H5OPEN_F(iError)

CALL H5PCREATE_F(H5P_FILE_ACCESS_F,plist_id,iError)
IF (PRESENT(comm)) THEN
#ifdef MPI
  CALL H5PSET_FAPL_MPIO_F(plist_id,comm,MPI_INFO_NULL%MPI_VAL,iError)
#endif
END IF

IF (STATUS .EQ. "OLD") THEN
  IF (ACTION .EQ. "READ") THEN
    CALL H5FOPEN_F(TRIM(filename),H5F_ACC_RDONLY_F,file_id,iError,access_prp=plist_id)
  ELSEIF ((ACTION .EQ. "WRITE") .OR. (ACTION .EQ. "READWRITE")) THEN
    CALL H5FOPEN_F(TRIM(filename),H5F_ACC_RDWR_F,file_id,iError,access_prp=plist_id)
  ELSE
    WRITE(*,*) "HDF5_OpenFile: ACTION = ", TRIM(ACTION), " not supported."
    STOP
  END IF
ELSEIF (STATUS .EQ. "NEW") THEN
  IF (ACTION .EQ. "READ") THEN
    WRITE(*,*) "HDF5_OpenFile: STATUS = ", TRIM(STATUS), &
    " combinated with ACTION = ", TRIM(ACTION), " not supported."
    STOP
  ELSEIF ((ACTION .EQ. "WRITE") .OR. (ACTION .EQ. "READWRITE")) THEN
    CALL H5FCREATE_F(TRIM(filename),H5F_ACC_TRUNC_F,file_id,iError,access_prp=plist_id)
  ELSE
    WRITE(*,*) "HDF5_OpenFile: ACTION = ", TRIM(ACTION), " not supported."
    STOP
  END IF
ELSEIF (STATUS .EQ. "REPLACE") THEN
  STAT_FILE = 0
  OPEN(NEWUNIT = UNIT_FILE,   &
       FILE    = filename,    &
       STATUS  = "OLD",       &
       ACTION  = "WRITE",     &
       ACCESS  = "SEQUENTIAL",&
       IOSTAT  = STAT_FILE)
  IF (STAT_FILE .EQ. 0) THEN 
    CLOSE (UNIT_FILE,STATUS="DELETE")
  END IF
  CALL H5FCREATE_F(TRIM(filename),H5F_ACC_EXCL_F,file_id,iError,access_prp=plist_id)
ELSE
  WRITE(*,*) "HDF5_OpenFile: STATUS = ", TRIM(STATUS), " not supported."
  STOP
END IF
CALL H5PCLOSE_F(plist_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_OpenFile
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_CloseFile(file_id)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN) :: file_id
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER                   :: iError
!--------------------------------------------------------------------------------------------------!

! Close File
CALL H5FCLOSE_F(file_id,iError)
CALL H5CLOSE_F(iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_CloseFile
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_CreateDataSet(loc_id,dset_name,dset_dims,dset_type)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)   :: loc_id
CHARACTER(LEN=*),INTENT(IN) :: dset_name
INTEGER,INTENT(IN)          :: dset_dims(:)
CHARACTER(LEN=*),INTENT(IN) :: dset_type
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: dspace_id
INTEGER(HID_T)               :: dset_id
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = SIZE(dset_dims,1)
ALLOCATE(dims(1:dset_rank))
dims(1:dset_rank) = INT(dset_dims,SIZE_T)

! Create Dataspace
CALL H5SCREATE_SIMPLE_F(dset_rank,dims,dspace_id,iError)

! Create Datasets
IF (dset_type .EQ. "INTEGER") THEN
  CALL H5DCREATE_F(loc_id,TRIM(dset_name),H5T_NATIVE_INTEGER,dspace_id,dset_id,iError)
ELSEIF (dset_type .EQ. "REAL") THEN
  CALL H5DCREATE_F(loc_id,TRIM(dset_name),H5T_NATIVE_DOUBLE,dspace_id,dset_id,iError)
ELSE
  WRITE(*,*) "HDF5_CreateDataSet: DSET_TYPE = ", TRIM(dset_type), " not supported."
  STOP
END IF

! Close Dataset/Dataspace
CALL H5DCLOSE_F(dset_id,iError)
CALL H5SCLOSE_F(dspace_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_CreateDataSet
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_WriteDataSet_REAL_rank0(loc_id,dset_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
REAL,INTENT(IN)              :: data
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: dspace_id
INTEGER(HID_T)               :: dset_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = 1
ALLOCATE(dims(1:dset_rank))
dims = (/0/)

! Create Dataspace
CALL H5SCREATE_F(H5S_SCALAR_F,dspace_id,iError)

! Create Dataset
CALL H5DCREATE_F(loc_id,TRIM(dset_name),H5T_NATIVE_DOUBLE,dspace_id,dset_id,iError)
CALL H5DWRITE_F(dset_id,H5T_NATIVE_DOUBLE,data,dims,iError)

! Close Dataset/Dataspace
CALL H5DCLOSE_F(dset_id,iError)
CALL H5SCLOSE_F(dspace_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_WriteDataSet_REAL_rank0
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_WriteDataSet_REAL_rank1(loc_id,dset_name,data,dimsf,offset)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)            :: loc_id
CHARACTER(LEN=*),INTENT(IN)          :: dset_name
REAL,INTENT(IN)                      :: data(:)
INTEGER(HSIZE_T),INTENT(IN),OPTIONAL :: dimsf(RANK(data))
INTEGER(HSIZE_T),INTENT(IN),OPTIONAL :: offset(RANK(data))
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: fdspace_id
INTEGER(HID_T)               :: dspace_id
INTEGER(HID_T)               :: dset_id
INTEGER(HSIZE_T)             :: dimsf_array(RANK(data))
INTEGER(HSIZE_T)             :: offset_rank(RANK(data))
INTEGER(HID_T)               :: plist_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

IF (PRESENT(dimsf)) THEN
  dimsf_array = dimsf
ELSE
  dimsf_array = dims
END IF
IF (PRESENT(offset)) THEN
  offset_rank = offset
ELSE
  offset_rank = 0
END IF

! Create Dataspace
CALL H5SCREATE_SIMPLE_F(dset_rank,dimsf_array,fdspace_id,iError)

! Create Dataset
CALL H5DCREATE_F(loc_id,TRIM(dset_name),H5T_NATIVE_DOUBLE,fdspace_id,dset_id,iError)

! Create Dataspace for current MPI process
CALL H5SCREATE_SIMPLE_F(dset_rank,dims,dspace_id,iError)
! Select hyperslab
CALL H5DGET_SPACE_F(dset_id,fdspace_id,iError)
CALL H5SSELECT_HYPERSLAB_F(fdspace_id,H5S_SELECT_SET_F,offset_rank,dims,iError)

! Create Properties List
CALL H5PCREATE_F(H5P_DATASET_XFER_F,plist_id,iError)
#ifdef MPI
CALL H5PSET_DXPL_MPIO_F(plist_id,H5FD_MPIO_COLLECTIVE_F,iError)
#endif
! Write Dataset
CALL H5DWRITE_F(dset_id,H5T_NATIVE_DOUBLE,data,dims,iError,&
                file_space_id=fdspace_id,mem_space_id=dspace_id,xfer_prp=plist_id)

! Close Dataset/Dataspace/PropertyList
CALL H5PCLOSE_F(plist_id,iError)
CALL H5DCLOSE_F(dset_id,iError)
CALL H5SCLOSE_F(dspace_id,iError)
CALL H5SCLOSE_F(fdspace_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_WriteDataSet_REAL_rank1
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_WriteDataSet_REAL_rank2(loc_id,dset_name,data,dimsf,offset)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)            :: loc_id
CHARACTER(LEN=*),INTENT(IN)          :: dset_name
REAL,INTENT(IN)                      :: data(:,:)
INTEGER(HSIZE_T),INTENT(IN),OPTIONAL :: dimsf(RANK(data))
INTEGER(HSIZE_T),INTENT(IN),OPTIONAL :: offset(RANK(data))
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: fdspace_id
INTEGER(HID_T)               :: dspace_id
INTEGER(HID_T)               :: dset_id
INTEGER(HSIZE_T)             :: dimsf_array(RANK(data))
INTEGER(HSIZE_T)             :: offset_rank(RANK(data))
INTEGER(HID_T)               :: plist_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

IF (PRESENT(dimsf)) THEN
  dimsf_array = dimsf
ELSE
  dimsf_array = dims
END IF
IF (PRESENT(offset)) THEN
  offset_rank = offset
ELSE
  offset_rank = 0
END IF

! Create Dataspace
CALL H5SCREATE_SIMPLE_F(dset_rank,dimsf_array,fdspace_id,iError)

! Create Dataset
CALL H5DCREATE_F(loc_id,TRIM(dset_name),H5T_NATIVE_DOUBLE,fdspace_id,dset_id,iError)

! Create Dataspace for current MPI process
CALL H5SCREATE_SIMPLE_F(dset_rank,dims,dspace_id,iError)
! Select hyperslab
CALL H5DGET_SPACE_F(dset_id,fdspace_id,iError)
CALL H5SSELECT_HYPERSLAB_F(fdspace_id,H5S_SELECT_SET_F,offset_rank,dims,iError)

! Create Properties List
CALL H5PCREATE_F(H5P_DATASET_XFER_F,plist_id,iError)
#ifdef MPI
CALL H5PSET_DXPL_MPIO_F(plist_id,H5FD_MPIO_COLLECTIVE_F,iError)
#endif
! Write Dataset
CALL H5DWRITE_F(dset_id,H5T_NATIVE_DOUBLE,data,dims,iError,&
                file_space_id=fdspace_id,mem_space_id=dspace_id,xfer_prp=plist_id)

! Close Dataset/Dataspace/PropertyList
CALL H5PCLOSE_F(plist_id,iError)
CALL H5DCLOSE_F(dset_id,iError)
CALL H5SCLOSE_F(dspace_id,iError)
CALL H5SCLOSE_F(fdspace_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_WriteDataSet_REAL_rank2
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_WriteDataSet_REAL_rank3(loc_id,dset_name,data,dimsf,offset)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
REAL,INTENT(IN)              :: data(:,:,:)
INTEGER(HSIZE_T),INTENT(IN),OPTIONAL :: dimsf(RANK(data))
INTEGER(HSIZE_T),INTENT(IN),OPTIONAL :: offset(RANK(data))
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: fdspace_id
INTEGER(HID_T)               :: dspace_id
INTEGER(HID_T)               :: dset_id
INTEGER(HID_T)               :: plist_id
INTEGER(HSIZE_T)             :: dimsf_array(RANK(data))
INTEGER(HSIZE_T)             :: offset_rank(RANK(data))
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

IF (PRESENT(dimsf)) THEN
  dimsf_array = dimsf
ELSE
  dimsf_array = dims
END IF
IF (PRESENT(offset)) THEN
  offset_rank = offset
ELSE
  offset_rank = 0
END IF

! Create Dataspace
CALL H5SCREATE_SIMPLE_F(dset_rank,dimsf_array,fdspace_id,iError)

! Create Dataset
CALL H5DCREATE_F(loc_id,TRIM(dset_name),H5T_NATIVE_DOUBLE,fdspace_id,dset_id,iError)

! Create Dataspace for current MPI process
CALL H5SCREATE_SIMPLE_F(dset_rank,dims,dspace_id,iError)
! Select hyperslab
CALL H5DGET_SPACE_F(dset_id,fdspace_id,iError)
CALL H5SSELECT_HYPERSLAB_F(fdspace_id,H5S_SELECT_SET_F,offset_rank,dims,iError)

! Create Properties List
CALL H5PCREATE_F(H5P_DATASET_XFER_F,plist_id,iError)
#ifdef MPI
CALL H5PSET_DXPL_MPIO_F(plist_id,H5FD_MPIO_COLLECTIVE_F,iError)
#endif
! Write Dataset
CALL H5DWRITE_F(dset_id,H5T_NATIVE_DOUBLE,data,dims,iError,&
                file_space_id=fdspace_id,mem_space_id=dspace_id,xfer_prp=plist_id)

! Close Dataset/Dataspace/PropertyList
CALL H5PCLOSE_F(plist_id,iError)
CALL H5DCLOSE_F(dset_id,iError)
CALL H5SCLOSE_F(dspace_id,iError)
CALL H5SCLOSE_F(fdspace_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_WriteDataSet_REAL_rank3
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_WriteDataSet_REAL_rank4(loc_id,dset_name,data,dimsf,offset)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
REAL,INTENT(IN)              :: data(:,:,:,:)
INTEGER(HSIZE_T),INTENT(IN),OPTIONAL :: dimsf(RANK(data))
INTEGER(HSIZE_T),INTENT(IN),OPTIONAL :: offset(RANK(data))
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: fdspace_id
INTEGER(HID_T)               :: dspace_id
INTEGER(HID_T)               :: dset_id
INTEGER(HSIZE_T)             :: dimsf_array(RANK(data))
INTEGER(HSIZE_T)             :: offset_rank(RANK(data))
INTEGER(HID_T)               :: plist_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

IF (PRESENT(dimsf)) THEN
  dimsf_array = dimsf
ELSE
  dimsf_array = dims
END IF
IF (PRESENT(offset)) THEN
  offset_rank = offset
ELSE
  offset_rank = 0
END IF

! Create Dataspace
CALL H5SCREATE_SIMPLE_F(dset_rank,dimsf_array,fdspace_id,iError)

! Create Dataset
CALL H5DCREATE_F(loc_id,TRIM(dset_name),H5T_NATIVE_DOUBLE,fdspace_id,dset_id,iError)

! Create Dataspace for current MPI process
CALL H5SCREATE_SIMPLE_F(dset_rank,dims,dspace_id,iError)
! Select hyperslab
CALL H5DGET_SPACE_F(dset_id,fdspace_id,iError)
CALL H5SSELECT_HYPERSLAB_F(fdspace_id,H5S_SELECT_SET_F,offset_rank,dims,iError)

! Create Properties List
CALL H5PCREATE_F(H5P_DATASET_XFER_F,plist_id,iError)
#ifdef MPI
CALL H5PSET_DXPL_MPIO_F(plist_id,H5FD_MPIO_COLLECTIVE_F,iError)
#endif
! Write Dataset
CALL H5DWRITE_F(dset_id,H5T_NATIVE_DOUBLE,data,dims,iError,&
                file_space_id=fdspace_id,mem_space_id=dspace_id,xfer_prp=plist_id)

! Close Dataset/Dataspace/PropertyList
CALL H5PCLOSE_F(plist_id,iError)
CALL H5DCLOSE_F(dset_id,iError)
CALL H5SCLOSE_F(dspace_id,iError)
CALL H5SCLOSE_F(fdspace_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_WriteDataSet_REAL_rank4
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_WriteDataSet_REAL_rank5(loc_id,dset_name,data,dimsf,offset)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
REAL,INTENT(IN)              :: data(:,:,:,:,:)
INTEGER(HSIZE_T),INTENT(IN),OPTIONAL :: dimsf(RANK(data))
INTEGER(HSIZE_T),INTENT(IN),OPTIONAL :: offset(RANK(data))
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: fdspace_id
INTEGER(HID_T)               :: dspace_id
INTEGER(HID_T)               :: dset_id
INTEGER(HSIZE_T)             :: dimsf_array(RANK(data))
INTEGER(HSIZE_T)             :: offset_rank(RANK(data))
INTEGER(HID_T)               :: plist_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

IF (PRESENT(dimsf)) THEN
  dimsf_array = dimsf
ELSE
  dimsf_array = dims
END IF
IF (PRESENT(offset)) THEN
  offset_rank = offset
ELSE
  offset_rank = 0
END IF

! Create Dataspace
CALL H5SCREATE_SIMPLE_F(dset_rank,dimsf_array,fdspace_id,iError)

! Create Dataset
CALL H5DCREATE_F(loc_id,TRIM(dset_name),H5T_NATIVE_DOUBLE,fdspace_id,dset_id,iError)

! Create Dataspace for current MPI process
CALL H5SCREATE_SIMPLE_F(dset_rank,dims,dspace_id,iError)
! Select hyperslab
CALL H5DGET_SPACE_F(dset_id,fdspace_id,iError)
CALL H5SSELECT_HYPERSLAB_F(fdspace_id,H5S_SELECT_SET_F,offset_rank,dims,iError)

! Create Properties List
CALL H5PCREATE_F(H5P_DATASET_XFER_F,plist_id,iError)
#ifdef MPI
CALL H5PSET_DXPL_MPIO_F(plist_id,H5FD_MPIO_COLLECTIVE_F,iError)
#endif
! Write Dataset
CALL H5DWRITE_F(dset_id,H5T_NATIVE_DOUBLE,data,dims,iError,&
                file_space_id=fdspace_id,mem_space_id=dspace_id,xfer_prp=plist_id)

! Close Dataset/Dataspace/PropertyList
CALL H5PCLOSE_F(plist_id,iError)
CALL H5DCLOSE_F(dset_id,iError)
CALL H5SCLOSE_F(dspace_id,iError)
CALL H5SCLOSE_F(fdspace_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_WriteDataSet_REAL_rank5
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_WriteDataSet_REAL_rank6(loc_id,dset_name,data,dimsf,offset)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
REAL,INTENT(IN)              :: data(:,:,:,:,:,:)
INTEGER(HSIZE_T),INTENT(IN),OPTIONAL :: dimsf(RANK(data))
INTEGER(HSIZE_T),INTENT(IN),OPTIONAL :: offset(RANK(data))
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: fdspace_id
INTEGER(HID_T)               :: dspace_id
INTEGER(HID_T)               :: dset_id
INTEGER(HSIZE_T)             :: dimsf_array(RANK(data))
INTEGER(HSIZE_T)             :: offset_rank(RANK(data))
INTEGER(HID_T)               :: plist_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

IF (PRESENT(dimsf)) THEN
  dimsf_array = dimsf
ELSE
  dimsf_array = dims
END IF
IF (PRESENT(offset)) THEN
  offset_rank = offset
ELSE
  offset_rank = 0
END IF

! Create Dataspace
CALL H5SCREATE_SIMPLE_F(dset_rank,dimsf_array,fdspace_id,iError)

! Create Dataset
CALL H5DCREATE_F(loc_id,TRIM(dset_name),H5T_NATIVE_DOUBLE,fdspace_id,dset_id,iError)

! Create Dataspace for current MPI process
CALL H5SCREATE_SIMPLE_F(dset_rank,dims,dspace_id,iError)
! Select hyperslab
CALL H5DGET_SPACE_F(dset_id,fdspace_id,iError)
CALL H5SSELECT_HYPERSLAB_F(fdspace_id,H5S_SELECT_SET_F,offset_rank,dims,iError)

! Create Properties List
CALL H5PCREATE_F(H5P_DATASET_XFER_F,plist_id,iError)
#ifdef MPI
CALL H5PSET_DXPL_MPIO_F(plist_id,H5FD_MPIO_COLLECTIVE_F,iError)
#endif
! Write Dataset
CALL H5DWRITE_F(dset_id,H5T_NATIVE_DOUBLE,data,dims,iError,&
                file_space_id=fdspace_id,mem_space_id=dspace_id,xfer_prp=plist_id)

! Close Dataset/Dataspace/PropertyList
CALL H5PCLOSE_F(plist_id,iError)
CALL H5DCLOSE_F(dset_id,iError)
CALL H5SCLOSE_F(dspace_id,iError)
CALL H5SCLOSE_F(fdspace_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_WriteDataSet_REAL_rank6
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_WriteDataSet_REAL_rank7(loc_id,dset_name,data,dimsf,offset)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
REAL,INTENT(IN)              :: data(:,:,:,:,:,:,:)
INTEGER(HSIZE_T),INTENT(IN),OPTIONAL :: dimsf(RANK(data))
INTEGER(HSIZE_T),INTENT(IN),OPTIONAL :: offset(RANK(data))
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: fdspace_id
INTEGER(HID_T)               :: dspace_id
INTEGER(HID_T)               :: dset_id
INTEGER(HSIZE_T)             :: dimsf_array(RANK(data))
INTEGER(HSIZE_T)             :: offset_rank(RANK(data))
INTEGER(HID_T)               :: plist_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

IF (PRESENT(dimsf)) THEN
  dimsf_array = dimsf
ELSE
  dimsf_array = dims
END IF
IF (PRESENT(offset)) THEN
  offset_rank = offset
ELSE
  offset_rank = 0
END IF

! Create Dataspace
CALL H5SCREATE_SIMPLE_F(dset_rank,dimsf_array,fdspace_id,iError)

! Create Dataset
CALL H5DCREATE_F(loc_id,TRIM(dset_name),H5T_NATIVE_DOUBLE,fdspace_id,dset_id,iError)

! Create Dataspace for current MPI process
CALL H5SCREATE_SIMPLE_F(dset_rank,dims,dspace_id,iError)
! Select hyperslab
CALL H5DGET_SPACE_F(dset_id,fdspace_id,iError)
CALL H5SSELECT_HYPERSLAB_F(fdspace_id,H5S_SELECT_SET_F,offset_rank,dims,iError)

! Create Properties List
CALL H5PCREATE_F(H5P_DATASET_XFER_F,plist_id,iError)
#ifdef MPI
CALL H5PSET_DXPL_MPIO_F(plist_id,H5FD_MPIO_COLLECTIVE_F,iError)
#endif
! Write Dataset
CALL H5DWRITE_F(dset_id,H5T_NATIVE_DOUBLE,data,dims,iError,&
                file_space_id=fdspace_id,mem_space_id=dspace_id,xfer_prp=plist_id)

! Close Dataset/Dataspace/PropertyList
CALL H5PCLOSE_F(plist_id,iError)
CALL H5DCLOSE_F(dset_id,iError)
CALL H5SCLOSE_F(dspace_id,iError)
CALL H5SCLOSE_F(fdspace_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_WriteDataSet_REAL_rank7
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_WriteDataSet_INTEGER_rank0(loc_id,dset_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
INTEGER,INTENT(IN)           :: data
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: dspace_id
INTEGER(HID_T)               :: dset_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = 1
ALLOCATE(dims(1:dset_rank))
dims = (/0/)

! Create Dataspace
CALL H5SCREATE_F(H5S_SCALAR_F,dspace_id,iError)

! Create Dataset
CALL H5DCREATE_F(loc_id,TRIM(dset_name),H5T_NATIVE_INTEGER,dspace_id,dset_id,iError)
CALL H5DWRITE_F(dset_id,H5T_NATIVE_INTEGER,data,dims,iError)

! Close Dataset/Dataspace
CALL H5DCLOSE_F(dset_id,iError)
CALL H5SCLOSE_F(dspace_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_WriteDataSet_INTEGER_rank0
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_WriteDataSet_INTEGER_rank1(loc_id,dset_name,data,dimsf,offset)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
INTEGER,INTENT(IN)           :: data(:)
INTEGER(HSIZE_T),INTENT(IN),OPTIONAL :: dimsf(RANK(data))
INTEGER(HSIZE_T),INTENT(IN),OPTIONAL :: offset(RANK(data))
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: fdspace_id
INTEGER(HID_T)               :: dspace_id
INTEGER(HID_T)               :: dset_id
INTEGER(HSIZE_T)             :: dimsf_array(RANK(data))
INTEGER(HSIZE_T)             :: offset_rank(RANK(data))
INTEGER(HID_T)               :: plist_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

IF (PRESENT(dimsf)) THEN
  dimsf_array = dimsf
ELSE
  dimsf_array = dims
END IF
IF (PRESENT(offset)) THEN
  offset_rank = offset
ELSE
  offset_rank = 0
END IF

! Create Dataspace
CALL H5SCREATE_SIMPLE_F(dset_rank,dimsf_array,fdspace_id,iError)

! Create Dataset
CALL H5DCREATE_F(loc_id,TRIM(dset_name),H5T_NATIVE_INTEGER,fdspace_id,dset_id,iError)

! Create Dataspace for current MPI process
CALL H5SCREATE_SIMPLE_F(dset_rank,dims,dspace_id,iError)
! Select hyperslab
CALL H5DGET_SPACE_F(dset_id,fdspace_id,iError)
CALL H5SSELECT_HYPERSLAB_F(fdspace_id,H5S_SELECT_SET_F,offset_rank,dims,iError)

! Create Properties List
CALL H5PCREATE_F(H5P_DATASET_XFER_F,plist_id,iError)
#ifdef MPI
CALL H5PSET_DXPL_MPIO_F(plist_id,H5FD_MPIO_COLLECTIVE_F,iError)
#endif
! Write Dataset
CALL H5DWRITE_F(dset_id,H5T_NATIVE_INTEGER,data,dims,iError,&
                file_space_id=fdspace_id,mem_space_id=dspace_id,xfer_prp=plist_id)

! Close Dataset/Dataspace/PropertyList
CALL H5PCLOSE_F(plist_id,iError)
CALL H5DCLOSE_F(dset_id,iError)
CALL H5SCLOSE_F(dspace_id,iError)
CALL H5SCLOSE_F(fdspace_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_WriteDataSet_INTEGER_rank1
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_WriteDataSet_INTEGER_rank2(loc_id,dset_name,data,dimsf,offset)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
INTEGER,INTENT(IN)           :: data(:,:)
INTEGER(HSIZE_T),INTENT(IN),OPTIONAL :: dimsf(RANK(data))
INTEGER(HSIZE_T),INTENT(IN),OPTIONAL :: offset(RANK(data))
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: fdspace_id
INTEGER(HID_T)               :: dspace_id
INTEGER(HID_T)               :: dset_id
INTEGER(HSIZE_T)             :: dimsf_array(RANK(data))
INTEGER(HSIZE_T)             :: offset_rank(RANK(data))
INTEGER(HID_T)               :: plist_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

IF (PRESENT(dimsf)) THEN
  dimsf_array = dimsf
ELSE
  dimsf_array = dims
END IF
IF (PRESENT(offset)) THEN
  offset_rank = offset
ELSE
  offset_rank = 0
END IF

! Create Dataspace
CALL H5SCREATE_SIMPLE_F(dset_rank,dimsf_array,fdspace_id,iError)

! Create Dataset
CALL H5DCREATE_F(loc_id,TRIM(dset_name),H5T_NATIVE_INTEGER,fdspace_id,dset_id,iError)

! Create Dataspace for current MPI process
CALL H5SCREATE_SIMPLE_F(dset_rank,dims,dspace_id,iError)
! Select hyperslab
CALL H5DGET_SPACE_F(dset_id,fdspace_id,iError)
CALL H5SSELECT_HYPERSLAB_F(fdspace_id,H5S_SELECT_SET_F,offset_rank,dims,iError)

! Create Properties List
CALL H5PCREATE_F(H5P_DATASET_XFER_F,plist_id,iError)
#ifdef MPI
CALL H5PSET_DXPL_MPIO_F(plist_id,H5FD_MPIO_COLLECTIVE_F,iError)
#endif
! Write Dataset
CALL H5DWRITE_F(dset_id,H5T_NATIVE_INTEGER,data,dims,iError,&
                file_space_id=fdspace_id,mem_space_id=dspace_id,xfer_prp=plist_id)

! Close Dataset/Dataspace/PropertyList
CALL H5PCLOSE_F(plist_id,iError)
CALL H5DCLOSE_F(dset_id,iError)
CALL H5SCLOSE_F(dspace_id,iError)
CALL H5SCLOSE_F(fdspace_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_WriteDataSet_INTEGER_rank2
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_WriteDataSet_INTEGER_rank3(loc_id,dset_name,data,dimsf,offset)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
INTEGER,INTENT(IN)           :: data(:,:,:)
INTEGER(HSIZE_T),INTENT(IN),OPTIONAL :: dimsf(RANK(data))
INTEGER(HSIZE_T),INTENT(IN),OPTIONAL :: offset(RANK(data))
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: fdspace_id
INTEGER(HID_T)               :: dspace_id
INTEGER(HID_T)               :: dset_id
INTEGER(HSIZE_T)             :: dimsf_array(RANK(data))
INTEGER(HSIZE_T)             :: offset_rank(RANK(data))
INTEGER(HID_T)               :: plist_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

IF (PRESENT(dimsf)) THEN
  dimsf_array = dimsf
ELSE
  dimsf_array = dims
END IF
IF (PRESENT(offset)) THEN
  offset_rank = offset
ELSE
  offset_rank = 0
END IF

! Create Dataspace
CALL H5SCREATE_SIMPLE_F(dset_rank,dimsf_array,fdspace_id,iError)

! Create Dataset
CALL H5DCREATE_F(loc_id,TRIM(dset_name),H5T_NATIVE_INTEGER,fdspace_id,dset_id,iError)

! Create Dataspace for current MPI process
CALL H5SCREATE_SIMPLE_F(dset_rank,dims,dspace_id,iError)
! Select hyperslab
CALL H5DGET_SPACE_F(dset_id,fdspace_id,iError)
CALL H5SSELECT_HYPERSLAB_F(fdspace_id,H5S_SELECT_SET_F,offset_rank,dims,iError)

! Create Properties List
CALL H5PCREATE_F(H5P_DATASET_XFER_F,plist_id,iError)
#ifdef MPI
CALL H5PSET_DXPL_MPIO_F(plist_id,H5FD_MPIO_COLLECTIVE_F,iError)
#endif
! Write Dataset
CALL H5DWRITE_F(dset_id,H5T_NATIVE_INTEGER,data,dims,iError,&
                file_space_id=fdspace_id,mem_space_id=dspace_id,xfer_prp=plist_id)

! Close Dataset/Dataspace/PropertyList
CALL H5PCLOSE_F(plist_id,iError)
CALL H5DCLOSE_F(dset_id,iError)
CALL H5SCLOSE_F(dspace_id,iError)
CALL H5SCLOSE_F(fdspace_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_WriteDataSet_INTEGER_rank3
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_WriteDataSet_INTEGER_rank4(loc_id,dset_name,data,dimsf,offset)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
INTEGER,INTENT(IN)           :: data(:,:,:,:)
INTEGER(HSIZE_T),INTENT(IN),OPTIONAL :: dimsf(RANK(data))
INTEGER(HSIZE_T),INTENT(IN),OPTIONAL :: offset(RANK(data))
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: fdspace_id
INTEGER(HID_T)               :: dspace_id
INTEGER(HID_T)               :: dset_id
INTEGER(HSIZE_T)             :: dimsf_array(RANK(data))
INTEGER(HSIZE_T)             :: offset_rank(RANK(data))
INTEGER(HID_T)               :: plist_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

IF (PRESENT(dimsf)) THEN
  dimsf_array = dimsf
ELSE
  dimsf_array = dims
END IF
IF (PRESENT(offset)) THEN
  offset_rank = offset
ELSE
  offset_rank = 0
END IF

! Create Dataspace
CALL H5SCREATE_SIMPLE_F(dset_rank,dimsf_array,fdspace_id,iError)

! Create Dataset
CALL H5DCREATE_F(loc_id,TRIM(dset_name),H5T_NATIVE_INTEGER,fdspace_id,dset_id,iError)

! Create Dataspace for current MPI process
CALL H5SCREATE_SIMPLE_F(dset_rank,dims,dspace_id,iError)
! Select hyperslab
CALL H5DGET_SPACE_F(dset_id,fdspace_id,iError)
CALL H5SSELECT_HYPERSLAB_F(fdspace_id,H5S_SELECT_SET_F,offset_rank,dims,iError)

! Create Properties List
CALL H5PCREATE_F(H5P_DATASET_XFER_F,plist_id,iError)
#ifdef MPI
CALL H5PSET_DXPL_MPIO_F(plist_id,H5FD_MPIO_COLLECTIVE_F,iError)
#endif
! Write Dataset
CALL H5DWRITE_F(dset_id,H5T_NATIVE_INTEGER,data,dims,iError,&
                file_space_id=fdspace_id,mem_space_id=dspace_id,xfer_prp=plist_id)

! Close Dataset/Dataspace/PropertyList
CALL H5PCLOSE_F(plist_id,iError)
CALL H5DCLOSE_F(dset_id,iError)
CALL H5SCLOSE_F(dspace_id,iError)
CALL H5SCLOSE_F(fdspace_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_WriteDataSet_INTEGER_rank4
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_WriteDataSet_INTEGER_rank5(loc_id,dset_name,data,dimsf,offset)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
INTEGER,INTENT(IN)           :: data(:,:,:,:,:)
INTEGER(HSIZE_T),INTENT(IN),OPTIONAL :: dimsf(RANK(data))
INTEGER(HSIZE_T),INTENT(IN),OPTIONAL :: offset(RANK(data))
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: fdspace_id
INTEGER(HID_T)               :: dspace_id
INTEGER(HID_T)               :: dset_id
INTEGER(HSIZE_T)             :: dimsf_array(RANK(data))
INTEGER(HSIZE_T)             :: offset_rank(RANK(data))
INTEGER(HID_T)               :: plist_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

IF (PRESENT(dimsf)) THEN
  dimsf_array = dimsf
ELSE
  dimsf_array = dims
END IF
IF (PRESENT(offset)) THEN
  offset_rank = offset
ELSE
  offset_rank = 0
END IF

! Create Dataspace
CALL H5SCREATE_SIMPLE_F(dset_rank,dimsf_array,fdspace_id,iError)

! Create Dataset
CALL H5DCREATE_F(loc_id,TRIM(dset_name),H5T_NATIVE_INTEGER,fdspace_id,dset_id,iError)

! Create Dataspace for current MPI process
CALL H5SCREATE_SIMPLE_F(dset_rank,dims,dspace_id,iError)
! Select hyperslab
CALL H5DGET_SPACE_F(dset_id,fdspace_id,iError)
CALL H5SSELECT_HYPERSLAB_F(fdspace_id,H5S_SELECT_SET_F,offset_rank,dims,iError)

! Create Properties List
CALL H5PCREATE_F(H5P_DATASET_XFER_F,plist_id,iError)
#ifdef MPI
CALL H5PSET_DXPL_MPIO_F(plist_id,H5FD_MPIO_COLLECTIVE_F,iError)
#endif
! Write Dataset
CALL H5DWRITE_F(dset_id,H5T_NATIVE_INTEGER,data,dims,iError,&
                file_space_id=fdspace_id,mem_space_id=dspace_id,xfer_prp=plist_id)

! Close Dataset/Dataspace/PropertyList
CALL H5PCLOSE_F(plist_id,iError)
CALL H5DCLOSE_F(dset_id,iError)
CALL H5SCLOSE_F(dspace_id,iError)
CALL H5SCLOSE_F(fdspace_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_WriteDataSet_INTEGER_rank5
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_WriteDataSet_INTEGER_rank6(loc_id,dset_name,data,dimsf,offset)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
INTEGER,INTENT(IN)           :: data(:,:,:,:,:,:)
INTEGER(HSIZE_T),INTENT(IN),OPTIONAL :: dimsf(RANK(data))
INTEGER(HSIZE_T),INTENT(IN),OPTIONAL :: offset(RANK(data))
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: fdspace_id
INTEGER(HID_T)               :: dspace_id
INTEGER(HID_T)               :: dset_id
INTEGER(HSIZE_T)             :: dimsf_array(RANK(data))
INTEGER(HSIZE_T)             :: offset_rank(RANK(data))
INTEGER(HID_T)               :: plist_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

IF (PRESENT(dimsf)) THEN
  dimsf_array = dimsf
ELSE
  dimsf_array = dims
END IF
IF (PRESENT(offset)) THEN
  offset_rank = offset
ELSE
  offset_rank = 0
END IF

! Create Dataspace
CALL H5SCREATE_SIMPLE_F(dset_rank,dimsf_array,fdspace_id,iError)

! Create Dataset
CALL H5DCREATE_F(loc_id,TRIM(dset_name),H5T_NATIVE_INTEGER,fdspace_id,dset_id,iError)

! Create Dataspace for current MPI process
CALL H5SCREATE_SIMPLE_F(dset_rank,dims,dspace_id,iError)
! Select hyperslab
CALL H5DGET_SPACE_F(dset_id,fdspace_id,iError)
CALL H5SSELECT_HYPERSLAB_F(fdspace_id,H5S_SELECT_SET_F,offset_rank,dims,iError)

! Create Properties List
CALL H5PCREATE_F(H5P_DATASET_XFER_F,plist_id,iError)
#ifdef MPI
CALL H5PSET_DXPL_MPIO_F(plist_id,H5FD_MPIO_COLLECTIVE_F,iError)
#endif
! Write Dataset
CALL H5DWRITE_F(dset_id,H5T_NATIVE_INTEGER,data,dims,iError,&
                file_space_id=fdspace_id,mem_space_id=dspace_id,xfer_prp=plist_id)

! Close Dataset/Dataspace/PropertyList
CALL H5PCLOSE_F(plist_id,iError)
CALL H5DCLOSE_F(dset_id,iError)
CALL H5SCLOSE_F(dspace_id,iError)
CALL H5SCLOSE_F(fdspace_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_WriteDataSet_INTEGER_rank6
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_WriteDataSet_INTEGER_rank7(loc_id,dset_name,data,dimsf,offset)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
INTEGER,INTENT(IN)           :: data(:,:,:,:,:,:,:)
INTEGER(HSIZE_T),INTENT(IN),OPTIONAL :: dimsf(RANK(data))
INTEGER(HSIZE_T),INTENT(IN),OPTIONAL :: offset(RANK(data))
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: fdspace_id
INTEGER(HID_T)               :: dspace_id
INTEGER(HID_T)               :: dset_id
INTEGER(HSIZE_T)             :: dimsf_array(RANK(data))
INTEGER(HSIZE_T)             :: offset_rank(RANK(data))
INTEGER(HID_T)               :: plist_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

IF (PRESENT(dimsf)) THEN
  dimsf_array = dimsf
ELSE
  dimsf_array = dims
END IF
IF (PRESENT(offset)) THEN
  offset_rank = offset
ELSE
  offset_rank = 0
END IF

! Create Dataspace
CALL H5SCREATE_SIMPLE_F(dset_rank,dimsf_array,fdspace_id,iError)

! Create Dataset
CALL H5DCREATE_F(loc_id,TRIM(dset_name),H5T_NATIVE_INTEGER,fdspace_id,dset_id,iError)

! Create Dataspace for current MPI process
CALL H5SCREATE_SIMPLE_F(dset_rank,dims,dspace_id,iError)
! Select hyperslab
CALL H5DGET_SPACE_F(dset_id,fdspace_id,iError)
CALL H5SSELECT_HYPERSLAB_F(fdspace_id,H5S_SELECT_SET_F,offset_rank,dims,iError)

! Create Properties List
CALL H5PCREATE_F(H5P_DATASET_XFER_F,plist_id,iError)
#ifdef MPI
CALL H5PSET_DXPL_MPIO_F(plist_id,H5FD_MPIO_COLLECTIVE_F,iError)
#endif
! Write Dataset
CALL H5DWRITE_F(dset_id,H5T_NATIVE_INTEGER,data,dims,iError,&
                file_space_id=fdspace_id,mem_space_id=dspace_id,xfer_prp=plist_id)

! Close Dataset/Dataspace/PropertyList
CALL H5PCLOSE_F(plist_id,iError)
CALL H5DCLOSE_F(dset_id,iError)
CALL H5SCLOSE_F(dspace_id,iError)
CALL H5SCLOSE_F(fdspace_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_WriteDataSet_INTEGER_rank7
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_ReadDataSet_REAL_rank0(loc_id,dset_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
REAL,INTENT(OUT)             :: data
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: dset_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = 1
ALLOCATE(dims(1:dset_rank))
dims = (/0/)

! Open Dataset
CALL H5DOPEN_F(loc_id,dset_name,dset_id,iError)

! Read Dataset
CALL H5DREAD_F(dset_id,H5T_NATIVE_DOUBLE,data,dims,iError)

! Close Dataset
CALL H5DCLOSE_F(dset_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_ReadDataSet_REAL_rank0
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_ReadDataSet_REAL_rank1(loc_id,dset_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
REAL,INTENT(OUT)             :: data(:)
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: dset_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

! Open Dataset
CALL H5DOPEN_F(loc_id,dset_name,dset_id,iError)

! Read Dataset
CALL H5DREAD_F(dset_id,H5T_NATIVE_DOUBLE,data,dims,iError)

! Close Dataset
CALL H5DCLOSE_F(dset_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_ReadDataSet_REAL_rank1
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_ReadDataSet_REAL_rank2(loc_id,dset_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
REAL,INTENT(OUT)             :: data(:,:)
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: dset_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

! Open Dataset
CALL H5DOPEN_F(loc_id,dset_name,dset_id,iError)

! Read Dataset
CALL H5DREAD_F(dset_id,H5T_NATIVE_DOUBLE,data,dims,iError)

! Close Dataset
CALL H5DCLOSE_F(dset_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_ReadDataSet_REAL_rank2
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_ReadDataSet_REAL_rank3(loc_id,dset_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
REAL,INTENT(OUT)             :: data(:,:,:)
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: dset_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

! Open Dataset
CALL H5DOPEN_F(loc_id,dset_name,dset_id,iError)

! Read Dataset
CALL H5DREAD_F(dset_id,H5T_NATIVE_DOUBLE,data,dims,iError)

! Close Dataset
CALL H5DCLOSE_F(dset_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_ReadDataSet_REAL_rank3
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_ReadDataSet_REAL_rank4(loc_id,dset_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
REAL,INTENT(OUT)             :: data(:,:,:,:)
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: dset_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

! Open Dataset
CALL H5DOPEN_F(loc_id,dset_name,dset_id,iError)

! Read Dataset
CALL H5DREAD_F(dset_id,H5T_NATIVE_DOUBLE,data,dims,iError)

! Close Dataset
CALL H5DCLOSE_F(dset_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_ReadDataSet_REAL_rank4
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_ReadDataSet_REAL_rank5(loc_id,dset_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
REAL,INTENT(OUT)             :: data(:,:,:,:,:)
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: dset_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

! Open Dataset
CALL H5DOPEN_F(loc_id,dset_name,dset_id,iError)

! Read Dataset
CALL H5DREAD_F(dset_id,H5T_NATIVE_DOUBLE,data,dims,iError)

! Close Dataset
CALL H5DCLOSE_F(dset_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_ReadDataSet_REAL_rank5
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_ReadDataSet_REAL_rank6(loc_id,dset_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
REAL,INTENT(OUT)             :: data(:,:,:,:,:,:)
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: dset_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

! Open Dataset
CALL H5DOPEN_F(loc_id,dset_name,dset_id,iError)

! Read Dataset
CALL H5DREAD_F(dset_id,H5T_NATIVE_DOUBLE,data,dims,iError)

! Close Dataset
CALL H5DCLOSE_F(dset_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_ReadDataSet_REAL_rank6
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_ReadDataSet_REAL_rank7(loc_id,dset_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
REAL,INTENT(OUT)             :: data(:,:,:,:,:,:,:)
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: dset_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

! Open Dataset
CALL H5DOPEN_F(loc_id,dset_name,dset_id,iError)

! Read Dataset
CALL H5DREAD_F(dset_id,H5T_NATIVE_DOUBLE,data,dims,iError)

! Close Dataset
CALL H5DCLOSE_F(dset_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_ReadDataSet_REAL_rank7
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_ReadDataSet_INTEGER_rank0(loc_id,dset_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
INTEGER,INTENT(OUT)          :: data
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: dset_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = 1
ALLOCATE(dims(1:dset_rank))
dims = (/0/)

! Open Dataset
CALL H5DOPEN_F(loc_id,dset_name,dset_id,iError)

! Read Dataset
CALL H5DREAD_F(dset_id,H5T_NATIVE_INTEGER,data,dims,iError)

! Close Dataset
CALL H5DCLOSE_F(dset_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_ReadDataSet_INTEGER_rank0
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_ReadDataSet_INTEGER_rank1(loc_id,dset_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
INTEGER,INTENT(OUT)          :: data(:)
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: dset_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

! Open Dataset
CALL H5DOPEN_F(loc_id,dset_name,dset_id,iError)

! Read Dataset
CALL H5DREAD_F(dset_id,H5T_NATIVE_INTEGER,data,dims,iError)

! Close Dataset
CALL H5DCLOSE_F(dset_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_ReadDataSet_INTEGER_rank1
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_ReadDataSet_INTEGER_rank2(loc_id,dset_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
INTEGER,INTENT(OUT)          :: data(:,:)
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: dset_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

! Open Dataset
CALL H5DOPEN_F(loc_id,dset_name,dset_id,iError)

! Read Dataset
CALL H5DREAD_F(dset_id,H5T_NATIVE_INTEGER,data,dims,iError)

! Close Dataset
CALL H5DCLOSE_F(dset_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_ReadDataSet_INTEGER_rank2
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_ReadDataSet_INTEGER_rank3(loc_id,dset_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
INTEGER,INTENT(OUT)          :: data(:,:,:)
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: dset_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

! Open Dataset
CALL H5DOPEN_F(loc_id,dset_name,dset_id,iError)

! Read Dataset
CALL H5DREAD_F(dset_id,H5T_NATIVE_INTEGER,data,dims,iError)

! Close Dataset
CALL H5DCLOSE_F(dset_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_ReadDataSet_INTEGER_rank3
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_ReadDataSet_INTEGER_rank4(loc_id,dset_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
INTEGER,INTENT(OUT)          :: data(:,:,:,:)
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: dset_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

! Open Dataset
CALL H5DOPEN_F(loc_id,dset_name,dset_id,iError)

! Read Dataset
CALL H5DREAD_F(dset_id,H5T_NATIVE_INTEGER,data,dims,iError)

! Close Dataset
CALL H5DCLOSE_F(dset_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_ReadDataSet_INTEGER_rank4
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_ReadDataSet_INTEGER_rank5(loc_id,dset_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
INTEGER,INTENT(OUT)          :: data(:,:,:,:,:)
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: dset_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

! Open Dataset
CALL H5DOPEN_F(loc_id,dset_name,dset_id,iError)

! Read Dataset
CALL H5DREAD_F(dset_id,H5T_NATIVE_INTEGER,data,dims,iError)

! Close Dataset
CALL H5DCLOSE_F(dset_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_ReadDataSet_INTEGER_rank5
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_ReadDataSet_INTEGER_rank6(loc_id,dset_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
INTEGER,INTENT(OUT)          :: data(:,:,:,:,:,:)
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: dset_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

! Open Dataset
CALL H5DOPEN_F(loc_id,dset_name,dset_id,iError)

! Read Dataset
CALL H5DREAD_F(dset_id,H5T_NATIVE_INTEGER,data,dims,iError)

! Close Dataset
CALL H5DCLOSE_F(dset_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_ReadDataSet_INTEGER_rank6
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_ReadDataSet_INTEGER_rank7(loc_id,dset_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
INTEGER,INTENT(OUT)          :: data(:,:,:,:,:,:,:)
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: dset_id
!--------------------------------------------------------------------------------------------------!

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

! Open Dataset
CALL H5DOPEN_F(loc_id,dset_name,dset_id,iError)

! Read Dataset
CALL H5DREAD_F(dset_id,H5T_NATIVE_INTEGER,data,dims,iError)

! Close Dataset
CALL H5DCLOSE_F(dset_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_ReadDataSet_INTEGER_rank7
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_WriteAttribute_REAL_rank0(loc_id,obj_name,attr_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: obj_name
CHARACTER(LEN=*),INTENT(IN)  :: attr_name
REAL,INTENT(IN)              :: data
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: dspace_id
INTEGER(HID_T)               :: obj_id
INTEGER(HID_T)               :: attr_id
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
!--------------------------------------------------------------------------------------------------!

! Open Object
IF (obj_name .EQ. "") THEN
  obj_id = loc_id
ELSE
  CALL H5OOPEN_F(loc_id,obj_name,obj_id,iError)
END IF

! Set rank and dim
dset_rank = 1
ALLOCATE(dims(1:dset_rank))
dims = (/0/)

! Create Dataspace
CALL H5SCREATE_F(H5S_SCALAR_F,dspace_id,iError)

! Create Attributes
CALL H5ACREATE_F(obj_id,attr_name,H5T_NATIVE_DOUBLE,dspace_id,attr_id,iError)
CALL H5AWRITE_F(attr_id,H5T_NATIVE_DOUBLE,data,dims,iError)

! Close Attributes/Dataspace
CALL H5ACLOSE_F(attr_id,iError)
CALL H5SCLOSE_F(dspace_id,iError)
IF (obj_name .NE. "") THEN
  CALL H5OCLOSE_F(obj_id,iError)
END IF

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_WriteAttribute_REAL_rank0
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_WriteAttribute_REAL_rank1(loc_id,obj_name,attr_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: obj_name
CHARACTER(LEN=*),INTENT(IN)  :: attr_name
REAL,INTENT(IN)              :: data(:)
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: dspace_id
INTEGER(HID_T)               :: obj_id
INTEGER(HID_T)               :: attr_id
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
!--------------------------------------------------------------------------------------------------!

! Open Object
IF (obj_name .EQ. "") THEN
  obj_id = loc_id
ELSE
  CALL H5OOPEN_F(loc_id,obj_name,obj_id,iError)
END IF

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

! Create Dataspace
CALL H5SCREATE_SIMPLE_F(dset_rank,dims,dspace_id,iError)

! Create Attributes
CALL H5ACREATE_F(obj_id,attr_name,H5T_NATIVE_DOUBLE,dspace_id,attr_id,iError)
CALL H5AWRITE_F(attr_id,H5T_NATIVE_DOUBLE,data,dims,iError)

! Close Attributes/Dataspace
CALL H5ACLOSE_F(attr_id,iError)
CALL H5SCLOSE_F(dspace_id,iError)
IF (obj_name .NE. "") THEN
  CALL H5OCLOSE_F(obj_id,iError)
END IF

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_WriteAttribute_REAL_rank1
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_WriteAttribute_INTEGER_rank0(loc_id,obj_name,attr_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: obj_name
CHARACTER(LEN=*),INTENT(IN)  :: attr_name
INTEGER,INTENT(IN)           :: data
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: dspace_id
INTEGER(HID_T)               :: obj_id
INTEGER(HID_T)               :: attr_id
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
!--------------------------------------------------------------------------------------------------!

! Open Object
IF (obj_name .EQ. "") THEN
  obj_id = loc_id
ELSE
  CALL H5OOPEN_F(loc_id,obj_name,obj_id,iError)
END IF

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

! Create Dataspace
CALL H5SCREATE_SIMPLE_F(dset_rank,dims,dspace_id,iError)

! Create Attributes
CALL H5ACREATE_F(obj_id,attr_name,H5T_NATIVE_INTEGER,dspace_id,attr_id,iError)
CALL H5AWRITE_F(attr_id,H5T_NATIVE_INTEGER,data,dims,iError)

! Close Attributes/Dataspace
CALL H5ACLOSE_F(attr_id,iError)
CALL H5SCLOSE_F(dspace_id,iError)
IF (obj_name .NE. "") THEN
  CALL H5OCLOSE_F(obj_id,iError)
END IF

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_WriteAttribute_INTEGER_rank0
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_WriteAttribute_INTEGER_rank1(loc_id,obj_name,attr_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: obj_name
CHARACTER(LEN=*),INTENT(IN)  :: attr_name
INTEGER,INTENT(IN)           :: data(:)
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: dspace_id
INTEGER(HID_T)               :: obj_id
INTEGER(HID_T)               :: attr_id
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
!--------------------------------------------------------------------------------------------------!

! Open Object
IF (obj_name .EQ. "") THEN
  obj_id = loc_id
ELSE
  CALL H5OOPEN_F(loc_id,obj_name,obj_id,iError)
END IF

! Set rank and dim
dset_rank = 1
ALLOCATE(dims(1:dset_rank))
dims = (/0/)

! Create Dataspace
CALL H5SCREATE_F(H5S_SCALAR_F,dspace_id,iError)

! Create Attributes
CALL H5ACREATE_F(obj_id,attr_name,H5T_NATIVE_INTEGER,dspace_id,attr_id,iError)
CALL H5AWRITE_F(attr_id,H5T_NATIVE_INTEGER,data,dims,iError)

! Close Attributes/Dataspace
CALL H5ACLOSE_F(attr_id,iError)
CALL H5SCLOSE_F(dspace_id,iError)
IF (obj_name .NE. "") THEN
  CALL H5OCLOSE_F(obj_id,iError)
END IF

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_WriteAttribute_INTEGER_rank1
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_WriteAttribute_CHARACTER_rank0(loc_id,obj_name,attr_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: obj_name
CHARACTER(LEN=*),INTENT(IN)  :: attr_name
CHARACTER(LEN=*),INTENT(IN)  :: data
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: dspace_id
INTEGER(HID_T)               :: attr_id
INTEGER(HID_T)               :: obj_id
INTEGER(HID_T)               :: type_id
INTEGER(SIZE_T)              :: attr_len
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
!--------------------------------------------------------------------------------------------------!

! Open Object
IF (obj_name .EQ. "") THEN
  obj_id = loc_id
ELSE
  CALL H5OOPEN_F(loc_id,obj_name,obj_id,iError)
END IF

! Set rank and dim
dset_rank = 1
ALLOCATE(dims(1:dset_rank))
dims = (/0/)

! Create Dataspace
CALL H5SCREATE_F(H5S_SCALAR_F,dspace_id,iError)

! Create Datatype
attr_len = LEN_TRIM(data,KIND=HID_T)
CALL H5TCOPY_F(H5T_NATIVE_CHARACTER,type_id,iError)
CALL H5TSET_SIZE_F(type_id,attr_len,iError)

! Create Attributes
CALL H5ACREATE_F(obj_id,attr_name,type_id,dspace_id,attr_id,iError)
CALL H5AWRITE_F(attr_id,type_id,data,dims,iError)

! Close Types/Attributes/Dataspace
CALL H5ACLOSE_F(attr_id,iError)
CALL H5TCLOSE_F(type_id,iError)
CALL H5SCLOSE_F(dspace_id,iError)
IF (obj_name .NE. "") THEN
  CALL H5OCLOSE_F(obj_id,iError)
END IF

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_WriteAttribute_CHARACTER_rank0
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_WriteAttribute_CHARACTER_rank1(loc_id,obj_name,attr_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: obj_name
CHARACTER(LEN=*),INTENT(IN)  :: attr_name
CHARACTER(LEN=*),INTENT(IN)  :: data(:)
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: dspace_id
INTEGER(HID_T)               :: attr_id
INTEGER(HID_T)               :: obj_id
INTEGER(HID_T)               :: type_id
INTEGER(SIZE_T)              :: attr_len
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
!--------------------------------------------------------------------------------------------------!

! Open Object
IF (obj_name .EQ. "") THEN
  obj_id = loc_id
ELSE
  CALL H5OOPEN_F(loc_id,obj_name,obj_id,iError)
END IF

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

! Create Dataspace
CALL H5SCREATE_SIMPLE_F(dset_rank,dims,dspace_id,iError)

! Create Datatype
attr_len = 256
CALL H5TCOPY_F(H5T_NATIVE_CHARACTER,type_id,iError)
CALL H5TSET_SIZE_F(type_id,attr_len,iError)

! Create Attributes
CALL H5ACREATE_F(obj_id,attr_name,type_id,dspace_id,attr_id,iError)
CALL H5AWRITE_F(attr_id,type_id,data,dims,iError)

! Close Types/Attributes/Dataspace
CALL H5ACLOSE_F(attr_id,iError)
CALL H5TCLOSE_F(type_id,iError)
CALL H5SCLOSE_F(dspace_id,iError)
IF (obj_name .NE. "") THEN
  CALL H5OCLOSE_F(obj_id,iError)
END IF

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_WriteAttribute_CHARACTER_rank1
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_ReadAttribute_REAL_rank0(loc_id,obj_name,attr_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: obj_name
CHARACTER(LEN=*),INTENT(IN)  :: attr_name
REAL,INTENT(OUT)             :: data
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: obj_id
INTEGER(HID_T)               :: attr_id
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
!--------------------------------------------------------------------------------------------------!

! Open Object
IF (obj_name .EQ. "") THEN
  obj_id = loc_id
ELSE
  CALL H5OOPEN_F(loc_id,obj_name,obj_id,iError)
END IF

! Set rank and dim
dset_rank = 1
ALLOCATE(dims(1:dset_rank))
dims = (/0/)

! Read Attributes
CALL H5AOPEN_F(obj_id,attr_name,attr_id,iError)
CALL H5AREAD_F(attr_id,H5T_NATIVE_DOUBLE,data,dims,iError)

! Close Attributes/Dataspace
CALL H5ACLOSE_F(attr_id,iError)
IF (obj_name .NE. "") THEN
  CALL H5OCLOSE_F(obj_id,iError)
END IF

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_ReadAttribute_REAL_rank0
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_ReadAttribute_REAL_rank1(loc_id,obj_name,attr_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: obj_name
CHARACTER(LEN=*),INTENT(IN)  :: attr_name
REAL,INTENT(OUT)             :: data(:)
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: obj_id
INTEGER(HID_T)               :: attr_id
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
!--------------------------------------------------------------------------------------------------!

! Open Object
IF (obj_name .EQ. "") THEN
  obj_id = loc_id
ELSE
  CALL H5OOPEN_F(loc_id,obj_name,obj_id,iError)
END IF

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

! Read Attributes
CALL H5AOPEN_F(obj_id,attr_name,attr_id,iError)
CALL H5AREAD_F(attr_id,H5T_NATIVE_DOUBLE,data,dims,iError)

! Close Attributes/Dataspace
CALL H5ACLOSE_F(attr_id,iError)
IF (obj_name .NE. "") THEN
  CALL H5OCLOSE_F(obj_id,iError)
END IF

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_ReadAttribute_REAL_rank1
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_ReadAttribute_INTEGER_rank0(loc_id,obj_name,attr_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: obj_name
CHARACTER(LEN=*),INTENT(IN)  :: attr_name
INTEGER,INTENT(OUT)          :: data
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: obj_id
INTEGER(HID_T)               :: attr_id
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
!--------------------------------------------------------------------------------------------------!

! Open Object
IF (obj_name .EQ. "") THEN
  obj_id = loc_id
ELSE
  CALL H5OOPEN_F(loc_id,obj_name,obj_id,iError)
END IF

! Set rank and dim
dset_rank = 1
ALLOCATE(dims(1:dset_rank))
dims = (/0/)

! Read Attributes
CALL H5AOPEN_F(obj_id,attr_name,attr_id,iError)
CALL H5AREAD_F(attr_id,H5T_NATIVE_INTEGER,data,dims,iError)

! Close Attributes/Dataspace
CALL H5ACLOSE_F(attr_id,iError)
IF (obj_name .NE. "") THEN
  CALL H5OCLOSE_F(obj_id,iError)
END IF

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_ReadAttribute_INTEGER_rank0
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_ReadAttribute_INTEGER_rank1(loc_id,obj_name,attr_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: obj_name
CHARACTER(LEN=*),INTENT(IN)  :: attr_name
INTEGER,INTENT(OUT)          :: data(:)
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: obj_id
INTEGER(HID_T)               :: attr_id
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
!--------------------------------------------------------------------------------------------------!

! Open Object
IF (obj_name .EQ. "") THEN
  obj_id = loc_id
ELSE
  CALL H5OOPEN_F(loc_id,obj_name,obj_id,iError)
END IF

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

! Read Attributes
CALL H5AOPEN_F(obj_id,attr_name,attr_id,iError)
CALL H5AREAD_F(attr_id,H5T_NATIVE_INTEGER,data,dims,iError)

! Close Attributes/Dataspace
CALL H5ACLOSE_F(attr_id,iError)
IF (obj_name .NE. "") THEN
  CALL H5OCLOSE_F(obj_id,iError)
END IF

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_ReadAttribute_INTEGER_rank1
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_ReadAttribute_CHARACTER_rank0(loc_id,obj_name,attr_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: obj_name
CHARACTER(LEN=*),INTENT(IN)  :: attr_name
CHARACTER(LEN=*),INTENT(OUT) :: data
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: attr_id
INTEGER(HID_T)               :: obj_id
INTEGER(HID_T)               :: type_id
INTEGER(SIZE_T)              :: attr_len
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
!--------------------------------------------------------------------------------------------------!

! Open Object
IF (obj_name .EQ. "") THEN
  obj_id = loc_id
ELSE
  CALL H5OOPEN_F(loc_id,obj_name,obj_id,iError)
END IF

! Set rank and dim
dset_rank = 1
ALLOCATE(dims(1:dset_rank))
dims = (/0/)

! Create Datatype
attr_len = 256
CALL H5TCOPY_F(H5T_NATIVE_CHARACTER,type_id,iError)
CALL H5TSET_SIZE_F(type_id,attr_len,iError)

! Read Attributes
CALL H5AOPEN_F(obj_id,attr_name,attr_id,iError)
CALL H5AREAD_F(attr_id,type_id,data,dims,iError)

! Close Types/Attributes/Dataspace
CALL H5ACLOSE_F(attr_id,iError)
CALL H5TCLOSE_F(type_id,iError)
IF (obj_name .NE. "") THEN
  CALL H5OCLOSE_F(obj_id,iError)
END IF

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_ReadAttribute_CHARACTER_rank0
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_ReadAttribute_CHARACTER_rank1(loc_id,obj_name,attr_name,data)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: obj_name
CHARACTER(LEN=*),INTENT(IN)  :: attr_name
CHARACTER(LEN=*),INTENT(OUT) :: data(:)
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER                      :: dset_rank
INTEGER                      :: iError
INTEGER(HID_T)               :: attr_id
INTEGER(HID_T)               :: obj_id
INTEGER(HID_T)               :: type_id
INTEGER(SIZE_T)              :: attr_len
INTEGER(HSIZE_T),ALLOCATABLE :: dims(:)
!--------------------------------------------------------------------------------------------------!

! Open Object
IF (obj_name .EQ. "") THEN
  obj_id = loc_id
ELSE
  CALL H5OOPEN_F(loc_id,obj_name,obj_id,iError)
END IF

! Set rank and dim
dset_rank = RANK(data)
ALLOCATE(dims(1:dset_rank))
dims = SHAPE(data,KIND=HID_T)

! Create Datatype
attr_len = 256
CALL H5TCOPY_F(H5T_NATIVE_CHARACTER,type_id,iError)
CALL H5TSET_SIZE_F(type_id,attr_len,iError)

! Read Attributes
CALL H5AOPEN_F(obj_id,attr_name,attr_id,iError)
CALL H5AREAD_F(attr_id,type_id,data,dims,iError)

! Close Types/Attributes/Dataspace
CALL H5ACLOSE_F(attr_id,iError)
CALL H5TCLOSE_F(type_id,iError)
IF (obj_name .NE. "") THEN
  CALL H5OCLOSE_F(obj_id,iError)
END IF

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_ReadAttribute_CHARACTER_rank1
!==================================================================================================!
!
!
!
!==================================================================================================!
FUNCTION HDF5_CHECKPATHS(loc_id,obj_name) RESULT(exists)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: obj_name
LOGICAL                      :: exists
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER                      :: iError
INTEGER                      :: pos
INTEGER                      :: cpos
INTEGER                      :: str_len
!--------------------------------------------------------------------------------------------------!

! Check Intermediate Paths (Subgroups)
str_len = LEN_TRIM(obj_name)
cpos = 0
DO
  pos = INDEX(obj_name(cpos+1:str_len),"/")
  ! No Subgroup Found
  IF (pos .EQ. 0) EXIT
  ! Check Subgroup
  cpos = cpos + pos
  CALL H5LEXISTS_F(loc_id,obj_name(1:cpos-1),exists,iError)
  ! Return if Intermediate Path Fails
  IF (exists .EQV. .FALSE.) THEN
    exists = .FALSE.
    RETURN
  END IF
END DO

! Check Object (unless obj_name ended with "/"
IF (cpos .NE. str_len) THEN
  CALL H5LEXISTS_F(loc_id,obj_name,exists,iError)
  IF (exists .EQV. .FALSE.) THEN
    exists = .FALSE.
    RETURN
  END IF
END IF

exists = .TRUE.
RETURN

!--------------------------------------------------------------------------------------------------!
END FUNCTION HDF5_CHECKPATHS
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_CreateGroup(loc_id,group_name)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: group_name
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T)               :: group_id
INTEGER                      :: iError
!--------------------------------------------------------------------------------------------------!

! Create Group
CALL H5GCREATE_F(loc_id,group_name,group_id,iError)

! Close Group
CALL H5GCLOSE_F(group_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_CreateGroup
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_OpenGroup(loc_id,group_name,group_id)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: group_name
INTEGER(HID_T),INTENT(OUT)   :: group_id
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER                      :: iError
!--------------------------------------------------------------------------------------------------!

! Open Group
IF (HDF5_CHECKPATHS(loc_id,group_name)) THEN
  CALL H5GOPEN_F(loc_id,group_name,group_id,iError)
ELSE
  iError = -1
END IF

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_OpenGroup
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_CloseGroup(group_id)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: group_id
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER                      :: iError
!--------------------------------------------------------------------------------------------------!

! Close Group
CALL H5GCLOSE_F(group_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_CloseGroup
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_GetRank(loc_id,dset_name,rank)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
integer(HID_T),intent(in)    :: loc_id
character(len=*),intent(in)  :: dset_name
integer,intent(out)          :: rank
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T)               :: dset_id
INTEGER(HID_T)               :: dspace_id
INTEGER                      :: iError
!--------------------------------------------------------------------------------------------------!

! Open Dataset
CALL H5DOPEN_F(loc_id,dset_name,dset_id,iError)

! Get Dataspace
CALL H5DGET_SPACE_F(dset_id,dspace_id,iError)

! Get rank
CALL H5SGET_SIMPLE_EXTENT_NDIMS_F(dspace_id,rank,iError)

! Close Dataspace/Dataset
CALL H5SCLOSE_F(dspace_id,iError)  
CALL H5DCLOSE_F(dset_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_GetRank
!==================================================================================================!
!
!
!
!==================================================================================================!
SUBROUTINE HDF5_GetDims(loc_id,dset_name,dims)
!--------------------------------------------------------------------------------------------------!
USE HDF5
!--------------------------------------------------------------------------------------------------!
IMPLICIT NONE
!--------------------------------------------------------------------------------------------------!
! FORMAL ARGUMENTS
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T),INTENT(IN)    :: loc_id
CHARACTER(LEN=*),INTENT(IN)  :: dset_name
INTEGER,INTENT(OUT)          :: dims(:)
!--------------------------------------------------------------------------------------------------!
! LOCAL VARIABLES
!--------------------------------------------------------------------------------------------------!
INTEGER(HID_T)               :: dset_id
INTEGER(HID_T)               :: dspace_id
INTEGER(HSIZE_T)             :: dset_dims(6)
INTEGER(HSIZE_T)             :: max_dims(6)
INTEGER                      :: rank
INTEGER                      :: iError
!--------------------------------------------------------------------------------------------------!

! Open Dataset
CALL H5DOPEN_F(loc_id,dset_name,dset_id,iError)

! Get Dataspace
CALL H5DGET_SPACE_F(dset_id,dspace_id,iError)

! Get rank (ndims)
CALL H5SGET_SIMPLE_EXTENT_NDIMS_F(dspace_id,rank,iError)

! Get dims
CALL H5SGET_SIMPLE_EXTENT_DIMS_F(dspace_id,dset_dims(1:rank),max_dims(1:rank),iError)
dims(1:rank) = INT(dset_dims(1:rank))

! Close Dataspace/Dataset
CALL H5SCLOSE_F(dspace_id,iError)  
CALL H5DCLOSE_F(dset_id,iError)

!--------------------------------------------------------------------------------------------------!
END SUBROUTINE HDF5_GetDims
!==================================================================================================!
!
!
!
!--------------------------------------------------------------------------------------------------!
END MODULE MOD_HDF5_Tools
!==================================================================================================!
