#--------------------------------------------------------------------------------------------------#
# MAIN LIB
#--------------------------------------------------------------------------------------------------#
ADD_LIBRARY(MainLib OBJECT ${SOURCES})
SET_TARGET_PROPERTIES(MainLib PROPERTIES COMPILE_FLAGS ${MAIN_FLAGS_OVERALL})
#--------------------------------------------------------------------------------------------------#
#
#--------------------------------------------------------------------------------------------------#
# MAIN
#--------------------------------------------------------------------------------------------------#
ADD_EXECUTABLE(main "./src/main.f90")
IF(MAIN_BUILD_HDF5)
  ADD_DEPENDENCIES(MainLib HDF5)
ENDIF()
ADD_DEPENDENCIES(main MainLib ${INTERNALLIBS})
TARGET_LINK_LIBRARIES(main MainLib ${LINKEDLIBS})
SET_TARGET_PROPERTIES(main PROPERTIES COMPILE_FLAGS ${MAIN_FLAGS_OVERALL})
#--------------------------------------------------------------------------------------------------#
