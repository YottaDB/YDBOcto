# - Finds BAT instalation
# This module sets up BAT information
# It defines:
# BAT_FOUND          If the BAT is found
find_program(BATS bats)

include(FindPackageHandleStandardArgs)
# handle the QUIETLY and REQUIRED arguments and set BAT_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args(BATS BATS)

mark_as_advanced(BATS)
