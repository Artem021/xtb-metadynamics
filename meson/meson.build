# This file is part of xtb.
#
# Copyright (C) 2019-2020 Sebastian Ehlert
#
# xtb is free software: you can redistribute it and/or modify it under
# the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# xtb is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with xtb.  If not, see <https://www.gnu.org/licenses/>.

fc = meson.get_compiler('fortran')
cc = meson.get_compiler('c')

if fc.get_id() != cc.get_id()
  warning('FC and CC are not from the same vendor')
endif

fopts = []
if fc.get_id() == 'gcc'
  fopts = [
    '-fdefault-real-8',
    '-fdefault-double-8',
    '-ffree-line-length-none',
    '-fbacktrace',
  ]
elif fc.get_id() == 'intel'
  if get_option('buildtype') == 'release'
    opt_level = [
      '-Ofast',
      '-ip',
      '-axAVX2',
      '-mtune=core-avx2',
      '-fma',
    ]
  else
    opt_level = [
      '-axAVX2',
    ]
  endif
  fopts = opt_level + [
    '-r8',
    '-traceback',
  ]
elif fc.get_id() == 'intel-cl'
  if get_option('buildtype') == 'release'
    opt_level = [
      '/O3',
      '/Qip',
      '/QaxAVX2',
      '/tune:core-avx2',
      '/Qfma',
    ]
  else
    opt_level = [
      '/QaxAVX2',
    ]
  endif
  fopts = opt_level + [
    '/fpp',
    '/4R8',
    '/traceback',
  ]
elif fc.get_id() == 'pgi' or fc.get_id() == 'nvidia_hpc'
  fopts = [
    '-Mpreprocess',
    '-Mbackslash',
    '-Mallocatable=03',
    '-traceback',
    '-r8',
  ]

  if get_option('gpu')
    add_project_arguments('-acc', '-Minfo=accel', '-DXTB_GPU', language: 'fortran')
    add_project_link_arguments('-acc', '-Minfo=accel', language: 'fortran')

    gpu_arch = get_option('gpu_arch') 
    add_project_arguments('-ta=tesla:cc@0@'.format(gpu_arch), language: 'fortran')
    add_project_link_arguments('-ta=tesla:cc@0@'.format(gpu_arch), language: 'fortran')

    if get_option('cusolver')
      add_project_arguments('-Mcudalib=cusolver,cublas', '-DUSE_CUSOLVER', '-DUSE_CUBLAS', language: 'fortran')
      add_project_link_arguments('-Mcudalib=cusolver,cublas', language: 'fortran')
    endif
  endif
endif
add_project_arguments(fopts, language: 'fortran')

# fix compiliation problems with of symmetry/symmetry_i.c
add_project_arguments('-D_Float128=__float128', language: 'c')

## ========================================== ##
## LIBRARIES
## ========================================== ##

la_backend = get_option('la_backend')
if la_backend == 'mkl' or la_backend == 'mkl-static'
  add_project_arguments('-DWITH_MKL', language: 'fortran')
  if la_backend == 'mkl-static'
    add_project_link_arguments('-static', language: 'fortran')
    add_project_link_arguments('-static', language: 'c') # icc will do linking
  endif

  if get_option('default_library') == 'dynamic'
    mkl_rt_dep = cc.find_library('mkl_rt', required: true)
    lib_deps += libmkl_exe
  else
    if fc.get_id() == 'gcc'
      libmkl_exe = [cc.find_library('mkl_gf_lp64')]
      if get_option('openmp')
        libmkl_exe += cc.find_library('mkl_gnu_thread')
      endif
    elif fc.get_id() == 'intel' or fc.get_id() == 'intel-cl'
      libmkl_exe = [cc.find_library('mkl_intel_lp64')]
      if get_option('openmp')
        libmkl_exe += cc.find_library('mkl_intel_thread')
      endif
    elif fc.get_id() == 'pgi' or fc.get_id() == 'nvidia_hpc'
      libmkl_exe = [cc.find_library('mkl_intel_lp64')]
      if get_option('openmp')
        libmkl_exe += cc.find_library('mkl_pgi_thread')
      endif
    endif
    if not get_option('openmp')
      libmkl_exe += cc.find_library('mkl_sequential')
    endif
    libmkl_exe += cc.find_library('mkl_core')
    lib_deps += libmkl_exe
  endif

elif la_backend == 'mkl-rt'
  add_project_arguments('-DWITH_MKL', language: 'fortran')

  mkl_rt_dep = fc.find_library('mkl_rt', required: true)
  if fc.get_id() == 'intel'
    lib_deps += fc.find_library('ifcore')
  endif

  lib_deps += mkl_rt_dep

elif la_backend == 'openblas'
  # search for OpenBLAS
  blas_dep = dependency('openblas', required: false)
  if not blas_dep.found()
    blas_dep = fc.find_library('openblas', required: true)
  endif
  lib_deps += blas_dep
  # some OpenBLAS versions can provide lapack, check if we can find dsygvd
  openblas_provides_lapack = fc.links(
    'external dsygvd; call dsygvd(); end',
    dependencies: blas_dep,
  )
  # otherwise we fall back to LAPACK
  if not openblas_provides_lapack
    lapack_dep = dependency('lapack', required: false)
    if not lapack_dep.found()
      lapack_dep = fc.find_library('lapack', required: true)
    endif
    lib_deps += lapack_dep
  endif

elif la_backend == 'custom'
  foreach lib: get_option('custom_libraries')
    lib_deps += fc.find_library(lib)
  endforeach

else
  # Find BLAS (usually netlib, but in conda also OpenBLAS/MKL)
  blas_dep = dependency('blas', required: false)
  if not blas_dep.found()
    blas_dep = fc.find_library('blas', required: true)
  endif
  lib_deps += blas_dep
  # Find LAPACK (usually netlib, but in conda also MKL)
  lapack_dep = dependency('lapack', required: false)
  if not lapack_dep.found()
    lapack_dep = fc.find_library('lapack', required: true)
  endif
  lib_deps += lapack_dep

endif

if get_option('openmp')
  omp_dep = dependency('openmp', required: fc.get_id() != 'intel' and fc.get_id() != 'nvidia_hpc')
  if not omp_dep.found()
    if fc.get_id() == 'intel'
      message('Using -qopenmp to use OpenMP with Intel compilers')
      omp_dep = declare_dependency(
        compile_args: '-qopenmp',
        link_args: '-qopenmp',
      )
    else
      message('Using -mp to use OpenMP with NVHPC compilers')
      omp_dep = declare_dependency(
        compile_args: '-mp',
        link_args: '-mp',
      )
    endif
  endif
  lib_deps += omp_dep
endif

lib_deps += dependency('threads')

if get_option('nvtx')
  lib_deps += fc.find_library('nvToolsExt', required: true)
endif

# Create the tool chain library as subproject
mctc_dep = dependency(
  'mctc-lib',
  fallback: ['mctc-lib', 'mctc_dep'],
  default_options: ['default_library=static'],
)
lib_deps += mctc_dep
