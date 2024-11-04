### Download ROMS Source Code
1. Based on instructions found [here](https://www.myroms.org/wiki/Git), within a directory created to house the source repository (e.g. `/home/james.polly/repos/roms`) run: `git clone https://github.com/myroms/roms.git .`
2. OPTIONAL: As described in the link above, set some environmental variables:
    - `export ROMS_ROOT_DIR=/home/james.polly/repos`
    - This is optional and can actually be done in the build script as disucssed in the following step.
3. Following [these instructions](https://www.myroms.org/wiki/ROMS_UNSW2008#Customize_the_Build_Script), make a folder for the "upwelling" example, copy the build script, and edit as described therein.
    - `mkdir ~/projects/roms_proj/upwelling`
    - `cd ~/projects/roms_proj/upwelling`
    - `cp /home/james.polly/repos/roms/ROMS/Bin/build_roms.sh .`
    - Note that the build script has a different name than in the (probably outdated) instructions.
    - Edit the `build_roms.sh` file as described in the above instructions. Note that the `svn` example in the instructions above indicate source code existing at `/srv/ckpt/roms/shared/src` which is the equivalent of `/home/james.polly/repos/roms` in this documentation.
    - Exceptions and caveats to the above edit instructions are as follows:
        - Making the above Step 2 optional: It is probably best to set the environment variable `MY_ROOT_DIR` in the `else` clause of the `if` statement around line 148.
            - Change:   `export      MY_ROOT_DIR=${HOME}/ocean/repository/git`
            - To:   `export      MY_ROOT_DIR=/home/james.polly/repos`
        - Comment out the use of `ifort` and uncomment the use of `gfortran` (Lines 211, 212)
4. Next is [copying input and CPPDEFS options files](https://www.myroms.org/wiki/ROMS_UNSW2008#Copy_the_input_and_CPPDEFS_options_files).
    - These file names have changed: `ocean_upwelling.in` is now `roms_upwelling.in`, and `varinfo.dat` is now `varinfo.yaml`.
5. [Compile ROMS](https://www.myroms.org/wiki/ROMS_UNSW2008#Compile_ROMS).
    - Surprise! It doesn't work!
    - Summary provided via stdout:
    - ``` <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
GNU Build script command:      build_roms.sh -j 8
ROMS source directory:         /home/james.polly/repos/roms
ROMS header file:              /home/james.polly/projects/roms_proj/upwelling/upwelling.h
ROMS build  directory:         /home/james.polly/projects/roms_proj/upwelling/Build_romsG
ROMS Application:              UPWELLING
make: nf-config: Command not found
make: nf-config: Command not found
make: nf-config: Command not found
cp: cannot stat '/include/netcdf.mod': No such file or directory
Fortran compiler:              gfortran
Fortran flags:                 /include/netcdf.mod /home/james.polly/projects/roms_proj/upwelling/Build_romsG
-frepack-arrays -g -O0 -fbounds-check -fbacktrace -fcheck=all -finit-real=nan -ffpe-trap=invalid,zero,overflow
<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
```
    - Potential troubleshooting:
        - [Installing NetCDF but HDF first](https://www.myroms.org/wiki/External_Libraries#NetCDF_.28Network_Common_Data_Form.29)
        - Might need to shift to doing this in a container.
### Independent Verification on Personal Laptop
1. Above error was repeated on personal laptop.
2. `nc-config` is not found on either machine. This is likely a requirement for NetCDF Fortran.
3. `nf-config` is not found on either machine. This (NetCDF Fortran) is absolutely required for ROMS.

### Install `netcdf-c`:
1. Download the source code from [this most recent release on github](https://github.com/Unidata/netcdf-c/tree/v4.9.2) but check for [recent releases](https://github.com/Unidata/netcdf-c/releases).
    - From the first link in the above line, click the green "<> Code" button and download a ZIP of the source.
    - Unzip the downloaded file: `unzip netcdf-c-4.9.2.zip`
    - This will create a directory containing the zipped archive contents in the directory housing the zip file.
    - This directory will be the "build directory" for the following steps.
2. Follow [these instructions](https://docs.unidata.ucar.edu/nug/current/getting_and_building_netcdf.html#netCDF-CMake) which can be summarized as follows:
    - `cd netcdf-c-4.9.2`
    - `cmake .`
    - `make`
    - `make test`
    - `sudo make install`
3. `nc-config` (and other utilities/libraries) will now be installed.
    - Verify via `which nc-config`

### Install `netcdf-fortran`
1. Download the [latest release of `netcdf-fortran`](https://github.com/Unidata/netcdf-fortran/tree/v4.6.1). Check for later releases [here](https://github.com/Unidata/netcdf-fortran/releases).
    - Repeat the download ZIP and unzip process described above for `netcdf-C` (Step 1).
2. Follow the instructions for Shared Libraries as described [here](https://docs.unidata.ucar.edu/netcdf-c/current/building_netcdf_fortran.html). In short, these are:
    - Set environmental variables `NCDIR` and `NFDIR` to `/usr/local`
    - Set environmental variables `CPPFLAGS` and `LDFLAGS` as described in the instructions.
    - From the top level run: `./configure --prefix=${NFDIR}`
    - `make check` gave a great number of outputs about type mismatches but seems to have generally succeeded.
    - `sudo make install` failed initially, but after `sudo apt-get install m4` and retrying, netcdf-fortran` seems to have installed successfully.
    - `which nf-config` provides output.

### Try to compile ROMS again.
1. Comment out any MPI and OpenMP usage unless these are installed (`which mpif90`)
2. Do `./build_roms.sh` again. Should produce a `romsG` executable.

### Try for single processor run.
1. `./romsG < roms_upwelling.in > my_upwelling.log` doesn't work immediately. Complains about a missing `libnetcdff.so.7`.
2. Try to add this to the `.bashrc` file via:
   - `LD_LIBRARY_PATHY = /usr/local/lib` and `export LD_LIBRARY_PATH` at the end of the file.
3. Try to run again (step 1 here). Still doesn't work. Cryptic errors.
   - Check the .log output file and see that it can't find the YAML file for variable defs.
   - The `roms_upwelling.in` file specifies the location of the variable definition YAML.
   - Change this line to: `VARNAME = varinfo.yml` (rather than the incorrect `VARNAME = ROMS/External/varinfo.yaml`
4. Try again (step 1). Seems to be thinking hard. Let it go overnight.

### Try to install MPI for parallelization.
1. TODO...
