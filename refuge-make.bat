:: Main build script and menu
@echo off
setlocal
set BUILD_DIR=c:\refuge-build
set SCRIPTS_DIR=%BUILD_DIR%\scripts

call %SCRIPTS_DIR%\sdk-path.cmd
call "%WindowsSdkPath%\bin\setenv.cmd" /Release /x86
color 07

:: evaluate arg
if "%~1"=="" call :all
if /I "%~1"=="help" call :usage
if /I "%~1"=="tools" call :tools
if /I "%~1"=="all" call :all
if /I "%~1"=="compile" call :compile
if /I "%~1"=="deps" call :deps
if /I "%~1"=="clean" call :clean
if /I "%~1"=="distclean" call :distclean
if /I "%~1"=="rel" call :rel
if /I "%~1"=="relclean" call :relclean
if /I "%~1"=="dev" call :dev
if /I "%~1"=="devrel" call :devrel
if /I "%~1"=="dev1dev2dev3" call :dev1dev2dev3
if /I "%~1"=="devclean" call :devclean
if /I "%~1"=="archive" call :archive
if /I "%~1"=="distdir" call :distdir
if /I "%~1"=="package" call :package
if /I "%~1"=="pkgclean" call :pkgclean

endlocal
goto eof

:: TODO: Not implemented options
:all
echo making for taget all ...
call rebar.cmd get-deps
call rebar.cmd compile
goto eof

:compile
echo compiling ...
call rebar.cmd compile
goto eof

:deps
echo retreiving erlang dependencies ...
call rebar.cmd get-deps
goto eof

:clean
echo cleaning ...
call rebar.cmd clean
goto eof

:distclean
echo cleaning distribution ...
call rebar.cmd clean
call rebar.cmd delete-deps
goto eof

:rel
echo generating release ...
call rebar.cmd get-deps
call rebar.cmd compile
goto eof

:relclean
echo cleaning release ...
rd /s /q rel/refuge
goto eof

:: dev targets
:dev
echo cleaning and compiling development nodes ...
echo option 'dev' Not implemented, be patient its coming
goto eof

:devrel
echo option 'devrel' Not implemented, be patient its coming
goto eof

:dev1dev2dev3
echo compiling development nodes ...
echo option 'dev1 dev2 dev3' not implemented, be patient its coming
goto eof

:devclean
echo cleaning development nodes ...
echo option 'devclean' not implemented, be patient its coming
goto eof

:: release tarballs
:archive
echo generating tar archive ...
echo option 'archive' not implemented, be patient its coming
goto eof

:distdir
echo gzipping archive to distribution directory ...
echo option 'distdir' not implemented, be patient its coming
goto eof

:package
echo packaging release tarball ..
echo option 'package' not implemented, be patient its coming
goto eof

:pkgclean
echo cleaning taball release files ...
echo option 'pkgclean' not implemented, be patient its coming
goto eof

:usage
echo.
echo refuge-make.bat,  Version 0.1.0 for Windows
echo.
echo Make command that works with Refuge-Build to support complex compilation
echo of refuge projects on Windows using Elang's "rebar" build tool.
echo.
echo    Usage:  refuge-make arg
echo.
echo    arg is one of the following supported arguments. Passing no arg
echo    is equivalent to passing 'all'.
echo.
echo    help          # Displays help
echo    all           # Fetches dependences and compiles the project
echo    compile       # Compiles the project
echo    deps          # Fetches erlang build dependencies
echo    clean         # Cleans the project of compile artefacts
echo    distclean     # Todo
echo    rel           # Fetch dependencies and compile release
echo    relclean      # Cleans release target removing all files
echo    dev           # Clean and compiles development nodes
echo    devrel        # Alias for dev1dev2dev3
echo    dev1dev2dev3  # Compiles development nodes
echo    devclean      # Cleans dev nodes removing all files
echo    archive       # Generates a tar archive
echo    distdir       # Generate a gzipped archive in distribution directory
echo    package       # Package the distribution
echo    pkgclean      # Cleand the package files
echo.
echo    Issues? File issues at http://github.com/refuge/refuge-build
echo.

:eof