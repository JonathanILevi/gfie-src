@echo off
rem    Greenfish Icon Editor Pro
rem    Copyright (c) 2012-13 B. Szalkai

rem    This program is free software: you can redistribute it and/or modify
rem    it under the terms of the GNU General Public License as published by
rem    the Free Software Foundation, either version 3 of the License, or
rem    (at your option) any later version.

rem    This program is distributed in the hope that it will be useful,
rem    but WITHOUT ANY WARRANTY; without even the implied warranty of
rem    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
rem    GNU General Public License for more details.

rem    You should have received a copy of the GNU General Public License
rem    along with this program.  If not, see <http://www.gnu.org/licenses/>.

rem Set 32-bit Lazarus installation location here.
set lazarus32=c:\lazarus32
rem Set 32-bit Lazarus config path here.
set lazarus32config=%lazarus32%\__config
rem Set 64-bit Lazarus installation location here.
set lazarus64=c:\lazarus
rem Set 64-bit Lazarus config path here.
set lazarus64config=%lazarus64%\__config

"%lazarus64%\lazbuild" --primary-config-path="%lazarus64config%" --build-mode=Release gfie.lpi

rem Modify output exe name
sed s/gfie64/gfie32/g gfie.lpi > gfie32.lpi
cp gfie.ico gfie32.ico
cp gfie.res gfie32.res
"%lazarus32%\lazbuild" --primary-config-path="%lazarus32config%" --build-mode=Release --operating-system=win32 --cpu=i386 gfie32.lpi
