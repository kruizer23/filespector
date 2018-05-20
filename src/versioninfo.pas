unit VersionInfo;
//***************************************************************************************
//  Description: Implements a class for accessing program version related information.
//    File Name: versioninfo.pas
//
//---------------------------------------------------------------------------------------
//                          C O P Y R I G H T
//---------------------------------------------------------------------------------------
//           Copyright (c) 2018 by Frank Voorburg   All rights reserved
//
//   This software has been carefully tested, but is not guaranteed for any particular
// purpose. The author does not offer any warranties and does not guarantee the accuracy,
//   adequacy, or completeness of the software and is not responsible for any errors or
//              omissions or the results obtained from use of the software.
//
//---------------------------------------------------------------------------------------
//                            L I C E N S E
//---------------------------------------------------------------------------------------
// This file is part of FileSpector. FileSpector is free software: you can redistribute
// it and/or modify it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or (at your option) any
// later version.
//
// FileSpector is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
// PARTICULAR PURPOSE. See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with this
// program.  If not, see <http://www.gnu.org/licenses/>.
//
//***************************************************************************************
{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

interface
//***************************************************************************************
// Includes
//***************************************************************************************
uses
  Classes, SysUtils;

//***************************************************************************************
// Type Definitions
//***************************************************************************************
type
  //------------------------------ TVersionInfo -----------------------------------------
  TVersionInfo = class (TObject)
  private
    const VERSION_MAJOR: Byte = 0;
    const VERSION_MINOR: Byte = 8;
    const VERSION_PATCH: Byte = 0;
    const VERSION_SPECIFIER: String = 'Beta';
  public
    class function GetVersionStr(WithSpecifier: Boolean = True): String; static;
    class function GetVersionMajor: Byte; static;
    class function GetVersionMinor: Byte; static;
    class function GetVersionPatch: Byte; static;
  end;

implementation
//---------------------------------------------------------------------------------------
//-------------------------------- TVersionInfo -----------------------------------------
//---------------------------------------------------------------------------------------
//***************************************************************************************
// NAME:           GetVersionStr
// PARAMETER:      WithSpecifier True to add the version specifier such as Alpha, Beta,
//                 RC, Stable. False to exclude this part.
// RETURN VALUE:   Program version information as a string.
// DESCRIPTION:    Obtains program version information as a string.
//
//***************************************************************************************
class function TVersionInfo.GetVersionStr(WithSpecifier: Boolean): String;
begin
  // Convert the version numbers to a string in the format x.yy.zz.
  Result := Format('%u.%u.%u', [VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH]);
  // Add the specifier if requested.
  if WithSpecifier then
  begin
    Result := Result + ' ' + VERSION_SPECIFIER;
  end;
end; //*** end of GetVersionStr ***


//***************************************************************************************
// NAME:           GetVersionMajor
// PARAMETER:      none
// RETURN VALUE:   Major version number.
// DESCRIPTION:    Obtains the major part of the program version number.
//
//***************************************************************************************
class function TVersionInfo.GetVersionMajor: Byte;
begin
  // Return the major version number.
  Result := VERSION_MAJOR;
end; //*** end of GetVersionMajor ***


//***************************************************************************************
// NAME:           GetVersionMinor
// PARAMETER:      none
// RETURN VALUE:   Minor version number.
// DESCRIPTION:    Obtains the minor part of the program version number.
//
//***************************************************************************************
class function TVersionInfo.GetVersionMinor: Byte;
begin
  // Return the minor version number.
  Result := VERSION_MINOR;
end; //*** end of GetVersionMinor ***


//***************************************************************************************
// NAME:           GetVersionPatch
// PARAMETER:      none
// RETURN VALUE:   Patch version number.
// DESCRIPTION:    Obtains the patch part of the program version number.
//
//***************************************************************************************
class function TVersionInfo.GetVersionPatch: Byte;
begin
  // Return the patch version number.
  Result := VERSION_PATCH;
end; //*** end of GetVersionPatch ***

end.
//******************************** end of versioninfo.pas *******************************


