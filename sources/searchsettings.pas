unit SearchSettings;
//***************************************************************************************
//  Description: Implements classes and functionality for managing the search settings.
//    File Name: searchsettings.pas
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
  //------------------------------ TSearchSettings --------------------------------------
  TSearchSettings = class (TObject)
  private
    FDirectory: String;
    FRecursive: Boolean;
    FSearchText: String;
    FCaseSensitive: Boolean;
    FFilePattern: String;
    procedure SetDirectory(Value: String);
  public
    constructor Create;
    destructor  Destroy; override;
    property Directory: String read FDirectory write SetDirectory;
    property Recursive: Boolean read FRecursive write FRecursive;
    property SearchText: String read FSearchText write FSearchText;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
    property FilePattern: String read FFilePattern write FFilePattern;
  end;

implementation
//---------------------------------------------------------------------------------------
//-------------------------------- TSearchSettings --------------------------------------
//---------------------------------------------------------------------------------------
//***************************************************************************************
// NAME:           Create
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Class constructor.
//
//***************************************************************************************
constructor TSearchSettings.Create;
begin
  // Call inherited constructor.
  inherited Create;
  // Initialize fields to their default values.
  FDirectory := '';
  FRecursive := True;
  FSearchText := '';
  FCaseSensitive := False;
  FFilePattern := '*';
end; //*** end of Create ***


//***************************************************************************************
// NAME:           Destroy
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Class destructor.
//
//***************************************************************************************
destructor TSearchSettings.Destroy;
begin
  // Call inherited destructor.
  inherited Destroy;
end; //*** end of Destroy ***


//***************************************************************************************
// NAME:           SetDirectory
// PARAMETER:      Value New directory value.
// RETURN VALUE:   none
// DESCRIPTION:    Setter for the directory field.
//
//***************************************************************************************
procedure TSearchSettings.SetDirectory(Value: String);
begin
  // Only set the new directory value if it actually exists.
  if DirectoryExists(Value) then
  begin
    FDirectory := Value;
  end
  // Directory does not exist, so revert back to the default value.
  else
  begin
    FDirectory := '';
  end;
end;

end.
//******************************** end of searchsettings.pas ****************************


