unit ConfigGroups;
//***************************************************************************************
//  Description: Configuration groups available to the program.
//    File Name: configgroups.pas
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
  Classes, SysUtils, CurrentConfig, XMLConf;


//***************************************************************************************
// Type Definitions
//***************************************************************************************
type
  //------------------------------ TMainWindowConfig ------------------------------------
  TMainWindowConfig = class (TConfigGroup)
  private
    FWidth: Integer;
    FHeight: Integer;
    FResultsColumn0Width: Integer;
    FResultsColumn1Width: Integer;
    FResultsColumn2Width: Integer;
    FResultsColumn3Width: Integer;
  public
    const GROUP_NAME='MainWindow';
    constructor Create;
    procedure Defaults; override;
    procedure LoadFromFile(XmlConfig: TXMLConfig); override;
    procedure SaveToFile(XmlConfig: TXMLConfig); override;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property ResultsColumn0Width: Integer read FResultsColumn0Width write FResultsColumn0Width;
    property ResultsColumn1Width: Integer read FResultsColumn1Width write FResultsColumn1Width;
    property ResultsColumn2Width: Integer read FResultsColumn2Width write FResultsColumn2Width;
    property ResultsColumn3Width: Integer read FResultsColumn3Width write FResultsColumn3Width;
  end;

  //------------------------------ TLastSearchConfig ------------------------------------
  TLastSearchConfig = class (TConfigGroup)
  private
    FDirectory: String;
    FRecursive: Integer;
    FCaseSensitive: Integer;
    FFilePattern: String;
  public
    const GROUP_NAME='LastSearch';
    constructor Create;
    procedure Defaults; override;
    procedure LoadFromFile(XmlConfig: TXMLConfig); override;
    procedure SaveToFile(XmlConfig: TXMLConfig); override;
    property Directory: String read FDirectory write FDirectory;
    property Recursive: Integer read FRecursive write FRecursive;
    property CaseSensitive: Integer read FCaseSensitive write FCaseSensitive;
    property FilePattern: String read FFilePattern write FFilePattern;
  end;


implementation
//---------------------------------------------------------------------------------------
//-------------------------------- TMainWindowConfig ------------------------------------
//---------------------------------------------------------------------------------------
//***************************************************************************************
// NAME:           Create
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Class constructor.
//
//***************************************************************************************
constructor TMainWindowConfig.Create;
begin
  // Call inherited constructor.
  inherited Create;
  // Set fields.
  FName := GROUP_NAME;
  Defaults;
end; //*** end of Create ***


//***************************************************************************************
// NAME:           Defaults
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Sets default values for this group's settings.
//
//***************************************************************************************
procedure TMainWindowConfig.Defaults;
begin
  FWidth := 1000;
  FHeight := 550;
  FResultsColumn0Width := 140;
  FResultsColumn1Width := 60;
  FResultsColumn2Width := 525;
  FResultsColumn3Width := 245;
end; //*** end of Defaults ***


//***************************************************************************************
// NAME:           LoadFromFile
// PARAMETER:      XmlConfig XML configuration instance.
// RETURN VALUE:   none
// DESCRIPTION:    Loads this group's configuration settings using the XML configuration
//                 instance.
//
//***************************************************************************************
procedure TMainWindowConfig.LoadFromFile(XmlConfig: TXMLConfig);
begin
  // Open this group's key.
  XmlConfig.OpenKey(UnicodeString(Self.Name));
  // Load all settings.
  FWidth := XmlConfig.GetValue('width', FWidth);
  FHeight := XmlConfig.GetValue('height', FHeight);
  FResultsColumn0Width := XmlConfig.GetValue('results_column0_width', FResultsColumn0Width);
  FResultsColumn1Width := XmlConfig.GetValue('results_column1_width', FResultsColumn1Width);
  FResultsColumn2Width := XmlConfig.GetValue('results_column2_width', FResultsColumn2Width);
  FResultsColumn3Width := XmlConfig.GetValue('results_column3_width', FResultsColumn3Width);
  // Close this group's key.
  XmlConfig.CloseKey;
end; //*** end of LoadFromFile ***/


//***************************************************************************************
// NAME:           SaveToFile
// PARAMETER:      XmlConfig XML configuration instance.
// RETURN VALUE:   none
// DESCRIPTION:    Saves this group's configuration settings using the XML configuration
//                 instance.
//
//***************************************************************************************
procedure TMainWindowConfig.SaveToFile(XmlConfig: TXMLConfig);
begin
  // Open this group's key.
  XmlConfig.OpenKey(UnicodeString(Self.Name));
  // Store all settings.
  XmlConfig.SetValue('width', FWidth);
  XmlConfig.SetValue('height', FHeight);
  XmlConfig.SetValue('results_column0_width', FResultsColumn0Width);
  XmlConfig.SetValue('results_column1_width', FResultsColumn1Width);
  XmlConfig.SetValue('results_column2_width', FResultsColumn2Width);
  XmlConfig.SetValue('results_column3_width', FResultsColumn3Width);
  // Close this group's key.
  xmlConfig.CloseKey;
end; //*** end of SaveToFile ***

//---------------------------------------------------------------------------------------
//-------------------------------- TLastSearchConfig ------------------------------------
//---------------------------------------------------------------------------------------
//***************************************************************************************
// NAME:           Create
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Class constructor.
//
//***************************************************************************************
constructor TLastSearchConfig.Create;
begin
  // Call inherited constructor.
  inherited Create;
  // Set fields.
  FName := GROUP_NAME;
  Defaults;
end; //*** end of Create ***


//***************************************************************************************
// NAME:           Defaults
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Sets default values for this group's settings.
//
//***************************************************************************************
procedure TLastSearchConfig.Defaults;
begin
  FDirectory := '';
  FRecursive := 1;
  FCaseSensitive := 0;
  FFilePattern := '*.*';
end; //*** end of Defaults ***


//***************************************************************************************
// NAME:           LoadFromFile
// PARAMETER:      XmlConfig XML configuration instance.
// RETURN VALUE:   none
// DESCRIPTION:    Loads this group's configuration settings using the XML configuration
//                 instance.
//
//***************************************************************************************
procedure TLastSearchConfig.LoadFromFile(XmlConfig: TXMLConfig);
begin
  // Open this group's key.
  XmlConfig.OpenKey(UnicodeString(Self.Name));
  // Load all settings.
  FDirectory := String(XmlConfig.GetValue('directory', UnicodeString(FDirectory)));
  FRecursive := XmlConfig.GetValue('recursive', FRecursive);
  FCaseSensitive := XmlConfig.GetValue('case_sensitive', FCaseSensitive);
  FFilePattern := String(XmlConfig.GetValue('file_pattern', UnicodeString(FFilePattern)));
  // Close this group's key.
  XmlConfig.CloseKey;
end; //*** end of LoadFromFile ***/


//***************************************************************************************
// NAME:           SaveToFile
// PARAMETER:      XmlConfig XML configuration instance.
// RETURN VALUE:   none
// DESCRIPTION:    Saves this group's configuration settings using the XML configuration
//                 instance.
//
//***************************************************************************************
procedure TLastSearchConfig.SaveToFile(XmlConfig: TXMLConfig);
begin
  // Open this group's key.
  XmlConfig.OpenKey(UnicodeString(Self.Name));
  // Store all settings.
  XmlConfig.SetValue('directory', UnicodeString(FDirectory));
  XmlConfig.SetValue('recursive', FRecursive);
  XmlConfig.SetValue('case_sensitive', FCaseSensitive);
  XmlConfig.SetValue('file_pattern', UnicodeString(FFilePattern));
  // Close this group's key.
  xmlConfig.CloseKey;
end; //*** end of SaveToFile ***

end.
//******************************** end of configgroups.pas ******************************

