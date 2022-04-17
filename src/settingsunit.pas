unit SettingsUnit;
//***************************************************************************************
//  Description: Contains the settings dialog.
//    File Name: settingsunit.pas
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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;


//***************************************************************************************
// Type Definitions
//***************************************************************************************
type
  //------------------------------ TSettingsForm ----------------------------------------

  { TSettingsForm }

  TSettingsForm = class(TForm)
    BtnClose: TButton;
    procedure BtnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FAutoConfigEnabled: Boolean;
    FEditor: String;
    FLineNumberOptPrefix: String;
  public
    property AutoConfigEnabled: Boolean read FAutoConfigEnabled write FAutoConfigEnabled;
    property Editor: String read FEditor write FEditor;
    property LineNumberOptPrefix: String read FLineNumberOptPrefix write FLineNumberOptPrefix;
  end;


implementation

{$R *.lfm}

{ TSettingsForm }

//***************************************************************************************
// NAME:           FormCreate
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the form is constructed.
//
//***************************************************************************************
procedure TSettingsForm.FormCreate(Sender: TObject);
begin
  // Set default values for the properties.
  FAutoConfigEnabled := True;
  FEditor := '';
  FLineNumberOptPrefix := '';
end; //*** end of FormCreate ***


//***************************************************************************************
// NAME:           BtnCloseClick
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the close button is pressed.
//
//***************************************************************************************
procedure TSettingsForm.BtnCloseClick(Sender: TObject);
begin
  // TODO Update the properties based on what the user selected on the dialog. Currently
  //      hardcoded to Xed for testing purposes.
  FAutoConfigEnabled := False;
  FEditor := 'xed';
  FLineNumberOptPrefix := '+';
end; //*** end of BtnCloseClick ***

end.
//******************************** end of settingsunit.pas ******************************

