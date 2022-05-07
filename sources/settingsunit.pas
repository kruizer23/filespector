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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  TextEditor;


//***************************************************************************************
// Type Definitions
//***************************************************************************************
type
  //------------------------------ TSettingsForm ----------------------------------------

  { TSettingsForm }

  TSettingsForm = class(TForm)
    BtnClose: TButton;
    CbxAutoDetect: TCheckBox;
    CmbEditorPlusArg: TComboBox;
    GbxSettings: TGroupBox;
    LblGroupTextEditor: TLabel;
    procedure BtnCloseClick(Sender: TObject);
    procedure CbxAutoDetectChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
var
  idx: Integer;
  entryStr: string;
begin
  // Set default values for the properties.
  FAutoConfigEnabled := True;
  FEditor := '';
  FLineNumberOptPrefix := '';
  // Add entries into the combobox.
  CmbEditorPlusArg.Items.Clear;
  // Loop through the array with commonly knowns text editors on a Linux system.
  for idx := 1 to TTextEditor.NUM_EDITORS do
  begin
    // Try to get the editor's executable to check its presence.
    entryStr := TTextEditor.GetEditorFullPath(TTextEditor.DEFAULT_EDITORS[1][idx]);
    // Is this one present on the user's system?
    if entryStr <> '' then
    begin
      // Append the argument to jump to a specific line number.
      entryStr := entryStr + ' ' + TTextEditor.DEFAULT_EDITORS[2][idx];
      // Add the resulting string as a new combobox entry.
      CmbEditorPlusArg.Items.Add(entryStr);
    end;
  end;
end; //*** end of FormCreate ***


//***************************************************************************************
// NAME:           FormShow
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the form is shown.
//
//***************************************************************************************
procedure TSettingsForm.FormShow(Sender: TObject);
begin
  // Show the currently configured editor and its line number argument in the combobox.
  CmbEditorPlusArg.Text := Editor + ' ' + LineNumberOptPrefix;
  // Set the auto detect checkbox state.
  CbxAutoDetect.Checked := AutoConfigEnabled;
  // Enable/disable the combobox based on the auto detect checkbox state.
  CmbEditorPlusArg.Enabled := not CbxAutoDetect.Checked;
  // If nothing configured, then display the first editor detected on the system.
  if Editor = '' then
  begin
    if CmbEditorPlusArg.Items.Count > 0 then
    begin
      CmbEditorPlusArg.Text := CmbEditorPlusArg.Items[0];
    end;
  end;
end; //*** end of FormShow ***


//***************************************************************************************
// NAME:           BtnCloseClick
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the close button is pressed.
//
//***************************************************************************************
procedure TSettingsForm.BtnCloseClick(Sender: TObject);
var
  spacePos: Integer;
begin
  // Initialize the properties
  FAutoConfigEnabled := CbxAutoDetect.Checked;
  FEditor := '';
  FLineNumberOptPrefix := '';
  // Text editor command manually entered?
  if not FAutoConfigEnabled then
  begin
    // It could be just a command or a command and its line number argument. In the
    // latter case the text should contain a space.
    spacePos := Pos(' ', CmbEditorPlusArg.Text);
    // Was a space detected?
    if spacePos > 0 then
    begin
      // Extract the editor command.
      FEditor := LeftStr(CmbEditorPlusArg.Text, spacePos - 1);
      // Extract the line number argument.
      FLineNumberOptPrefix := RightStr(CmbEditorPlusArg.Text,
                                       Length(CmbEditorPlusArg.Text) - spacePos);
    end
    // No space detected, so no line number arguement specified.
    else
    begin
      FLineNumberOptPrefix := '';
      FEditor := CmbEditorPlusArg.Text;
    end;
  end;
end; //*** end of BtnCloseClick ***


//***************************************************************************************
// NAME:           CbxAutoDetectChange
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the checkbox state changes.
//
//***************************************************************************************
procedure TSettingsForm.CbxAutoDetectChange(Sender: TObject);
begin
  // Update the configuration of the auto config property.
  AutoConfigEnabled := CbxAutoDetect.Checked;
  // Enable/disable the combobox based on the auto detect checkbox state.
  CmbEditorPlusArg.Enabled := not CbxAutoDetect.Checked;
end; //*** end of CbxAutoDetectChange ***


end.
//******************************** end of settingsunit.pas ******************************

