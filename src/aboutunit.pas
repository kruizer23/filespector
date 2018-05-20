//***************************************************************************************
//  Description: Contains the main user interface for FileSpector.
//    File Name: mainunit.pas
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
unit AboutUnit;
{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

interface
//***************************************************************************************
// Includes
//***************************************************************************************
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, VersionInfo, LCLIntf;


//***************************************************************************************
// Type Definitions
//***************************************************************************************
type
  //------------------------------ TAboutForm -------------------------------------------
  TAboutForm = class(TForm)
    BtnClose: TButton;
    ImgProgramIcon: TImage;
    LblWebLink: TLabel;
    LblCopyright: TLabel;
    LblVersion: TLabel;
    LblProgramName: TLabel;
    MmoLicense: TMemo;
    TgbLicense: TToggleBox;
    procedure FormCreate(Sender: TObject);
    procedure LblWebLinkClick(Sender: TObject);
    procedure LblWebLinkMouseEnter(Sender: TObject);
    procedure LblWebLinkMouseLeave(Sender: TObject);
    procedure TgbLicenseChange(Sender: TObject);
  private
    procedure PopulateLicenseText;
  public
  end;

implementation

{$R *.lfm}
//***************************************************************************************
// Localization
//***************************************************************************************
resourcestring
  RsCopyrightPre = 'Copyright';
  RsCopyrightPost = '2018 by Frank Voorburg';
  RsWeblinkInfo = 'Visit FileSpector on GitHub';

//---------------------------------------------------------------------------------------
//-------------------------------- TAboutForm -------------------------------------------
//---------------------------------------------------------------------------------------
//***************************************************************************************
// NAME:           FormCreate
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the form is constructed.
//
//***************************************************************************************
procedure TAboutForm.FormCreate(Sender: TObject);
begin
  // Adjust the height of the form, such that the memo with license info is not visible.
  Height := 326;
  // Set the version information on the label.
  LblVersion.Caption := TVersionInfo.GetVersionStr();
  // Set copyright info.
  LblCopyright.Caption := RsCopyrightPre + ' ' + #$C2#$A9 + ' ' + RsCopyrightPost;
  // Set visit on the web info.
  LblWebLink.Caption := RsWeblinkInfo;
  // Align certain components horizontally on the form.
  ImgProgramIcon.Left := (Width div 2) - (ImgProgramIcon.Width div 2);
  // Add the license text to the memo.
  PopulateLicenseText;
  // Don't show the memo by default.
  MmoLicense.Visible := False;
end; //*** end of FormCreate ***


//***************************************************************************************
// NAME:           LblWebLinkClick
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the component was clicked.
//
//***************************************************************************************
procedure TAboutForm.LblWebLinkClick(Sender: TObject);
begin
  // Open the browser and visit the FileSpector page on GitHub.
  OpenURL('https://github.com/kruizer23/filespector');
end; //*** end of LblWebLinkClick ***


//***************************************************************************************
// NAME:           LblWebLinkMouseEnter
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the mouse entered the rect of the
//                 component.
//
//***************************************************************************************
procedure TAboutForm.LblWebLinkMouseEnter(Sender: TObject);
begin
  // Switch mouse cursor to hand to indicate that it is a hyperlink.
  LblWebLink.Cursor := crHandPoint;
end; //*** end of LblWebLinkMouseEnter ***


//***************************************************************************************
// NAME:           LblWebLinkMouseLeave
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the mouse left the rect of the
//                 component.
//
//***************************************************************************************
procedure TAboutForm.LblWebLinkMouseLeave(Sender: TObject);
begin
  // Switch back to the default mouse cursor.
  LblWebLink.Cursor := crDefault;
end; //*** end of LblWebLinkMouseLeave ***


//***************************************************************************************
// NAME:           TgbLicenseChange
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the components was clicked.
//
//***************************************************************************************
procedure TAboutForm.TgbLicenseChange(Sender: TObject);
begin
  // Did the toggle box get checked?
  if TgbLicense.Checked then
  begin
    // Move the license memo into view.
    MmoLicense.Top := 182;
    MmoLicense.Visible := True;
  end
  else
  begin
    // Move the license memo out of view.
    MmoLicense.Top := 336;
    MmoLicense.Visible := False;
  end;
end; //*** end of TgbLicenseChange ***


//***************************************************************************************
// NAME:           PopulateLicenseText
// PARAMETER:      nione
// RETURN VALUE:   none
// DESCRIPTION:    Adds the license text to the memo. This is done programmatically to
//                 make sure the text doesn't end up in the PO file. It doesn't have to
//                 be translated, because the FSF does not accept translations.
//
//***************************************************************************************
procedure TAboutForm.PopulateLicenseText;
begin
  MmoLicense.Lines.Clear;
  with MmoLicense.Lines do
  begin
    Add('This program is free software: you can redistribute it and/or modify');
    Add('it under the terms of the GNU General Public License as published by');
    Add('the Free Software Foundation, either version 3 of the License, or');
    Add('(at your option) any later version.');
    Add('');
    Add('This program is distributed in the hope that it will be useful,');
    Add('but WITHOUT ANY WARRANTY; without even the implied warranty of');
    Add('MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the');
    Add('GNU General Public License for more details.');
    Add('');
    Add('You should have received a copy of the GNU General Public License');
    Add('along with this program.  If not, see <https://www.gnu.org/licenses/>.');
  end;
end; //*** end of PopulateLicenseText ***

end.
//******************************** end of aboutunit.pas *********************************


