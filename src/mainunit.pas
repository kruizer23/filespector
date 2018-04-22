unit MainUnit;
//***************************************************************************************
//  Description: Contains the main user interface for FileCruncher.
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
// This file is part of FileCruncher. FileCruncher is free software: you can redistribute
// it and/or modify it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or (at your option) any
// later version.
//
// FileCruncher is distributed in the hope that it will be useful, but WITHOUT ANY
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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls;


//***************************************************************************************
// Type Definitions
//***************************************************************************************
type

  { TMainForm }

  TMainForm = class(TForm)
    BtnBrowse: TButton;
    BtnSearch: TButton;
    CbxCaseSensitive: TCheckBox;
    CbxRecursive: TCheckBox;
    CmdSearchPattern: TComboBox;
    EdtDirectory: TEdit;
    EdtSearchText: TEdit;
    GbxSearchLocation: TGroupBox;
    GbxSearchSpecification: TGroupBox;
    GbxSearchResults: TGroupBox;
    GbxSearchLimitation: TGroupBox;
    LblDirectory: TLabel;
    LblSearchText: TLabel;
    LblSearchPattern: TLabel;
    MmoResults: TMemo;
    PnlBody: TPanel;
    PnlCaseSensitive: TPanel;
    PnlSearchText: TPanel;
    PnlRecursive: TPanel;
    PnlDirectory: TPanel;
    PnlSearchPattern: TPanel;
    procedure BtnBrowseClick(Sender: TObject);
    procedure BtnSearchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function  StartSearch: Boolean;
    procedure FinishSearch;
    procedure CancelSearch;
    procedure ClearSearchResults;
    procedure AddSearchFileToResults(Filename: String);
    procedure AddSearchOccurenceToResults(LineNumber: LongWord; LineContents: String; SearchedFile: String);
  public
  end;


//***************************************************************************************
// Global Variables
//***************************************************************************************
var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

{ TODO : Add user interface state enum, class field and procedure UpdateUserInterface. }
{ TODO : Add class with search settings or maybe add them individually as fields. }
{ TODO : Perhaps add progressbar or -label on the row with the search-button. }



//***************************************************************************************
// NAME:           FormCreate
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Form constructor.
//
//***************************************************************************************
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Clear captions of panels that are only used for layour purposes.
  PnlBody.Caption := '';
  PnlDirectory.Caption := '';
  PnlRecursive.Caption := '';
  PnlSearchText.Caption := '';
  PnlCaseSensitive.Caption := '';
  PnlSearchPattern.Caption := '';
end; //*** end of FormCreate ***


//***************************************************************************************
// NAME:           BtnBrowseClick
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the button is clicked.
//
//***************************************************************************************
procedure TMainForm.BtnBrowseClick(Sender: TObject);
begin
  { TODO : Implement browse button clicked event handler. }
end; //*** end of BtnBrowseClick ***


//***************************************************************************************
// NAME:           BtnSearchClick
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the button is clicked.
//
//***************************************************************************************
procedure TMainForm.BtnSearchClick(Sender: TObject);
begin
  { TODO : Implement browse button clicked event handler. }
end; //*** end of BtnSearchClick ***


//***************************************************************************************
// NAME:           StartSearch
// PARAMETER:      none
// RETURN VALUE:   True if successful, False otherwise.
// DESCRIPTION:    Starts the search operation.
//
//***************************************************************************************
function TMainForm.StartSearch: Boolean;
begin
  // Initialize the result.
  Result := False;
  { TODO : Implement start of search operation. }
end; //*** end of StartSearch ***


//***************************************************************************************
// NAME:           FinishSearch
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Finishes the completed search operation.
//
//***************************************************************************************
procedure TMainForm.FinishSearch;
begin
  { TODO : Implement finish of search operation. }
end; //*** end of FinishSearch ***


//***************************************************************************************
// NAME:           CancelSearch
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Cancels the search operation.
//
//***************************************************************************************
procedure TMainForm.CancelSearch;
begin
  { TODO : Implement cancel search operation. }
end; //*** end of CancelSearch ***


//***************************************************************************************
// NAME:           ClearSearchResults
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Removes all previously added search results from the user interface.
//
//***************************************************************************************
procedure TMainForm.ClearSearchResults;
begin
  // Remove all lines with search results.
  MmoResults.Lines.Clear;
end; //*** end of ClearSearchResults ***


//***************************************************************************************
// NAME:           AddSearchFileToResults
// PARAMETER:      Filename The name of the file that is about to be searched.
// RETURN VALUE:   none
// DESCRIPTION:    Adds the name of the file that is about to be searched to the user
//                 interface.
//
//***************************************************************************************
procedure TMainForm.AddSearchFileToResults(Filename: String);
begin
  // Display filename on a new line.
  MmoResults.Lines.Add(Filename);
end; //*** end of AddSearchFileToResults ***


//***************************************************************************************
// NAME:           AddSearchOccurenceToResults
// PARAMETER:      LineNumber Line number in the file that the match occurred on.
//                 LineContetns Contents of the line that the match occurred on.
//                 SearchedFile Filename of the file that containts the match.
// RETURN VALUE:   none
// DESCRIPTION:    Adds the detected match of the search through the file to the user
//                 interface.
//
//***************************************************************************************
procedure TMainForm.AddSearchOccurenceToResults(LineNumber: LongWord; LineContents: String; SearchedFile: String);
var
  lineEntry: String;
begin
  // Suppress hint due to unused parameter.
  SearchedFile := SearchedFile;
  // Construct the entry.
  lineEntry := Format('  %6u: ', [LineNumber]) + LineContents;
  // Add the entry as a new line.
  MmoResults.Lines.Add(lineEntry);
end; //*** end of AddSearchOccurenceToResults ***

end.
//******************************** end of mainunit.pas **********************************

