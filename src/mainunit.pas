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
  ExtCtrls, ComCtrls, DateUtils, LCLType, SearchSettings, FileContentSearcher;


//***************************************************************************************
// Type Definitions
//***************************************************************************************
type
  //------------------------------ TUserInterfaceSetting --------------------------------
  TUserInterfaceSetting = ( UIS_DEFAULT = 0,
                            UIS_SEARCHING );

  //------------------------------ TMainForm --------------------------------------------
  TMainForm = class(TForm)
    BtnBrowse: TButton;
    BtnSearch: TButton;
    CbxCaseSensitive: TCheckBox;
    CbxRecursive: TCheckBox;
    CmbSearchPattern: TComboBox;
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
    SelectDirectoryDialog: TSelectDirectoryDialog;
    procedure BtnBrowseClick(Sender: TObject);
    procedure BtnSearchClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FUISetting: TUserInterfaceSetting;
    FSearchSettings: TSearchSettings;
    FFileContentSearcher: TFileContentSearcher;
    FTotalFilesSearchCount: LongWord;
    FFilesWithHitCount: LongWord;
    FTotalSearchHitCount: LongWord;
    FFileWithLastHit: String;
    FSearchStartTime: TDateTime;
    procedure InitializeUserInterface;
    procedure UpdateUserInterface;
    procedure CollectSearchSettings;
    function  StartSearch: Boolean;
    procedure FinishSearch;
    procedure CancelSearch;
    procedure HandleSearchError(ErrorInfo: String);
    procedure ClearSearchResults;
    procedure AddSearchFileToResults(Filename: String);
    procedure AddSearchOccurenceToResults(LineNumber: LongWord; LineContents: String; SearchedFile: String);
    procedure FileContentSearcherOnDone(Sender: TObject);
    procedure FileContentSearcherOnError(Sender: TObject; ErrorInfo: String);
    procedure FileContentSearcherFileFound(Sender: TObject; FoundFile: String);
    procedure FileContentSearcherOnFileSearchStarted(Sender: TObject; SearchFile: String);
    procedure FileContentSearcherOnFileSearchHit(Sender: TObject; SearchFile: String; HitLine: String; LineNumber: Longword);
  public
  end;


//***************************************************************************************
// Global Variables
//***************************************************************************************
var
  MainForm: TMainForm;
  CmdLineIgnoreCaseOption: Boolean;
  CmdLineNotRecursiveOption: Boolean;
  CmdLineDirectoryOption: String;


implementation

{$R *.lfm}

{ TMainForm }

{ TODO : Perhaps add progressbar or -label on the row with the search-button. }

//***************************************************************************************
// NAME:           FormCreate
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the form is constructed.
//
//***************************************************************************************
procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Clear captions of panels that are only used for layout purposes.
  PnlBody.Caption := '';
  PnlDirectory.Caption := '';
  PnlRecursive.Caption := '';
  PnlSearchText.Caption := '';
  PnlCaseSensitive.Caption := '';
  PnlSearchPattern.Caption := '';
  // Create instances of the search settings.
  FSearchSettings := TSearchSettings.Create;
  // Initialize default search settings.
  FSearchSettings.Directory :=  GetCurrentDir;
  // Create instance of the file content searcher.
  FFileContentSearcher := TFileContentSearcher.Create;
  // Configure event handlers.
  FFileContentSearcher.OnDone := @FileContentSearcherOnDone;
  FFileContentSearcher.OnError := @FileContentSearcherOnError;
  FFileContentSearcher.OnFileSearchStarted := @FileContentSearcherOnFileSearchStarted;
  FFileContentSearcher.OnFileSearchHit :=@FileContentSearcherOnFileSearchHit;
  FFileContentSearcher.OnFileFound := @FileContentSearcherFileFound;
  // Initialize fields to their default values.
  FUISetting := UIS_DEFAULT;
  FTotalFilesSearchCount := 0;
  FFilesWithHitCount := 0;
  FTotalSearchHitCount := 0;
  FFileWithLastHit := '';
  FSearchStartTime := Now;
  // Update search settings based on the info specified as command line options.
  if CmdLineIgnoreCaseOption then
    FSearchSettings.CaseSensitive := False;
  if CmdLineNotRecursiveOption then
    FSearchSettings.Recursive := False;
  if CmdLineDirectoryOption <> '' then
    FSearchSettings.Directory := CmdLineDirectoryOption;
  // Initialize the user interface.
  InitializeUserInterface;
end; //*** end of FormCreate ***


//***************************************************************************************
// NAME:           FormDestroy
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the form is destroyed.
//
//***************************************************************************************
procedure TMainForm.FormDestroy(Sender: TObject);
begin
  // Release the file content search instance.
  FFileContentSearcher.Free;
  // Release the search settings instance.
  FSearchSettings.Free;
end; //*** end of FormDestroy ***


//***************************************************************************************
// NAME:           BtnBrowseClick
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the button is clicked.
//
//***************************************************************************************
procedure TMainForm.BtnBrowseClick(Sender: TObject);
begin
  // Show directory selection dialog.
  if SelectDirectoryDialog.Execute then
  begin
    // Show the selected directory in the edit box.
    EdtDirectory.Text := SelectDirectoryDialog.FileName;
  end;
end; //*** end of BtnBrowseClick ***


//***************************************************************************************
// NAME:           BtnSearchClick
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the button is clicked.
//
//***************************************************************************************
procedure TMainForm.BtnSearchClick(Sender: TObject);
var
  boxStyle: Integer;
begin
  // The behavior of the event handler depends on the current user interface settings.
  if FUISetting = UIS_DEFAULT then
  begin
    // Start the search operation.
    if not StartSearch then
    begin
      // Configure the message box.
      boxStyle := MB_ICONINFORMATION + MB_OK;
      // Display the message box.
      Application.MessageBox('Invalid search settings detected. Please correct them and try again.',
                             'Problem detected', boxStyle);
    end;
  end
  else
  begin
    // Cancel the search operation.
    CancelSearch;
  end;
end; //*** end of BtnSearchClick ***


//***************************************************************************************
// NAME:           InitializeUserInterface
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Initializes the user interface.
//
//***************************************************************************************
procedure TMainForm.InitializeUserInterface;
var
  idx: Integer;
  filePatternMatchFound: Boolean;
begin
  // Initialize the user interface, based on currently set search settings.
  EdtDirectory.Text := FSearchSettings.Directory;
  CbxRecursive.Checked := FSearchSettings.Recursive;
  EdtSearchText.Text := FSearchSettings.SearchText;
  CbxCaseSensitive.Checked := FSearchSettings.CaseSensitive;
  // Try to find a matching file pattern entry in the combobox.
  filePatternMatchFound := False;
  for Idx := 0 to (CmbSearchPattern.Items.Count - 1) do
  begin
    // Is this entry a match to the file pattern in the current search settings?
    if CmbSearchPattern.Items[idx] = FSearchSettings.FilePattern then
    begin
      // Select this item in the combobox.
      CmbSearchPattern.ItemIndex := idx;
      // Set flag.
      filePatternMatchFound := True;
      // Match found, no need to continue looping.
      Break;
    end;
  end;
  // Check if a matching entry in the combobox was found.
  if not filePatternMatchFound then
  begin
    // Select the default file pattern from the combobox, which is the first entry.
    CmbSearchPattern.ItemIndex := 0;
    FSearchSettings.FilePattern := CmbSearchPattern.Items[CmbSearchPattern.ItemIndex];
  end;
  // Clear search results
  ClearSearchResults;
  // Update the user interface based on the currently selected user interface setting.
  UpdateUserInterface;
end; //*** end of InitializeUserInterface ***


//***************************************************************************************
// NAME:           UpdateUserInterface
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Updates the user interface based on the currently configured setting.
//
//***************************************************************************************
procedure TMainForm.UpdateUserInterface;
begin
  // Is the searching user interface setting selected?
  if FUISetting = UIS_SEARCHING then
  begin
    EdtDirectory.Enabled := False;
    BtnBrowse.Enabled := False;
    CbxRecursive.Enabled := False;
    EdtSearchText.Enabled := False;
    CbxCaseSensitive.Enabled := False;
    CmbSearchPattern.Enabled := False;
    BtnSearch.Caption := 'Cancel';
  end
  // The default setting must be selected.
  else
  begin
    EdtDirectory.Enabled := True;
    BtnBrowse.Enabled := True;
    CbxRecursive.Enabled := True;
    EdtSearchText.Enabled := True;
    CbxCaseSensitive.Enabled := True;
    CmbSearchPattern.Enabled := True;
    BtnSearch.Caption := 'Search';
  end;
end; //*** end of UpdateUserInterface ***


//***************************************************************************************
// NAME:           CollectSearchSettings
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Extracts the search settings from the user interface and stored them.
//
//***************************************************************************************
procedure TMainForm.CollectSearchSettings;
begin
 FSearchSettings.Directory := EdtDirectory.Text;
 FSearchSettings.Recursive := CbxRecursive.Checked;
 FSearchSettings.SearchText := EdtSearchText.Text;
 FSearchSettings.CaseSensitive := CbxCaseSensitive.Checked;
 FSearchSettings.FilePattern := CmbSearchPattern.Text;
end; //*** end of CollectSearchSettings ***/


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
  // Initialize fields used for search tracking and statistics.
  FTotalFilesSearchCount := 0;
  FFilesWithHitCount := 0;
  FTotalSearchHitCount := 0;
  FFileWithLastHit := '';
  // Collect search settings from the user interface.
  CollectSearchSettings;
  // Clear previous search results.
  ClearSearchResults;
  // Attempt to start the search operation.
  if FFileContentSearcher.Start(FSearchSettings) then
  begin
    // Set the start time.
    FSearchStartTime := Now;
    // Update the user interface.
    FUISetting := UIS_SEARCHING;
    UpdateUserInterface;
    // Update the result.
    Result := True;
  end;
end; //*** end of StartSearch ***


//***************************************************************************************
// NAME:           FinishSearch
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Finishes the completed search operation.
//
//***************************************************************************************
procedure TMainForm.FinishSearch;
var
  infoStr: String;
begin
  // Update the user interface.
  FUISetting := UIS_DEFAULT;
  UpdateUserInterface;
  // Show search statistics.
  infoStr := IntToStr(FTotalSearchHitCount) + ' hits found in ' +
             IntToStr(FFilesWithHitCount) + ' of ' + IntToStr(FTotalFilesSearchCount) +
             ' files (took ' + IntToStr(MilliSecondsBetween(Now, FSearchStartTime)) +
             ' ms)';
  MmoResults.Lines.Add('----- ' + infoStr + ' -----');
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
  // Cancellation the search operation.
  FFileContentSearcher.Cancel;
  // Update the user interface.
  FUISetting := UIS_DEFAULT;
  UpdateUserInterface;
end; //*** end of CancelSearch ***


//***************************************************************************************
// NAME:           HandleSearchError
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Processes and error that occured during the search operation.
//
//***************************************************************************************
procedure TMainForm.HandleSearchError(ErrorInfo: String);
var
  boxStyle: Integer;
begin
  // Configure the message box.
  boxStyle := MB_ICONERROR + MB_OK;
  // Display the message box.
  Application.MessageBox(PChar(ErrorInfo), 'Error detected', boxStyle);
  // Update the user interface.
  FUISetting := UIS_DEFAULT;
  UpdateUserInterface;
end; //*** end of HandleSearchError ***


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


//***************************************************************************************
// NAME:           FileContentSearcherOnDone
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the file content searcher
//                 completed successfully.
//
//***************************************************************************************
procedure TMainForm.FileContentSearcherOnDone(Sender: TObject);
begin
  // Wrap up the search operation.
  FinishSearch;
end; //*** end of FileContentSearcherOnDone ***


//***************************************************************************************
// NAME:           FileContentSearcherOnError
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the file content searcher
//                 aborted due to an error.
//
//***************************************************************************************
procedure TMainForm.FileContentSearcherOnError(Sender: TObject; ErrorInfo: String);
begin
  // Pass the error event on to the error handler.
  HandleSearchError(ErrorInfo);
end; //*** end of FileContentSearcherOnError ***


//***************************************************************************************
// NAME:           FileContentSearcherFileFound
// PARAMETER:      Sender Source of the event.
//                 FoundFile Detected file.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the file content searcher
//                 found a while, while it is building a list of files to search through.
//
//***************************************************************************************
procedure TMainForm.FileContentSearcherFileFound(Sender: TObject; FoundFile: String);
begin
  // Suppress hint for unused parameter.
  FoundFile := FoundFile;
  // Update total files found counter.
  FTotalFilesSearchCount := FTotalFilesSearchCount + 1;
end; //*** end of FileContentSearcherFileFound ***


//***************************************************************************************
// NAME:           FileContentSearcherOnFileSearchStarted
// PARAMETER:      Sender Source of the event.
//                 SearchFile The file who's content is now being searched through.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the file content searcher just
//                 started searching the contents of the next file.
//
//***************************************************************************************
procedure TMainForm.FileContentSearcherOnFileSearchStarted(Sender: TObject; SearchFile: String);
begin
  // Nothing needs to be done here for now.
  // Suppress hint for unused parameter.
  SearchFile := SearchFile;
end; //*** end of FileContentSearcherOnFileSearchStarted ***


//***************************************************************************************
// NAME:           FileContentSearcherOnFileSearchHit
// PARAMETER:      Sender Source of the event.
//                 SearchFile The file who's content contains the match.
//                 HitLine The entire line in the file that contains the search text.
//                 LineNumber Line number in which the search text was found.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the file content searcher found
//                 a match of the search for text in a file.
//
//***************************************************************************************
procedure TMainForm.FileContentSearcherOnFileSearchHit(Sender: TObject; SearchFile: String; HitLine: String; LineNumber: Longword);
begin
  // Is this hit in a new file?
  if SearchFile <> FFileWithLastHit then
  begin
    // Show the file in the results view.
    AddSearchFileToResults(SearchFile);
    // Store this file to be able to detect hits in a new file.
    FFileWithLastHit := SearchFile;
    // Update counter.
    FFilesWithHitCount := FFilesWithHitCount + 1;
  end;
  // Increment hit counter.
  FTotalSearchHitCount := FTotalSearchHitCount + 1;
  // Show the search hit information in the results view.
  AddSearchOccurenceToResults(LineNumber, HitLine, SearchFile);
end; //*** end of FileContentSearcherOnFileSearchHit ***

end.
//******************************** end of mainunit.pas **********************************

