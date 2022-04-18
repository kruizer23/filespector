unit MainUnit;
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
{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

interface
//***************************************************************************************
// Includes
//***************************************************************************************
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, DateUtils, LCLType, ActnList, Menus, SearchSettings, Clipbrd,
  FileContentSearcher, TextEditor, AboutUnit, SettingsUnit, CurrentConfig, ConfigGroups;


//***************************************************************************************
// Type Definitions
//***************************************************************************************
type
  //------------------------------ TUserInterfaceSetting --------------------------------
  TUserInterfaceSetting = ( UIS_DEFAULT = 0,
                            UIS_SEARCHING );

  //------------------------------ TMainForm --------------------------------------------

  { TMainForm }

  TMainForm = class(TForm)
    ActCopySelectedLineToClipboard: TAction;
    ActProgramSettings: TAction;
    ActProgramAbout: TAction;
    ActProgramExit: TAction;
    ActOpenInEditor: TAction;
    ActSearch: TAction;
    ActSaveAllLinesToFile: TAction;
    ActionList: TActionList;
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
    ImageList: TImageList;
    LblDirectory: TLabel;
    LblSearchText: TLabel;
    LblSearchPattern: TLabel;
    LvwResults: TListView;
    MainMenu: TMainMenu;
    MnuItemSettings: TMenuItem;
    MnuItemEdit: TMenuItem;
    MnuItemOpenInEditor: TMenuItem;
    MnuItemAbout: TMenuItem;
    MnuItemExit: TMenuItem;
    MnuItemHelp: TMenuItem;
    MnuItemFile: TMenuItem;
    MnuItemCopySelectedLine: TMenuItem;
    MnuItemSaveAllLines: TMenuItem;
    PnlBody: TPanel;
    PnlCaseSensitive: TPanel;
    PnlSearchText: TPanel;
    PnlRecursive: TPanel;
    PnlDirectory: TPanel;
    PnlSearchPattern: TPanel;
    CtxMnuResultsView: TPopupMenu;
    PrgBarSearch: TProgressBar;
    SaveDialog: TSaveDialog;
    SelectDirectoryDialog: TSelectDirectoryDialog;
    StatusBar: TStatusBar;
    TimerStartSearch: TTimer;
    procedure ActCopySelectedLineToClipboardExecute(Sender: TObject);
    procedure ActOpenInEditorExecute(Sender: TObject);
    procedure ActProgramAboutExecute(Sender: TObject);
    procedure ActProgramExitExecute(Sender: TObject);
    procedure ActProgramSettingsExecute(Sender: TObject);
    procedure ActSaveAllLinesToFileExecute(Sender: TObject);
    procedure ActSearchExecute(Sender: TObject);
    procedure BtnBrowseClick(Sender: TObject);
    procedure CtxMnuResultsViewPopup(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LvwResultsDblClick(Sender: TObject);
    procedure LvwResultsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure OnKeyUpHandlerToStartSearch(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure LvwResultsColumnClick(Sender: TObject; Column: TListColumn);
    procedure LvwResultsCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
  private
    FCurrentConfig: TCurrentConfig;
    FUISetting: TUserInterfaceSetting;
    FSearchSettings: TSearchSettings;
    FTextEditor: TTextEditor;
    FFileContentSearcher: TFileContentSearcher;
    FTotalFilesSearchCount: LongWord;
    FFilesSearchedCount: LongWord;
    FFilesWithHitCount: LongWord;
    FTotalSearchHitCount: LongWord;
    FFileWithLastHit: String;
    procedure LoadCurrentConfig;
    procedure SaveCurrentConfig;
    procedure InitializeUserInterface;
    procedure UpdateUserInterface;
    procedure ReattachProgressbar;
    procedure UpdateSearchProgress(Reset: Boolean = False);
    procedure CollectSearchSettings;
    function  StartSearch: Boolean;
    procedure FinishSearch;
    procedure CancelSearch;
    procedure HandleSearchError(ErrorInfo: String);
    procedure ClearSearchResults;
    procedure AddSearchFileToResults(Filename: String);
    procedure AddSearchOccurenceToResults(LineNumber: LongWord; LineContents: String; SearchedFile: String);
    procedure ResultsViewAddRow(LineContents: String; LineNumber: LongWord = 0; SearchedFile: String = '');
    function  ResultsViewGetRowInfo(RowIdx: LongWord; var SearchedFile: String; var LineContents: String; var LineNumber: LongWord):Boolean;
    procedure ResultsViewClearRows;
    function  ConstructClipboardLine(LineContents: String; LineNumber: LongWord; SearchedFile: String): String;
    procedure FileContentSearcherOnDone(Sender: TObject);
    procedure FileContentSearcherOnError(Sender: TObject; ErrorInfo: String);
    procedure FileContentSearcherOnFileFound(Sender: TObject; FoundFile: String);
    procedure FileContentSearcherOnFileSearchStarted(Sender: TObject; SearchFile: String);
    procedure FileContentSearcherOnFileSearchHit(Sender: TObject; SearchFile: String; HitLine: String; LineNumber: Longword);
  public
  end;


//***************************************************************************************
// Global Variables
//***************************************************************************************
var
  MainForm: TMainForm;
  CmdLineIgnoreCaseOptionFound: Boolean;
  CmdLineIgnoreCaseOption: Boolean;
  CmdLineRecursiveOptionFound: Boolean;
  CmdLineRecursiveOption: Boolean;
  CmdLineDirectoryOption: String;
  CmdLineAutoStartOption: Boolean;
  CmdLineFilePatternOption: String;
  CmdLineSearchTextOption: String;


implementation

{$R *.lfm}

//***************************************************************************************
// Localization
//***************************************************************************************
resourcestring
  RsSearchStartErrorInfo = 'Invalid search settings detected. Please correct them and try again.';
  RsSearchStartErrorTitle = 'Problem detected';
  RsButtonCaptionSearch = 'Search';
  RsButtonCaptionCancel = 'Cancel';
  RsSearchGenericErrorTitle = 'Error detected';
  RsStatusBarSearchHits = 'Search hits';
  RsStatusBarFilesWithHits = 'Files with a search hit';
  RsStatusBarConnectorText = 'out of';


//---------------------------------------------------------------------------------------
//-------------------------------- TMainForm --------------------------------------------
//---------------------------------------------------------------------------------------
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
  // Create instance to manage the program's configuration and add the configuration
  // group instance(s).
  FCurrentConfig := TCurrentConfig.Create;
  FCurrentConfig.AddGroup(TMainWindowConfig.Create);
  FCurrentConfig.AddGroup(TLastSearchConfig.Create);
  FCurrentConfig.AddGroup(TTextEditorConfig.Create);
  // Create instance of the text editor.
  FTextEditor := TTextEditor.Create;
  // Create instance of the file content searcher.
  FFileContentSearcher := TFileContentSearcher.Create;
  // Load the programs configuration. Make sure to do this before processing the
  // settings from the command-line, as the latter ones should get priority.
  LoadCurrentConfig;
  // Initialize default search directory if it is still empty.
  if FSearchSettings.Directory = '' then
    FSearchSettings.Directory :=  GetCurrentDir;
  // Configure event handlers.
  FFileContentSearcher.OnDone := @FileContentSearcherOnDone;
  FFileContentSearcher.OnError := @FileContentSearcherOnError;
  FFileContentSearcher.OnFileSearchStarted := @FileContentSearcherOnFileSearchStarted;
  FFileContentSearcher.OnFileSearchHit :=@FileContentSearcherOnFileSearchHit;
  FFileContentSearcher.OnFileFound := @FileContentSearcherOnFileFound;
  // Initialize fields to their default values.
  FUISetting := UIS_DEFAULT;
  FTotalFilesSearchCount := 0;
  FFilesSearchedCount := 0;
  FFilesWithHitCount := 0;
  FTotalSearchHitCount := 0;
  FFileWithLastHit := '';
  // Update search settings based on the info specified as command line options.
  if CmdLineIgnoreCaseOptionFound then
    FSearchSettings.CaseSensitive := not CmdLineIgnoreCaseOption;
  if CmdLineRecursiveOptionFound then
    FSearchSettings.Recursive := CmdLineRecursiveOption;
  if CmdLineDirectoryOption <> '' then
    FSearchSettings.Directory := CmdLineDirectoryOption;
  if CmdLineFilePatternOption <> '' then
    FSearchSettings.FilePattern := CmdLineFilePatternOption;
  if CmdLineSearchTextOption <> '' then
    FSearchSettings.SearchText := CmdLineSearchTextOption;
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
  // Save the program's configuration.
  SaveCurrentConfig;
  // Release the file content search instance.
  FFileContentSearcher.Free;
  // Release the text editor instance.
  FTextEditor.Free;
  // Release the instance that manages the program's configuration.
  FCurrentConfig.Free;
  // Release the search settings instance.
  FSearchSettings.Free;
end; //*** end of FormDestroy ***


//***************************************************************************************
// NAME:           FormShow
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the form is shown.
//
//***************************************************************************************
procedure TMainForm.FormShow(Sender: TObject);
begin
  // Should the search be started automatically?
  if CmdLineAutoStartOption then
  begin
    // Reset the auto start flag as this should only be done once.
    CmdLineAutoStartOption := False;
    // Perform start of search. This assumes all needed search settings were specified
    // on the command line.
    ActSearchExecute(Self);
  end;
end; //*** end of FormShow ***


//***************************************************************************************
// NAME:           FormResize
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the form is resized.
//
//***************************************************************************************
procedure TMainForm.FormResize(Sender: TObject);
begin
  // Make sure the progress bar is properly located on the statusbar.
  ReattachProgressbar;
end; //*** end of FormResize ***


//***************************************************************************************
// NAME:           LvwResultsColumnClick
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when a click event occured on a column
//                 of the list view header.
//
//***************************************************************************************
procedure TMainForm.LvwResultsColumnClick(Sender: TObject; Column: TListColumn);
var
  colIdx: Integer;
begin
  // Set this column as the one to be sorted on so it can be accessed in the OnCompare
  // event to determine the column to sort on.
  LvwResults.SortColumn := Column.Index;
  // Use the tag field to set all other columns to "do not sort" (0).
  for colIdx := 0 to (LvwResults.Columns.Count - 1) do
  begin
     if LvwResults.Columns[colIdx] <> Column then
     begin
       LvwResults.Columns[colIdx].Tag := 0;
     end;
  end;
  // Is the current column not yet sorted?
  if Column.Tag = 0 then
  begin
    // Configure it for ascending sort order (1).
    Column.Tag := 1;
  end
  // Column has been sorted in the past.
  else
  begin
     // In this case invert the sort order. (1) for ascending and (-1) for descending.
     Column.Tag := Column.Tag * -1;
  end;
  // Sort the items in the listview. This will invoke the OnCompare event if the SortType
  // is set to stNone.
  LvwResults.AlphaSort;
end; //*** end of LvwResultsColumnClick ***


//***************************************************************************************
// NAME:           LvwResultsCompare
// PARAMETER:      Sender Source of the event.
//                 Item1 The first item to compare.
//                 Item2 The second item to compare.
//                 Data Not really sure what this one means. Not used.
//                 Compare To store the result of the comparison:
//                   -1 if Item1 < Item2
//                    0 if Item1 = Item2
//                    1 if Item1 > Item2
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when a compare of two items in a column
//                 is requested for sorting purposes. Note that this event handler only
//                 gets called when the listview's SortType is set to stNone.
//
//***************************************************************************************
procedure TMainForm.LvwResultsCompare(Sender: TObject; Item1, Item2: TListItem;
                                      Data: Integer; var Compare: Integer);
var
  item1IntVal: LongWord;
  item2IntVal: LongWord;
begin
  // Suppress warning due to unused parameter.
  Data := Data;
  // Initialize the result.
  Compare := 0;
  // The file column contains text and can use a default text comparison.
  if LvwResults.SortColumn = 0 then
  begin
    Compare := CompareStr(Item1.Caption, Item2.Caption);
  end
  // The line column contains an integer and requires a number comparison.
  else if LvwResults.SortColumn = 1 then
  begin
    try
      item1IntVal := StrToInt(Item1.SubItems[0]);
      item2IntVal := StrToInt(Item2.SubItems[0]);
      if item1IntVal < item2IntVal then
      begin
        Compare := -1;
      end
      else if item1IntVal > item2IntVal then
      begin
        Compare := 1;
      end;
    finally
    end;
  end
  // The contents column contains text and can use a default text comparison. it might
  // start with spaces or tabs, so make sure to trim it first.
  else if LvwResults.SortColumn = 2 then
  begin
    Compare := CompareStr(Trim(Item1.SubItems[1]), Trim(Item2.SubItems[1]));
  end
  // The directory column contains text and can use a default text comparison.
  else if LvwResults.SortColumn = 3 then
  begin
    Compare := CompareStr(Item1.SubItems[2], Item2.SubItems[2]);
  end;
  if (LvwResults.SortColumn >= 0) and
     (LvwResults.SortColumn < LvwResults.Columns.Count) then
  begin
    // Handle sort order.
    Compare := Compare * LvwResults.Columns[LvwResults.SortColumn].Tag;
  end;
end;  //*** end of LvwResultsCompare ***


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
// NAME:           ActCopySelectedLineToClipboardExecute
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the associated action should be
//                 executed.
//
//***************************************************************************************
procedure TMainForm.ActCopySelectedLineToClipboardExecute(Sender: TObject);
var
  searchedFile: String;
  lineContents: String;
  lineNumber: LongWord;
  clipboardLine: String;
begin
  // Initialize locals.
  searchedFile := '';
  lineContents := '';
  lineNumber := 0;
  // Only continue if a row is selected in the list view.
  if LvwResults.Selected <> nil then
  begin
    // Attempt to extract the contents of the selected row.
    if ResultsViewGetRowInfo(LvwResults.Selected.Index, searchedFile, lineContents,
                             lineNumber) then
    begin
      // Convert it to a clipboard line and add it to the clipboard.
      clipboardLine := ConstructClipboardLine(lineContents, lineNumber, searchedFile);
      Clipboard.AsText := Trim(clipboardLine) + LineEnding;
    end;
  end;
end; //*** end of ActCopySelectedLineToClipboardExecute ***


//***************************************************************************************
// NAME:           ActOpenInEditorExecute
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the associated action should be
//                 executed.
//
//***************************************************************************************
procedure TMainForm.ActOpenInEditorExecute(Sender: TObject);
var
  searchedFile: String;
  lineContents: String;
  lineNumber: LongWord;
begin
  // Initialize locals.
  searchedFile := '';
  lineContents := '';
  lineNumber := 0;

  // Only continue if a row is selected in the list view.
  if LvwResults.Selected <> nil then
  begin
    // Attempt to extract the contents of the selected row.
    if ResultsViewGetRowInfo(LvwResults.Selected.Index, searchedFile, lineContents,
                             lineNumber) then
    begin
      // Attempt to open the file at the specific line number with a text editor.
      FTextEditor.Open(searchedFile, lineNumber);
    end;
  end;
end; //*** end of ActOpenInEditorExecute ***


//***************************************************************************************
// NAME:           ActProgramAboutExecute
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the associated action should be
//                 executed.
//
//***************************************************************************************
procedure TMainForm.ActProgramAboutExecute(Sender: TObject);
var
  aboutForm: TAboutForm;
begin
  // Create instance of the about form.
  aboutForm := TAboutForm.Create(Self);
  // Show the form in a modal manner.
  aboutForm.ShowModal;
  // Release about form instance.
  aboutForm.Free;
end; //*** end of ActProgramAboutExecute ***


//***************************************************************************************
// NAME:           ActProgramExitExecute
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the associated action should be
//                 executed.
//
//***************************************************************************************
procedure TMainForm.ActProgramExitExecute(Sender: TObject);
begin
  // Close the program.
  Close;
end; //*** end of ActProgramExitExecute ***


//***************************************************************************************
// NAME:           ActProgramSettingsExecute
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the associated action should be
//                 executed.
//
//***************************************************************************************
procedure TMainForm.ActProgramSettingsExecute(Sender: TObject);
var
  settingsForm: TSettingsForm;
  textEditorConfig: TTextEditorConfig;
begin
  // Initialize configuration variable for convenient access to the group.
  textEditorConfig := FCurrentConfig.Groups[TTextEditorConfig.GROUP_NAME]
                      as TTextEditorConfig;
  // Create instance of the settings form.
  settingsForm := TSettingsForm.Create(Self);
  // Initialize the settings properties because on the currently configured values.
  settingsForm.AutoConfigEnabled := textEditorConfig.AutoConfigEnabled;
  settingsForm.LineNumberOptPrefix := textEditorConfig.LineNumberOptPrefix;
  settingsForm.Editor := textEditorConfig.Editor;
  // Show the form in a modal manner.
  if settingsForm.ShowModal = mrOK then
  begin
    // Update this class' text editor object.
    if settingsForm.AutoConfigEnabled then
    begin
      // Perform automatic configuration of the editor.
      FTextEditor.Locate;
    end
    else
    begin
       // Store the user configured values.
       FTextEditor.LineNumberOptPrefix := settingsForm.LineNumberOptPrefix;
       FTextEditor.Editor := settingsForm.Editor;
    end;
    // Update the configuration of the settings for persistency.
    textEditorConfig.AutoConfigEnabled := settingsForm.AutoConfigEnabled;
    textEditorConfig.LineNumberOptPrefix := FTextEditor.LineNumberOptPrefix;
    textEditorConfig.Editor := FTextEditor.Editor;
  end;
  // Release about form instance.
  settingsForm.Free;
end; //*** end of ActProgramSettingsExecute ***


//***************************************************************************************
// NAME:           ActSaveAllLinesToFileExecute
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the associated action should be
//                 executed.
//
//***************************************************************************************
procedure TMainForm.ActSaveAllLinesToFileExecute(Sender: TObject);
var
  linesList: TStringList;
  lineIdx: LongWord;
  searchedFile: String;
  lineContents: String;
  lineNumber: LongWord;
  clipboardLine: String;
begin
  // Initialize locals.
  searchedFile := '';
  lineContents := '';
  lineNumber := 0;
  // Set the initial directory to the work directory.
  SaveDialog.InitialDir := GetCurrentDir;
    // Only continue if there are actually contents in the list view.
  if LvwResults.Items.Count > 0 then
  begin
    // Display the dialog to prompt the user to pick a file.
    if SaveDialog.Execute then
    begin
      // Create instance of a string list.
      linesList := TStringList.Create;
      // Loop through all rows in the list view.
      for lineIdx := 0 to (LvwResults.Items.Count - 1) do
      begin
        // Attempt to extract the contents of the selected row.
        if ResultsViewGetRowInfo(lineIdx, searchedFile, lineContents, lineNumber) then
        begin
          // Convert it to a clipboard line and add it to the lines list.
          clipboardLine := ConstructClipboardLine(lineContents, lineNumber, searchedFile);
          linesList.Add(clipboardLine);
        end;
      end;
      // Save the contents of the string list to the selected file.
      linesList.SaveToFile(SaveDialog.FileName);
      // Release the string list instance.
      linesList.Free;
    end;
  end;
end; //*** end of ActSaveAllLinesToFileExecute ***


//***************************************************************************************
// NAME:           ActStartSearchExecute
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the associated action should be
//                 executed.
//
//***************************************************************************************
procedure TMainForm.ActSearchExecute(Sender: TObject);
var
  boxStyle: Integer;
begin
  // Disable the timer, which might have been used to trigger this operation.
  TimerStartSearch.Enabled := False;

  // The behavior of the event handler depends on the current user interface settings.
  if FUISetting = UIS_DEFAULT then
  begin
    // Start the search operation.
    if not StartSearch then
    begin
      // Configure the message box.
      boxStyle := MB_ICONINFORMATION + MB_OK;
      // Display the message box.
      Application.MessageBox(PChar(RsSearchStartErrorInfo),
                             PChar(RsSearchStartErrorTitle), boxStyle);
      // This problem typically happens when the user did not enter a search text, so
      // automatically set that edit box as the active control, so the user can correct
      // the problem right awaz.
      ActiveControl := EdtSearchText;
    end;
  end
  else
  begin
    // Cancel the search operation.
    CancelSearch;
  end;
end; //*** end of ActStartSearchExecute ***


//***************************************************************************************
// NAME:           BtnSearchClick
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the context menu is about to
//                 pop up.
//
//***************************************************************************************
procedure TMainForm.CtxMnuResultsViewPopup(Sender: TObject);
var
  relativeCursosPos: TPoint;
  itemUnderCursor: TListItem;
begin
  // Enable all entries by default.
  ActOpenInEditor.Enabled := True;
  ActCopySelectedLineToClipboard.Enabled := True;
  ActSaveAllLinesToFile.Enabled := True;

  // Some items in the context menu should only be available and execute on the row
  // that the context menu pops up on. Find out on which item the contect menu popped
  // up on. First, get the position of the mouse cursor relative to the list view.
  relativeCursosPos := LvwResults.ScreenToClient(Mouse.CursorPos);
  // Next, find the item under the cursor, if any.
  itemUnderCursor := LvwResults.GetItemAt(relativeCursosPos.x, relativeCursosPos.y);
  // Is there is no item under the cursor, then deselect whatever item is currently
  // selected in the list view.
  if itemUnderCursor = nil then
  begin
    LvwResults.Selected := nil;
  end
  // The cursor is on and item.
  else
  begin
    // It is possible that another row is selected than the one the cursor is on. In this
    // case, select the one that the cursor is on.
    if itemUnderCursor <> LvwResults.Selected then
    begin
      LvwResults.Selected := itemUnderCursor;
    end;
  end;

  // Disable the copy line to clipboard and open in editor actions, if no line is
  // selected.
  if LvwResults.Selected = nil then
  begin
    ActCopySelectedLineToClipboard.Enabled := False;
    ActOpenInEditor.Enabled := False;
  end;

  // Disable the save lines to file action if there are no lines present.
  if LvwResults.Items.Count <= 0 then
  begin
    ActSaveAllLinesToFile.Enabled := False;
  end;
end; //*** end of CtxMnuResultsViewPopup ***


//***************************************************************************************
// NAME:           LvwResultsDblClick
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   None.
// DESCRIPTION:    Event handler that gets called when a double-click event occurred on
//                 the component.
//
//***************************************************************************************
procedure TMainForm.LvwResultsDblClick(Sender: TObject);
var
  relativeCursosPos: TPoint;
begin
  // Get the position of the mouse cursor relative to the list view.
  relativeCursosPos := LvwResults.ScreenToClient(Mouse.CursorPos);
  // Only process the event if the mouse was actually on an item. Otherwise the editor
  // will actually be opened if the header was double-clicked, for example for
  // automatic column width sizing.
  if LvwResults.GetItemAt(relativeCursosPos.x, relativeCursosPos.y) <> nil then
  begin
    // Attempt to open the search hit in the selected row in a text editor.
    ActOpenInEditorExecute(Sender);
  end;
end; //*** end of LvwResultsDblClick ***


//***************************************************************************************
// NAME:           LvwResultsKeyUp
// PARAMETER:      Sender Source of the event.
//                 Key The key on the keyboard. For non-alphanumeric keys, you must use
//                 virtual key codes to determine the key pressed.
//                 Shift Indicates whether the Shift, Alt, or Ctrl keys are combined with
//                 the keystroke
// RETURN VALUE:   None.
// DESCRIPTION:    Event handler that gets called when a key is released.
//
//***************************************************************************************
procedure TMainForm.LvwResultsKeyUp(Sender: TObject; var Key: Word;
                                    Shift: TShiftState);
begin
  // Suppress warning due to unused parameter.
  Shift := Shift;
  // Was the enter key pressed?
  if Key = VK_RETURN then
  begin
    // Execute the associated action to open the file in a text editor.
    ActOpenInEditorExecute(Sender);
  end;
end; //*** end of LvwResultsKeyUp ***


//***************************************************************************************
// NAME:           OnKeyUpHandlerToStartSearch
// PARAMETER:      Sender Source of the event.
//                 Key The key on the keyboard. For non-alphanumeric keys, you must use
//                 virtual key codes to determine the key pressed.
//                 Shift Indicates whether the Shift, Alt, or Ctrl keys are combined with
//                 the keystroke
// RETURN VALUE:   None.
// DESCRIPTION:    Event handler that gets called when a key is released. This generic
//                 version can be reused for all components that that should start
//                 a search operation when the enter key is released.
//
//***************************************************************************************
procedure TMainForm.OnKeyUpHandlerToStartSearch(Sender: TObject; var Key: Word;
                                                Shift: TShiftState);
begin
  // Suppress warning due to unused parameter.
  Shift := Shift;
  // Was the enter key pressed?
  if Key = VK_RETURN then
  begin
    // Theoretically we want to directly start the search operation here by calling
    // ActSearchExecute(Sender);
    // This works when the LCLWidgetType is set to the default GTK2. However, it doesn't
    // work properly when the LCLWidgetType is set to QT5. The OnKeyUp handler does
    // trigger, but the search doesn't actually start somehow.
    // A workaround to fix this problem is to start a timer and in the OnTimer event
    // start the search operation by calling ActSearchExecute(Sender);
    // So all that is left to do here is reset and start the timer.
    TimerStartSearch.Enabled := False;
    TimerStartSearch.Enabled := True;
  end;
end; //*** end of OnKeyUpHandlerToStartSearch ***


//***************************************************************************************
// NAME:           LoadCurrentConfig
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Loads the current configuration from the file and processes the loaded
//                 values.
//
//***************************************************************************************
procedure TMainForm.LoadCurrentConfig;
var
  mainWindowConfig: TMainWindowConfig;
  lastSearchConfig: TLastSearchConfig;
  textEditorConfig: TTextEditorConfig;
begin
  // Load the program's configuration from the configuration file.
  FCurrentConfig.LoadFromFile;
  // Set main window configuration settings.
  mainWindowConfig := FCurrentConfig.Groups[TMainWindowConfig.GROUP_NAME]
                      as TMainWindowConfig;
  MainForm.Width := mainWindowConfig.Width;
  MainForm.Height := mainWindowConfig.Height;
  LvwResults.Column[0].Width := mainWindowConfig.ResultsColumn0Width;
  LvwResults.Column[1].Width := mainWindowConfig.ResultsColumn1Width;
  LvwResults.Column[2].Width := mainWindowConfig.ResultsColumn2Width;
  LvwResults.Column[3].Width := mainWindowConfig.ResultsColumn3Width;
  // Set last search configuration settings.
  lastSearchConfig := FCurrentConfig.Groups[TLastSearchConfig.GROUP_NAME]
                      as TLastSearchConfig;
  FSearchSettings.Directory := lastSearchConfig.Directory;
  FSearchSettings.Recursive:= False;
  if lastSearchConfig.Recursive > 0 then
    FSearchSettings.Recursive := True;
  FSearchSettings.CaseSensitive := False;
  if lastSearchConfig.CaseSensitive > 0 then
    FSearchSettings.CaseSensitive := True;
  FSearchSettings.FilePattern := lastSearchConfig.FilePattern;
  // Set the text editor settings.
  textEditorConfig := FCurrentConfig.Groups[TTextEditorConfig.GROUP_NAME]
                      as TTextEditorConfig;
  // Automatically detect and configure the text editor?
  if textEditorConfig.AutoConfigEnabled then
  begin
    // Perform automatic detection and configuration.
    FTextEditor.Locate;
    // Update the results in the settings for persistency.
    textEditorConfig.LineNumberOptPrefix := FTextEditor.LineNumberOptPrefix;
    textEditorConfig.Editor := FTextEditor.Editor;
  end
  // Restore the text editor settings from the configuration file.
  else
  begin
    FTextEditor.LineNumberOptPrefix := textEditorConfig.LineNumberOptPrefix;
    FTextEditor.Editor := textEditorConfig.Editor;
  end;
end; //*** end of LoadCurrentConfig ***


//***************************************************************************************
// NAME:           SaveCurrentConfig
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Stores the current configuration values and saves them to the file.
//
//***************************************************************************************
procedure TMainForm.SaveCurrentConfig;
var
  mainWindowConfig: TMainWindowConfig;
begin
  // Store main window configuration settings.
  mainWindowConfig := FCurrentConfig.Groups[TMainWindowConfig.GROUP_NAME]
                      as TMainWindowConfig;
  mainWindowConfig.Width := MainForm.Width;
  mainWindowConfig.Height := MainForm.Height;
  mainWindowConfig.ResultsColumn0Width := LvwResults.Column[0].Width;
  mainWindowConfig.ResultsColumn1Width := LvwResults.Column[1].Width;
  mainWindowConfig.ResultsColumn2Width := LvwResults.Column[2].Width;
  mainWindowConfig.ResultsColumn3Width := LvwResults.Column[3].Width;
  // Save the program's configuration to the configuration file.
  FCurrentConfig.SaveToFile;
end; //*** end of SaveCurrentConfig ***


//***************************************************************************************
// NAME:           InitializeUserInterface
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Initializes the user interface.
//
//***************************************************************************************
procedure TMainForm.InitializeUserInterface;
begin
  // Initialize the user interface, based on currently set search settings.
  EdtDirectory.Text := FSearchSettings.Directory;
  CbxRecursive.Checked := FSearchSettings.Recursive;
  EdtSearchText.Text := FSearchSettings.SearchText;
  CbxCaseSensitive.Checked := FSearchSettings.CaseSensitive;
  CmbSearchPattern.Text := FSearchSettings.FilePattern;
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
    ActSearch.Caption := RsButtonCaptionCancel;
    // Make the search button the active control during a search. This makes it possible
    // for the user to cancel the search by pressing the Enter key.
    ActiveControl := BtnSearch;
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
    ActSearch.Caption := RsButtonCaptionSearch;
    // Make the search text edit the active control. This makes it possible to enter
    // a new search text right away after a search. Handy when a type was made or to
    // continue searching for different terms.
    ActiveControl := EdtSearchText;
  end;
end; //*** end of UpdateUserInterface ***


//***************************************************************************************
// NAME:           ReattachProgressbar
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Makes sure the program bar is correctly positioned on the statusbar.
//                 Statusbar panels cannot hold components such as a progressbar. The
//                 workaround is to simply reposition the progress bar whenever the size
//                 of the form changes. This routines manages that. Note that the anchors
//                 of the progressbar should all be set to False in the object inspector.
//
//***************************************************************************************
procedure TMainForm.ReattachProgressbar;
begin
  // Reposition the progress bar on panel[0] of the statusbar.
  {$IFDEF LCLqt5}
  // QT5 Widgetset shows borders around panel and makes the progress bar look misaligned.
  // Correct for this.
  PrgBarSearch.Top := StatusBar.Top + 3;
  PrgBarSearch.Left := StatusBar.Left + 2;
  PrgBarSearch.Width := StatusBar.Panels[0].Width;
  PrgBarSearch.Height := StatusBar.Height - 6;
  {$ELSE}
  PrgBarSearch.Top := StatusBar.Top + 2;
  PrgBarSearch.Left := StatusBar.Left + 7;
  PrgBarSearch.Height := StatusBar.Height - 4;
  PrgBarSearch.Width := StatusBar.Panels[0].Width - 20;
  {$ENDIF}
end; //*** end of ReattachProgressbar ***


//***************************************************************************************
// NAME:           UpdateSearchProgress
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Updates the user interface based on the latest status of the search
//                 operation.
//
//***************************************************************************************
procedure TMainForm.UpdateSearchProgress(Reset: Boolean);
var
  totalFilesFound: LongWord;
  filesSearched: LongWord;
  totalSearchHits: LongWord;
  filesWithHit: LongWord;
  progressPct: Integer;
begin
  // Initialize progress info to the reset values.
  totalFilesFound := 0;
  filesSearched := 0;
  totalSearchHits := 0;
  filesWithHit := 0;
  progressPct := 0;
  // Only update progress info values if no reset was requested and if a search operation
  // is running.
  if (not Reset) and (FUISetting = UIS_SEARCHING) then
  begin
    totalFilesFound := FTotalFilesSearchCount;
    filesSearched := FFilesSearchedCount;
    totalSearchHits := FTotalSearchHitCount;
    filesWithHit := FFilesWithHitCount;
    if totalFilesFound > 0 then
    begin
      progressPct := (filesSearched * 100) div totalFilesFound;
    end;
  end;
  PrgBarSearch.Position := progressPct;
  StatusBar.Panels[1].Text := RsStatusBarSearchHits + ': ' + IntToStr(totalSearchHits);
  StatusBar.Panels[2].Text := RsStatusBarFilesWithHits + ': ' + IntToStr(filesWithHit) +
                              ' ' + RsStatusBarConnectorText + ' ' +
                              IntToStr(totalFilesFound);
end; //*** end of UpdateSearchProgress ***


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
  FFilesSearchedCount := 0;
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
  lastSearchConfig: TLastSearchConfig;
begin
  // Store the last used search settings in the current config for persistency purposes.
  lastSearchConfig := FCurrentConfig.Groups[TLastSearchConfig.GROUP_NAME]
                      as TLastSearchConfig;
  lastSearchConfig.Directory := FSearchSettings.Directory;
  lastSearchConfig.Recursive := 0;
  if FSearchSettings.Recursive then
    lastSearchConfig.Recursive := 1;
  lastSearchConfig.CaseSensitive := 0;
  if FSearchSettings.CaseSensitive then
    lastSearchConfig.CaseSensitive := 1;
  lastSearchConfig.FilePattern := FSearchSettings.FilePattern;
  // Update the user interface.
  FUISetting := UIS_DEFAULT;
  UpdateUserInterface;
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
  Application.MessageBox(PChar(ErrorInfo), PChar(RsSearchGenericErrorTitle), boxStyle);
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
  // Reset the search progress information
  UpdateSearchProgress(True);
  // Remove all lines with search results.
  ResultsViewClearRows;
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
  // Nothing needs to be done here for now. Keep the event handler for possible future
  // new functionality.
  // Suppress warning due to unused parameter.
  Filename := Filename;
end; //*** end of AddSearchFileToResults ***


//***************************************************************************************
// NAME:           AddSearchOccurenceToResults
// PARAMETER:      LineNumber Line number in the file that the match occurred on.
//                 LineContents Contents of the line that the match occurred on.
//                 SearchedFile Filename of the file that contains the match.
// RETURN VALUE:   none
// DESCRIPTION:    Adds the detected match of the search through the file to the user
//                 interface.
//
//***************************************************************************************
procedure TMainForm.AddSearchOccurenceToResults(LineNumber: LongWord; LineContents: String; SearchedFile: String);
begin
  // Add the information as a new row in the results view.
  ResultsViewAddRow(LineContents, LineNumber, SearchedFile);
end; //*** end of AddSearchOccurenceToResults ***


//***************************************************************************************
// NAME:           ResultsViewAddRow
// PARAMETER:      LineContents Contents of the line that the match occurred on.
//                 LineNumber Line number in the file that the match occurred on.
//                 SearchedFile Filename of the file that contains the match.
// RETURN VALUE:   none
// DESCRIPTION:    Adds the detected match of the search through the file as a new row
//                 to the results list view.
//
//***************************************************************************************
procedure TMainForm.ResultsViewAddRow(LineContents: String; LineNumber: LongWord ; SearchedFile: String);
var
  fileName: String;
  dirName: String;
begin
  // Initialize local variables.
  fileName := '';
  dirName := '';
  // Split the filename into just the filename and its directory if it is valid.
  if SearchedFile <> '' then
  begin
    fileName := ExtractFileName(SearchedFile);
    dirName := ExtractFileDir(SearchedFile);
  end;
  // Add the new row to the listview.
  with LvwResults.Items.Add do
  begin
     Caption := fileName;
     if LineNumber > 0 then
     begin
       SubItems.Add(IntToStr(LineNumber));
     end
     else
     begin
        SubItems.Add('');
     end;
     SubItems.Add(LineContents);
     SubItems.Add(dirName);
     // Automatically scroll to the newly added item to make sure it is visible.
     MakeVisible(False);
  end;
end; //*** end of ResultsViewAddRow ***


//***************************************************************************************
// NAME:           ResultsViewGetRowInfo
// PARAMETER:      RowIdx Index of the row to get info from.
//                 SearchedFile Filename of the file that contains the match.
//                 LineContents Contents of the line that the match occurred on.
//                 LineNumber Line number in the file that the match occurred on.
// RETURN VALUE:   True if successful, False otherwise.
// DESCRIPTION:    Extracts information from a row with the selected index on the results
//                 list view.
//
//***************************************************************************************
function TMainForm.ResultsViewGetRowInfo(RowIdx: LongWord; var SearchedFile: String; var LineContents: String; var LineNumber: LongWord):Boolean;
begin
  // Initialize the result.
  Result := False;
  // Only continue if the specified index is valid.
  if RowIdx < LvwResults.Items.Count then
  begin
    // Extract the information.
    SearchedFile := LvwResults.Items[RowIdx].SubItems[2] + DirectorySeparator +
                    LvwResults.Items[RowIdx].Caption;
    LineContents := LvwResults.Items[RowIdx].SubItems[1];
    try
      LineNumber := StrToInt(LvwResults.Items[RowIdx].SubItems[0]);
    except
      LineNumber := 0;
    end;
    // Update the result.
    Result := True;
  end;
end; //*** end of ResultsViewGetRowInfo ***


//***************************************************************************************
// NAME:           ResultsViewAddRow
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Removes all rows with content from the results list view.
//
//***************************************************************************************
procedure TMainForm.ResultsViewClearRows;
begin
  // Clear all rows from the listview.
  LvwResults.Clear;
end; //*** end of ResultsViewClearRows ****


//***************************************************************************************
// NAME:           ConstructClipboardLine
// PARAMETER:      LineContents Contents of the line that the match occurred on.
//                 LineNumber Line number in the file that the match occurred on.
//                 SearchedFile Filename of the file that contains the match.
// RETURN VALUE:   Line formated for copying to the clipboard.
// DESCRIPTION:    Construct the information from a row in the results view to a one
//                 liner that can be copied to the clipboard (or saved to a file).
//
//***************************************************************************************
function TMainForm.ConstructClipboardLine(LineContents: String; LineNumber: LongWord; SearchedFile: String): String;
begin
  // Construct the line.
  Result := Trim(SearchedFile) + '(' + Trim(IntToStr(LineNumber)) + ') ' + Trim(LineContents);
end; //*** end of ConstructClipboardLine ***


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
// NAME:           FileContentSearcherOnFileFound
// PARAMETER:      Sender Source of the event.
//                 FoundFile Detected file.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the file content searcher
//                 found a while, while it is building a list of files to search through.
//
//***************************************************************************************
procedure TMainForm.FileContentSearcherOnFileFound(Sender: TObject; FoundFile: String);
begin
  // Suppress hint for unused parameter.
  FoundFile := FoundFile;
  // Update total files found counter.
  FTotalFilesSearchCount := FTotalFilesSearchCount + 1;
  // Update the search progress information
  UpdateSearchProgress;
end; //*** end of FileContentSearcherOnFileFound ***


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
  // Suppress hint for unused parameter.
  SearchFile := SearchFile;
  // Increment searched file counter.
  FFilesSearchedCount := FFilesSearchedCount + 1;
  // Update the search progress information
  UpdateSearchProgress;
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
  // Update the search progress information
  UpdateSearchProgress;
end; //*** end of FileContentSearcherOnFileSearchHit ***

end.
//******************************** end of mainunit.pas **********************************

