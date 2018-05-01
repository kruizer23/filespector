unit FileContentSearcher;
//***************************************************************************************
//  Description: Implements classes and functionality for searching through the content
//               of files.
//    File Name: filecontentsearcher.pas
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
  {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    cmem, // the c memory manager is on some systems much faster for multi-threading
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, StrUtils, CommandRunner, SearchSettings;

type
  // Forward declarations
  TFileContentSearcher = class;

  //------------------------------ TFirmwareUpdateState ---------------------------------
  TFileContentSearcherState = ( FCSS_IDLE = 0,
                                FCSS_BUILDING_FILE_LIST,
                                FCSS_SEARCHING_FILE,
                                FCSS_FINISHING_UP );

  //------------------------------ TFileContentSearcherStartedEvent ---------------------
  TFileContentSearcherStartedEvent = procedure(Sender: TObject) of object;

  //------------------------------ TFileContentSearcherCancelledEvent -------------------
  TFileContentSearcherCancelledEvent = procedure(Sender: TObject) of object;

  //------------------------------ TFileContentSearcherDoneEvent ------------------------
  TFileContentSearcherDoneEvent = procedure(Sender: TObject) of object;

  //------------------------------ TFileContentSearcherErrorEvent -----------------------
  TFileContentSearcherErrorEvent = procedure(Sender: TObject; ErrorInfo: String) of object;

  //------------------------------ TFileContentSearcherFileFoundEvent -------------------
  TFileContentSearcherFileFoundEvent = procedure(Sender: TObject; FoundFile: String) of object;

  //------------------------------ TFileContentSearcherFileSearchStartedEvent -----------
  TFileContentSearcherFileSearchStartedEvent = procedure(Sender: TObject; SearchFile: String) of object;

  //------------------------------ TFileContentSearcherFileSearchHitEvent ---------------
  TFileContentSearcherFileSearchHitEvent = procedure(Sender: TObject; SearchFile: String; HitLine: String; LineNumber: Longword) of object;

  //------------------------------ TFileContentSearcher ---------------------------------
  TFileContentSearcher = class (TObject)
  private
    FState: TFileContentSearcherState;
    FSearchSettings: TSearchSettings;
    FCommandRunner: TCommandRunner;
    FFileList: TStringList;
    FCurrentFileIdx: Integer;
    FStartedEvent: TFileContentSearcherStartedEvent;
    FCancelledEvent: TFileContentSearcherCancelledEvent;
    FDoneEvent: TFileContentSearcherDoneEvent;
    FErrorEvent: TFileContentSearcherErrorEvent;
    FFileFoundEvent: TFileContentSearcherFileFoundEvent;
    FFileSearchStartedEvent: TFileContentSearcherFileSearchStartedEvent;
    FFileSearchHitEvent: TFileContentSearcherFileSearchHitEvent;
    function StartFileDetection: Boolean;
    function StartFileSearching(ListIdx: Integer): Boolean;
    procedure CommandRunnerOnDone(Sender: TObject);
    procedure CommandRunnerOnUpdate(Sender: TObject; OutputLine: String);
  public
    constructor Create;
    destructor  Destroy; override;
    function  Start(SearchSettings: TSearchSettings): Boolean;
    procedure Cancel;
    property OnStarted: TFileContentSearcherStartedEvent read FStartedEvent write FStartedEvent;
    property OnCancelled: TFileContentSearcherCancelledEvent read FCancelledEvent write FCancelledEvent;
    property OnDone: TFileContentSearcherDoneEvent read FDoneEvent write FDoneEvent;
    property OnError: TFileContentSearcherErrorEvent read FErrorEvent write FErrorEvent;
    property OnFileFound: TFileContentSearcherFileFoundEvent read FFileFoundEvent write FFileFoundEvent;
    property OnFileSearchStarted: TFileContentSearcherFileSearchStartedEvent read FFileSearchStartedEvent write FFileSearchStartedEvent;
    property OnFileSearchHit: TFileContentSearcherFileSearchHitEvent read FFileSearchHitEvent write FFileSearchHitEvent;
  end;


implementation
{ TODO : After cancelling a long search operation from the GUI, starting a next one
         doesn't seem to find anything. It just looks like it is working but no hits
         come in. Needs debugging.
         * Actually, it is the long search that seems to hang. I tried it on
           ~/Development/*.* for text FormCreate. A few hits come in and then nothing
           else happens.
}
//***************************************************************************************
// NAME:           StringListFilenameCompare
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Utility function that can be used as a custom sort routine on a
//                 TStringList if it contains a list of files.
//
//***************************************************************************************
function StringListFilenameCompare(List: TStringList; Index1, Index2: Integer): Integer;
var
  dir1: String;
  dir2: String;
  file1: String;
  file2: String;
begin
  // Initialize the result.
  Result := 0;
  // Only continue if both entries are actually valid files.
  if (FileExists(List[Index1])) and (FileExists(List[Index2])) then
  begin
    // Extract directory and filenames.
    dir1 := ExtractFilePath(List[Index1]);
    dir2 := ExtractFilePath(List[Index2]);
    file1 := ExtractFileName(List[Index1]);
    file2 := ExtractFileName(List[Index2]);
    // Check if files are in the same directory.
    if dir1 = dir2 then
    begin
      // Compare based on filename.
      Result := CompareStr(file1, file2);
    end
    // File are in different directories.
    else
    begin
      // Compare based on directory.
      Result := CompareStr(dir1, dir2);
    end;
  end;
end;  //*** end of StringListFilenameCompare ***


//---------------------------------------------------------------------------------------
//-------------------------------- TFileContentSearcher ---------------------------------
//---------------------------------------------------------------------------------------
//***************************************************************************************
// NAME:           Create
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Class constructor.
//
//***************************************************************************************
constructor TFileContentSearcher.Create;
begin
  // Call inherited constructor.
  inherited Create;
  // Initialize fields.
  FState := FCSS_IDLE;
  FCurrentFileIdx := 0;
  FSearchSettings := nil;
  FStartedEvent := nil;
  FCancelledEvent := nil;
  FDoneEvent := nil;
  FErrorEvent := nil;
  FFileFoundEvent := nil;
  FFileSearchStartedEvent := nil;
  FFileSearchHitEvent := nil;
  // Create instance of the command runner.
  FCommandRunner := TCommandRunner.Create;
  // Configure the command runner event handlers.
  FCommandRunner.OnDone := @CommandRunnerOnDone;
  FCommandRunner.OnUpdate :=@CommandRunnerOnUpdate;
  // Create instance of the search settings.
  FSearchSettings := TSearchSettings.Create;
  // Create instance of the file list.
  FFileList := TStringList.Create;
end; //*** end of Create ***


//***************************************************************************************
// NAME:           Destroy
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Class destructor.
//
//***************************************************************************************
destructor TFileContentSearcher.Destroy;
begin
  // Release file list instance.
  FFileList.Free;
  // Release the search settings instance.
  FSearchSettings.Free;
  // Release command runner instance.
  FCommandRunner.Free;
  // Call inherited destructor.
  inherited Destroy;
end; //*** end of Destroy ***


//***************************************************************************************
// NAME:           Start
// PARAMETER:      SearchSettings Settings for the search operation.
// RETURN VALUE:   True if successful, False otherwise.
// DESCRIPTION:    Starts the search operation by constructing a file list and searching
//                 through the contents of each file for a match of the search text.
//
//***************************************************************************************
function TFileContentSearcher.Start(SearchSettings: TSearchSettings): Boolean;
begin
  // Initialize the result.
  Result := False;
  // Store the search settings.
  FSearchSettings.Directory := SearchSettings.Directory;
  FSearchSettings.Recursive := SearchSettings.Recursive;
  FSearchSettings.SearchText := SearchSettings.SearchText;
  FSearchSettings.CaseSensitive := SearchSettings.CaseSensitive;
  FSearchSettings.FilePattern := SearchSettings.FilePattern;
  // Validate search settings before actually starting the search operation.
  if (Trim(FSearchSettings.SearchText) <> '') and
     (Trim(FSearchSettings.Directory) <> '')  then
  begin
    // Kick of the search operation by building a list of files that need to be searched.
    Result := StartFileDetection;
  end;
end; //*** end of Start ***


//***************************************************************************************
// NAME:           Cancel
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Cancels a currently running command.
//
//***************************************************************************************
procedure TFileContentSearcher.Cancel;
begin
  // Cancel the command runner.
  FCommandRunner.Cancel;
  // Set idle state.
  FState := FCSS_IDLE;
  // Trigger event handler, if configured.
  if Assigned(FCancelledEvent) then
  begin
    FCancelledEvent(Self);
  end;
end; //*** end of Cancel ***


//***************************************************************************************
// NAME:           StartFileDetection
// PARAMETER:      none
// RETURN VALUE:   True if successful, False otherwise.
// DESCRIPTION:    Configures and starts running the command for building a list of
//                 files that need to be searched.
//
//***************************************************************************************
function TFileContentSearcher.StartFileDetection: Boolean;
var
  command: String;
  recursive: String;
  directory: String;
  patternList: TStringList;
  patternIdx: Integer;
begin
  // Initialize the result.
  Result := False;
  // Empty out the file list.
  FFileList.Clear;
  // Set option to configure recursiveness of the search operation.
  recursive := '';
  if not FSearchSettings.Recursive then
    recursive := ' -maxdepth 1';
  // Add double-quotes around the base directory, if it contains spaces.
  directory := FSearchSettings.Directory;
  if Pos(' ', directory) > 0 then
    directory := '"' + directory + '"';
  // Build string list with file extensions that should be searched.
  patternList := TStringList.Create;
  try
    patternList.Delimiter := '|';
    patternList.DelimitedText := DelSpace(FSearchSettings.FilePattern);
  except
    // Split operation failed. Resort to a default pattern for all file extensions.
    patternList.Clear;
    patternList.Add('*.*');
  end;
  // Only continue if at least one pattern is in the list.
  if patternList.Count > 0 then
  begin
    // Check all patterns. They should always start with '*.' and be at least 3 characters
    // long.
    for patternIdx := 0 to (patternList.Count - 1) do
    begin
      if (Pos('*.', patternList[patternIdx]) <> 1) or (Length(patternList[patternIdx]) < 3) then
      begin
        // Invalid pattern detected. Resort to a default pattern for all file extensions.
        // Split operation failed. Resort to a default pattern for all file extensions.
        patternList.Clear;
        patternList.Add('*.*');
        // Stop looping.
        Break;
      end;
    end;
    // Construct the command and its parameters.
    command := 'find ' + directory + recursive + ' -mount -readable -type f';
    // Add the patterns.
    for patternIdx := 0 to (patternList.Count - 1) do
    begin
      if patternIdx = 0 then
        command := command + ' -name "' + patternList[patternIdx] + '"'
      else
        command := command + ' -o -name "' + patternList[patternIdx] + '"';
    end;
    // Set the command.
    FCommandRunner.Command := command;
    // Update the state and result.
    FState := FCSS_BUILDING_FILE_LIST;
    Result := True;
    // Start the command.
    if not FCommandRunner.Start then
    begin
      // Update the state and result.
      FState := FCSS_IDLE;
      Result := False;
    end
    // Command successfully started.
    else
    begin
      // Trigger event handler, if configured.
      if Assigned(FStartedEvent) then
      begin
        FStartedEvent(Self);
      end;
    end;
  end;
  // Release the list.
  patternList.Free;
end; //*** end of StartFileDetection ***


//***************************************************************************************
// NAME:           StartFileSearching
// PARAMETER:      none
// RETURN VALUE:   True if successful, False otherwise.
// DESCRIPTION:    Configures and starts the running the command for searching the file
//                 at the specified index.
//
//***************************************************************************************
function TFileContentSearcher.StartFileSearching(ListIdx: Integer): Boolean;
var
  command: String;
  caseSensitive: String;
  searchFile: String;
begin
  // Initialize the result.
  Result := False;
  // Validate list index.
  Assert(ListIdx >= 0);
  Assert(ListIdx < FFileList.Count);
  // Set the filename.
  searchFile := FFileList[ListIdx];
  // Only continue if the file actually exists.
  if FileExists(searchFile) then
  begin
    // Add double-quotes around the file name, if it contains spaces.
    if Pos(' ', searchFile) > 0 then
      searchFile := '"' + searchFile + '"';
    // Set option to configure recursiveness of the search operation.
    caseSensitive := '';
    if not FSearchSettings.CaseSensitive then
      caseSensitive := ' -i';
    // Construct the command and its parameters.
    command := 'grep -I -n' + caseSensitive + ' "' + FSearchSettings.SearchText + '" ' +
               searchFile;
    // Set the command.
    FCommandRunner.Command := command;
    // Update the result.
    Result := True;
    // Start the command.
    if not FCommandRunner.Start then
    begin
      // Update the state and result.
      FState := FCSS_IDLE;
      Result := False;
    end
    else
    begin
      // Trigger event handler, if configured.
      if Assigned(FFileSearchStartedEvent) then
      begin
        FFileSearchStartedEvent(Self, FFileList[ListIdx]);
      end;
    end;
  end;
end; //*** end of StartFileSearching ***


//***************************************************************************************
// NAME:           CommandRunnerOnDone
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the command runner completed
//                 successfully.
//
//***************************************************************************************
procedure TFileContentSearcher.CommandRunnerOnDone(Sender: TObject);
begin
  // Handle the event based on the internal state.
  // ---------------------  FCSS_BUILDING_FILE_LIST -------------------------------------
  if FState = FCSS_BUILDING_FILE_LIST then
  begin
    // Transition to the file searching state if files were found.
    if FFileList.Count > 0 then
    begin
      // Sort the file list contents based on the both directory and filename.
      FFileList.CustomSort(@StringListFilenameCompare);
      // Prepare file searching state by setting the index to the first file in the list.
      FCurrentFileIdx := 0;
      // Transition to the new state.
      FState := FCSS_SEARCHING_FILE;
      // Kick of the file searching starting iwth the first one in the list.
      if not StartFileSearching(FCurrentFileIdx) then
      begin
        // Could not start the file search. Go back to idle state and report the error.
        FState := FCSS_IDLE;
        // Trigger event handler, if configured.
        if Assigned(FErrorEvent) then
        begin
          FErrorEvent(Self, 'Could not start the search operation for file ' +
                      ExtractFileName(FFileList[FCurrentFileIdx]) + '.');
        end;
      end;
    end
    else
    begin
      // No files found so all is done.
      FState := FCSS_IDLE;
      // Trigger event handler, if configured.
      if Assigned(FDoneEvent) then
      begin
        FDoneEvent(Self);
      end;
    end;
  end
  // ---------------------  FCSS_SEARCHING_FILE -----------------------------------------
  else if FState = FCSS_SEARCHING_FILE then
  begin
    // Increment the current file indexer.
    FCurrentFileIdx := FCurrentFileIdx + 1;
    // Are there still files left to search?
    if FCurrentFileIdx < FFileList.Count then
    begin
      // Kick of the file searching starting iwth the first one in the list.
      if not StartFileSearching(FCurrentFileIdx) then
      begin
        // Could not start the file search. Go back to idle state and report the error.
        FState := FCSS_IDLE;
        // Trigger event handler, if configured.
        if Assigned(FErrorEvent) then
        begin
          FErrorEvent(Self, 'Could not start the search operation for file ' +
                      ExtractFileName(FFileList[FCurrentFileIdx]) + '.');
        end;
      end;
    end
    // All done so finish up the search.
    else
    begin
      // Set state back to idle.
      FState := FCSS_IDLE;
      // Trigger event handler, if configured.
      if Assigned(FDoneEvent) then
      begin
        FDoneEvent(Self);
      end;
    end;
  end;
end; //*** end of CommandRunnerOnDone ***


//***************************************************************************************
// NAME:           CommandRunnerOnUpdate
// PARAMETER:      Sender Source of the event.
// RETURN VALUE:   none
// DESCRIPTION:    Event handler that gets called when the command runner detected a new
//                 line on the standard output.
//
//***************************************************************************************
procedure TFileContentSearcher.CommandRunnerOnUpdate(Sender: TObject; OutputLine: String);
var
  searchFile: String;
  hitLine: String;
  lineNumber: Longword;
  lineNumberStr: String;
  colonPos: Longword;
begin
  // Handle the event based on the internal state.
  // ---------------------  FCSS_BUILDING_FILE_LIST -------------------------------------
  if FState = FCSS_BUILDING_FILE_LIST then
  begin
    // The expected output is a filename. Verify this by checking the existance of it.
    if FileExists(OutputLine) then
    begin
      // Add it to the internal file list.
      FFileList.Add(OutputLine);
      // Trigger event handler, if configured.
      if Assigned(FFileFoundEvent) then
      begin
        FFileFoundEvent(Self, OutputLine);
      end;
    end;
  end
  // ---------------------  FCSS_SEARCHING_FILE -----------------------------------------
  else if FState = FCSS_SEARCHING_FILE then
  begin
    // The expected output is expected to have the following format:
    //  <linenumber>:<linecontents>.
    // Verify that the collon is present.
    colonPos := Pos(':', LowerCase(OutputLine));
    if (colonPos > 1) and (colonPos <= 11) then
    begin
      // Collect info regarding the search hit.
      searchFile := FFileList[FCurrentFileIdx];
      // For now copy the entire output line to the hitline. The linenumber part will be
      // removed later on.
      hitLine := OutputLine;
      // Copy the part up to but excluding the first colon, which should then start with
      // the linenumber.
      lineNumberStr := Copy(hitLine, 1, colonPos - 1);
      // Remove the same part from the hitline but now including the color, so the
      // hitline actually contains what it is supposed to contain.
      Delete(hitLine, 1, colonPos);
      // Attempt to convert the contents of lineNumberStr to an actual line number
      // integer value.
      try
        lineNumber := StrToInt(lineNumberStr);
      except
        on Exception : EConvertError do
          // Invalidate the hitline simply by setting it to empty.
          hitLine := '';
      end;
      // Only continue if hitline is valid.
      if hitLine <> '' then
      begin
        // Trigger event handler, if configured.
        if Assigned(FFileSearchHitEvent) then
        begin
          FFileSearchHitEvent(Self, searchFile, hitLine, lineNumber);
        end;
      end;
    end;
  end;
end; //*** end of CommandRunnerOnUpdate ***

end.
//******************************** end of filecontentsearcher.pas ***********************

