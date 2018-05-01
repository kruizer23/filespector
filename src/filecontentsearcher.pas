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
    FFileList: TStrings;
    FStartedEvent: TFileContentSearcherStartedEvent;
    FCancelledEvent: TFileContentSearcherCancelledEvent;
    FDoneEvent: TFileContentSearcherDoneEvent;
    FErrorEvent: TFileContentSearcherErrorEvent;
    FFileFoundEvent: TFileContentSearcherFileFoundEvent;
    FFileSearchStartedEvent: TFileContentSearcherFileSearchStartedEvent;
    FFileSearchHitEvent: TFileContentSearcherFileSearchHitEvent;
    function StartFileDetection: Boolean;
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
{ TODO : Note that the directories and filenames with spaces in them should be in double-
         quotes if it is added as a command parameter.  }
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
// RETURN VALUE:   none
// DESCRIPTION:    Configures and starts the running the command for building a list of
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
    end;
  end;
  // Release the list.
  patternList.Free;
end; //*** end of StartFileDetection ***


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
  if FState = FCSS_BUILDING_FILE_LIST then
  begin
    { TODO : Switch to the FCSS_SEARCHING_FILE and kick it off. }
    { TODO : Remove temporary test code once done with it. }
    // Set state back to idle.
    FState := FCSS_IDLE;
    // Trigger event handler, if configured.
    if Assigned(FDoneEvent) then
    begin
      FDoneEvent(Self);
    end;
  end
  else if FState = FCSS_SEARCHING_FILE then
  begin
    { TODO : Implement OnDone handler. }
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
begin
  // Handle the event based on the internal state.
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
  else if FState = FCSS_SEARCHING_FILE then
  begin
    { TODO : Implement OnUpdate handler. }
  end;
end; //*** end of CommandRunnerOnUpdate ***

end.
//******************************** end of filecontentsearcher.pas ***********************

