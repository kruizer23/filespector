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
  Classes, SysUtils, CommandRunner, SearchSettings;

type
  // Forward declarations
  TFileContentSearcher = class;

  //------------------------------ TFileContentSearcherStartedEvent ---------------------
  TFileContentSearcherStartedEvent = procedure(Sender: TObject) of object;

  //------------------------------ TFileContentSearcherCancelledEvent -------------------
  TFileContentSearcherCancelledEvent = procedure(Sender: TObject) of object;

  //------------------------------ TFileContentSearcherDoneEvent ------------------------
  TFileContentSearcherDoneEvent = procedure(Sender: TObject) of object;

  //------------------------------ TFileContentSearcherFileFoundEvent -------------------
  TFileContentSearcherFileFoundEvent = procedure(Sender: TObject; FoundFile: String) of object;

  //------------------------------ TFileContentSearcherFileSearchStartedEvent -----------
  TFileContentSearcherFileSearchStartedEvent = procedure(Sender: TObject; SearchFile: String) of object;

  //------------------------------ TFileContentSearcherFileSearchHitEvent ---------------
  TFileContentSearcherFileSearchHitEvent = procedure(Sender: TObject; SearchFile: String; HitLine: String) of object;


  //------------------------------ TFileContentSearcher ---------------------------------
  TFileContentSearcher = class (TObject)
  private
    FSearchSettings: TSearchSettings;
    FCommandRunner: TCommandRunner;
    FStartedEvent: TFileContentSearcherStartedEvent;
    FCancelledEvent: TFileContentSearcherCancelledEvent;
    FDoneEvent: TFileContentSearcherDoneEvent;
    FFileFoundEvent: TFileContentSearcherFileFoundEvent;
    FFileSearchStartedEvent: TFileContentSearcherFileSearchStartedEvent;
    FFileSearchHitEvent: TFileContentSearcherFileSearchHitEvent;
  public
    constructor Create;
    destructor  Destroy; override;
    function  Start(SearchSettings: TSearchSettings): Boolean;
    procedure Cancel;
    property OnStarted: TFileContentSearcherStartedEvent read FStartedEvent write FStartedEvent;
    property OnCancelled: TFileContentSearcherCancelledEvent read FCancelledEvent write FCancelledEvent;
    property OnDone: TFileContentSearcherDoneEvent read FDoneEvent write FDoneEvent;
    property OnFileFound: TFileContentSearcherFileFoundEvent read FFileFoundEvent write FFileFoundEvent;
    property OnFileSearchStarted: TFileContentSearcherFileSearchStartedEvent read FFileSearchStartedEvent write FFileSearchStartedEvent;
    property OnFileSearchHit: TFileContentSearcherFileSearchHitEvent read FFileSearchHitEvent write FFileSearchHitEvent;
  end;


implementation
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
  FSearchSettings := nil;
  FStartedEvent := nil;
  FCancelledEvent := nil;
  FDoneEvent := nil;
  FFileFoundEvent := nil;
  FFileSearchStartedEvent := nil;
  FFileSearchHitEvent := nil;
  // Create instance of the command runner.
  FCommandRunner := TCommandRunner.Create;
  // Create instance of the search settings.
  FSearchSettings := TSearchSettings.Create;
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
  { TODO : Implement start functionality. }
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
  { TODO : Implement cancel functionality. }
end; //*** end of Cancel ***

end.
//******************************** end of filecontentsearcher.pas ***********************

