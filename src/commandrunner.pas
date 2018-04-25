unit CommandRunner;
//***************************************************************************************
//  Description: Implements classes and functionality for running external programs in
//               the background and capturing their output.
//    File Name: commandrunner.pas
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
  Classes, SysUtils;


//***************************************************************************************
// Type Definitions
//***************************************************************************************
type
  //------------------------------ TCommandRunnerStartedEvent ---------------------------
  TCommandRunnerStartedEvent = procedure(Sender: TObject) of object;

  //------------------------------ TCommandRunnerCancelledEvent -------------------------
  TCommandRunnerCancelledEvent = procedure(Sender: TObject) of object;

  //------------------------------ TCommandRunnerDoneEvent ------------------------------
  TCommandRunnerDoneEvent = procedure(Sender: TObject) of object;

  //------------------------------ TCommandRunner ---------------------------------------
  TCommandRunner = class (TObject)
  private
    FRunning: Boolean;
    FStartedEvent: TCommandRunnerStartedEvent;
    FCancelledEvent: TCommandRunnerCancelledEvent;
    FDoneEvent: TCommandRunnerDoneEvent;
  public
    constructor Create;
    destructor  Destroy; override;
    function Start: Boolean;
    procedure Cancel;
    property Running: Boolean read FRunning write FRunning;
    property OnStarted: TCommandRunnerStartedEvent read FStartedEvent write FStartedEvent;
    property OnCancelled: TCommandRunnerCancelledEvent read FCancelledEvent write FCancelledEvent;
    property OnDone: TCommandRunnerDoneEvent read FDoneEvent write FDoneEvent;
  end;

implementation
//---------------------------------------------------------------------------------------
//-------------------------------- TCommandRunner ---------------------------------------
//---------------------------------------------------------------------------------------
//***************************************************************************************
// NAME:           Create
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Class constructor.
//
//***************************************************************************************
constructor TCommandRunner.Create;
begin
  // Call inherited constructor.
  inherited Create;
  // Initialize fields to their default values.
  FRunning := False;
  FStartedEvent := nil;
  FCancelledEvent := nil;
  FDoneEvent := nil;
end; //*** end of Create ***


//***************************************************************************************
// NAME:           Destroy
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Class destructor.
//
//***************************************************************************************
destructor TCommandRunner.Destroy;
begin
  { TODO : Release local instances and other general cleanup. }
  // Call inherited destructor.
  inherited Destroy;
end; //*** end of Destroy ***


//***************************************************************************************
// NAME:           Start
// PARAMETER:      none
// RETURN VALUE:   True if successful, False otherwise.
// DESCRIPTION:    Starts running the command.
//
//***************************************************************************************
function TCommandRunner.Start: Boolean;
begin
  // Initialize the result.
  Result := False;
  // Only continue if another command is not already running.
  if not FRunning then
  begin
    { TODO : Implement start running of the command. }
  end;
end; //*** end of Start ***


//***************************************************************************************
// NAME:           Cancel
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Cancels a currently running command.
//
//***************************************************************************************
procedure TCommandRunner.Cancel;
begin
  // Only continue if a command is actually running.
  if FRunning then
  begin
    { TODO : Implement cancellation of the running command. }
  end;
end; //*** end of Cancel ***

end.
//******************************** end of commandrunner.pas *****************************

