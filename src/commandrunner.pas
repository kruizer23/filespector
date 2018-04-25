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
    FCommand: String;
    FRunning: Boolean;
    FOutput: TStringList;
    FStartedEvent: TCommandRunnerStartedEvent;
    FCancelledEvent: TCommandRunnerCancelledEvent;
    FDoneEvent: TCommandRunnerDoneEvent;
    procedure SetCommand(Value: String);
  public
    constructor Create;
    destructor  Destroy; override;
    function Start: Boolean;
    procedure Cancel;
    property Command: String read FCommand write SetCommand;
    property Running: Boolean read FRunning write FRunning;
    property Output: TStringList read FOutput;
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
  // Create instance of the output stringlist.
  FOutput := TStringList.Create;
  // Initialize fields to their default values.
  FCommand := '';
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
  // Free instance of the output stringlist.
  FOutput.Free;
  // Call inherited destructor.
  inherited Destroy;
end; //*** end of Destroy ***


//***************************************************************************************
// NAME:           SetCommand
// PARAMETER:      Value The command to execute when started.
// RETURN VALUE:   none
// DESCRIPTION:    Setter for the command property.
//
//***************************************************************************************
procedure TCommandRunner.SetCommand(Value: String);
begin
  // Only set the command, if it is not empty. Otherwise, set the default value.
  if Trim(Value) <> '' then
  begin
    FCommand := Value;
  end
  else
  begin
    FCommand := '';
  end;
end; //*** end of SetCommand ***


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
  // Only continue if another command is not already running and a valid command is set.
  if (not FRunning) and (FCommand <> '') then
  begin
    // Clear the output.
    FOutput.Clear;
    // Set running flag.
    FRunning := True;
    { TODO : Implement start running of the command. }
    // Update the result.
    Result := True;
    // Trigger event handler, if configured.
    if Assigned(FStartedEvent) then
    begin
      FStartedEvent(Self);
    end;
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
  // Only cancel if a command is actually running.
  if FRunning then
  begin
    { TODO : Implement cancellation of the running command. }
    // Reset running flag.
    FRunning := False;
  end;
  // Trigger event handler, if configured.
  if Assigned(FCancelledEvent) then
  begin
    FCancelledEvent(Self);
  end;
end; //*** end of Cancel ***

end.
//******************************** end of commandrunner.pas *****************************

