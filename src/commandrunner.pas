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
  {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    cmem, // the c memory manager is on some systems much faster for multi-threading
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, Process;


//***************************************************************************************
// Type Definitions
//***************************************************************************************
type
  // Forward declarations
  TCommandRunner = class;

  //------------------------------ TCommandRunnerStartedEvent ---------------------------
  TCommandRunnerStartedEvent = procedure(Sender: TObject) of object;

  //------------------------------ TCommandRunnerCancelledEvent -------------------------
  TCommandRunnerCancelledEvent = procedure(Sender: TObject) of object;

  //------------------------------ TCommandRunnerDoneEvent ------------------------------
  TCommandRunnerDoneEvent = procedure(Sender: TObject) of object;

  //------------------------------ TCommandRunnerThread ---------------------------------
  TCommandRunnerThread = class(TThread)
  private
   protected
     FCommand: String;
     FCommandRunner: TCommandRunner;
     procedure Execute; override;
     procedure SynchronizeDoneEvent;
    public
      constructor Create(Command: String; CreateSuspended : Boolean; CommandRunner: TCommandRunner); reintroduce;
    end;


  //------------------------------ TCommandRunner ---------------------------------------
  TCommandRunner = class (TObject)
  private
    FCommand: String;
    FOutput: TStringList;
    FWorkerThread: TCommandRunnerThread;
    FStartedEvent: TCommandRunnerStartedEvent;
    FCancelledEvent: TCommandRunnerCancelledEvent;
    FDoneEvent: TCommandRunnerDoneEvent;
    procedure SetCommand(Value: String);
    function  GetRunning: Boolean;
  public
    constructor Create;
    destructor  Destroy; override;
    function Start: Boolean;
    procedure Cancel;
    property Command: String read FCommand write SetCommand;
    property Running: Boolean read GetRunning;
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
  FStartedEvent := nil;
  FCancelledEvent := nil;
  FDoneEvent := nil;
  FWorkerThread := nil;
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
  // Check if the worker thread is instanced.
  if Assigned(FWorkerThread) then
  begin
    // Set termination request for the worker thread.
    FWorkerThread.Terminate;
    // Wait for thread termination to complete.
    FWorkerThread.WaitFor;
    // Release the thread instance.
    FWorkerThread.Free;
  end;
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
// NAME:           GetRunning
// PARAMETER:      none
// RETURN VALUE:   The value of the running property.
// DESCRIPTION:    Getter of the running property.
//
//***************************************************************************************
function  TCommandRunner.GetRunning: Boolean;
begin
  // Initialize the result value.
  Result := False;
  // Check if a worker thread was instanced.
  if Assigned(FWorkerThread) then
  begin
    // Check if the worker thread is not yet finished.
    if not FWorkerThread.Finished then
    begin
      Result := True;
    end;
  end;
end; //*** end of GetRunning ***


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
  // Only continue if a valid command is set.
  if FCommand <> '' then
  begin
    // Clear the output.
    FOutput.Clear;
    // Check if the worker thread is terminated but not yet freed from a previous update.
    if Assigned(FWorkerThread) then
    begin
      if FWorkerThread.Finished then
      begin
        // Free it.
        FreeAndNil(FWorkerThread);
      end;
    end;
    // Only start running a command if another one is not already in progress.
    if not Assigned(FWorkerThread) then
    begin
      // Create the worker thread in a suspended state.
      FWorkerThread := TCommandRunnerThread.Create(FCommand, True, Self);
      // Only continue if the worker thread could be instanced.
      if Assigned(FWorkerThread) then
      begin
        // Start the worker thread, which handles the actual command running.
        FWorkerThread.Start;
        // Update the result.
        Result := True;
        // Trigger event handler, if configured.
        if Assigned(FStartedEvent) then
        begin
          FStartedEvent(Self);
        end;
      end;
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
  // No need to stop the worker thread if it is not instanced.
  if Assigned(FWorkerThread) then
  begin
    // Set termination request for the worker thread.
    FWorkerThread.Terminate;
    // Wait for thread termination to complete.
    FWorkerThread.WaitFor;
    // Release the thread instance.
    FreeAndNil(FWorkerThread);
  end;
  // Trigger event handler, if configured.
  if Assigned(FCancelledEvent) then
  begin
    FCancelledEvent(Self);
  end;
end; //*** end of Cancel ***


//---------------------------------------------------------------------------------------
//-------------------------------- TCommandRunnerThread ---------------------------------
//---------------------------------------------------------------------------------------
//***************************************************************************************
// NAME:           Create
// PARAMETER:      Command The command that should be run by this thread.
//                 CreateSuspended True to suspend the thread after creation.
//                 CommandRunner Instance of the TCommandRunner class, needed to trigger
//                 its events.
// RETURN VALUE:   none
// DESCRIPTION:    Thread constructor.
//
//***************************************************************************************
constructor TCommandRunnerThread.Create(Command: String; CreateSuspended : Boolean; CommandRunner: TCommandRunner);
begin
  // Call inherited constructor.
  inherited Create(CreateSuspended);
  // Configure the thread to not automatically free itself upon termination.
  FreeOnTerminate := False;
  // Initialize fields.
  FCommand := Command;
  FCommandRunner := CommandRunner;
end; //*** end of Create ***


//***************************************************************************************
// NAME:           Execute
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Thread execution function.
//
//***************************************************************************************
procedure TCommandRunnerThread.Execute;
const
  BUF_SIZE = 1024;
var
  cmdProcess: TProcess;
  cmdSplitter: TStringList;
  cmdSplitterIdx: Integer;
  outputBuffer: array[0..(BUF_SIZE - 1)] of Byte;
  bytesRead: Longword;
  stringStream: TStringStream;
  idx: Integer;
begin
  // Initialize the output buffer with all zeroes.
  for idx := 0 to  (SizeOf(outputBuffer) - 1) do
  begin
    outputBuffer[idx] := 0;
  end;
  // Create process instance.
  cmdProcess := TProcess.Create(nil);
  // Create string stream instance.
  stringStream := TStringStream.Create('');
  // Enter thread's execution loop.
  while not Terminated do
  begin
    // Create string list instance.
    cmdSplitter := TStringList.Create;
    // Break the command apart into the executable and its parameters.
    CommandToList(FCommand, cmdSplitter);
    // It must have at least one entry, which is the executable.
    Assert(cmdSplitter.Count >= 1);
    // Store the executable.
    cmdProcess.Executable := cmdSplitter[0];
    // Store the parameters, if any.
    if cmdSplitter.Count > 1 then
    begin
      for cmdSplitterIdx := 1 to (cmdSplitter.Count - 1) do
      begin
        cmdProcess.Parameters.Add(cmdSplitter[cmdSplitterIdx]);
      end;
    end;
    // Release string list instance.
    cmdSplitter.Free;
    // Configure the process to use a pipe so the output of the command can be read.
    cmdProcess.Options := [poUsePipes];
    // Run the command. This will not return until it is complete.
    cmdProcess.Execute;
    // Read all output from the pipe into the buffer.
    repeat
      // Read a chunk of data from the pipe. Note that this blocks until the data is
      // available.
      bytesRead := cmdProcess.Output.Read(outputBuffer, BUF_SIZE);
      // Convert the raw data to a string stream.
      stringStream.Size := 0;
      stringStream.Write(outputBuffer, bytesRead);
      { TODO : Chop this up in lines using LineEnding and string find somehow and then
               add line per line and also invoke a new update event handler per line. }
               // NOTE: Windows \r\n and Linux \n=10=0ah
      // Store the results.
      FCommandRunner.FOutput.Text := FCommandRunner.FOutput.Text + stringStream.DataString;
      // Check for cancellation event.
      if Terminated then
      begin
        // Stop the thread..
        Break;
      end;
    until bytesRead = 0;
    // All done so no need to continue the thread.
    Break;
  end;
  // Trigger the done event, unless the thread was cancelled.
  if not Terminated then
  begin
    Synchronize(@SynchronizeDoneEvent);
  end;
  // Release instances.
  stringStream.Free;
  cmdProcess.Free;
end; //*** end of Execute ***


//***************************************************************************************
// NAME:           SynchronizeDoneEvent
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Synchronizes to the main thread to execute the code inside this
//                 procedure. This function should only be called from thread level,
//                 so from Execute-method in the following manner: Synchronize(@<name>).
//
//***************************************************************************************
procedure TCommandRunnerThread.SynchronizeDoneEvent;
begin
  // Only continue if the event is set.
  if Assigned(FCommandRunner.FDoneEvent) then
  begin
    // Trigger the event.
    FCommandRunner.FDoneEvent(FCommandRunner);
  end;
end; //*** end of SynchronizeDoneEvent ***


end.
//******************************** end of commandrunner.pas *****************************

