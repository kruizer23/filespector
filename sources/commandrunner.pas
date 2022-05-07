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
  {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    cmem, // the c memory manager is on some systems much faster for multi-threading
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, ExtCtrls, Process;


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

  //------------------------------ TCommandRunnerErrorEvent -----------------------
  TCommandRunnerErrorEvent = procedure(Sender: TObject; ErrorInfo: String) of object;

  //------------------------------ TCommandRunnerUpdateEvent ----------------------------
  TCommandRunnerUpdateEvent = procedure(Sender: TObject; OutputLine: String) of object;

  //------------------------------ TCommandRunnerThreadState ----------------------------
  TCommandRunnerThreadState = ( CRTS_IDLE = 0,
                                CRTS_BUSY );

  //------------------------------ TCommandRunnerThread ---------------------------------
  TCommandRunnerThread = class(TThread)
  private
  protected
    FState: TCommandRunnerThreadState;
    FCommand: String;
    FCommandRunner: TCommandRunner;
    FUpdateString: String;
    FErrorString: String;
    procedure Execute; override;
    procedure SynchronizeUpdateEvent;
    procedure SynchronizeDoneEvent;
    procedure SynchronizeErrorEvent;
  public
    constructor Create(CreateSuspended : Boolean; CommandRunner: TCommandRunner); reintroduce;
    property Command: String read FCommand write FCommand;
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
    FErrorEvent: TCommandRunnerErrorEvent;
    FUpdateEvent: TCommandRunnerUpdateEvent;
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
    property OnError: TCommandRunnerErrorEvent read FErrorEvent write FErrorEvent;
    property OnUpdate: TCommandRunnerUpdateEvent read FUpdateEvent write FUpdateEvent;
  end;

implementation
//***************************************************************************************
// Localization
//***************************************************************************************
resourcestring
  RsErrorExecute = 'Could not execute';


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
  // Create instance of the worker thread.
  FWorkerThread := TCommandRunnerThread.Create(True, Self);
  // Start the worker thread in idle mode if it was successfully instanced.
  if Assigned(FWorkerThread) then
  begin
    FWorkerThread.FState := CRTS_IDLE;
    FWorkerThread.Start;
  end;
  // Initialize fields to their default values.
  FCommand := '';
  FStartedEvent := nil;
  FCancelledEvent := nil;
  FDoneEvent := nil;
  FErrorEvent := nil;
  FUpdateEvent := nil;
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
    if FWorkerThread.FState <> CRTS_IDLE then
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
  // Only continue if a valid command is set and the worker thread is idling.
  if (FCommand <> '') and (FWorkerThread.FState = CRTS_IDLE) then
  begin
    // Clear the output.
    FOutput.Clear;
    // Pass the command on to the worker thread.
    FWorkerThread.Command := FCommand;
    // Kick of the worker thread. Note that it is already running. It just needs to be set
    // to busy.
    FWorkerThread.FState := CRTS_BUSY;
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
  // No need to stop the worker thread if it is not instanced.
  if Assigned(FWorkerThread) then
  begin
    // Switch the thread back to idle mode.
    FWorkerThread.FState := CRTS_IDLE;
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
constructor TCommandRunnerThread.Create(CreateSuspended : Boolean; CommandRunner: TCommandRunner);
begin
  // Call inherited constructor.
  inherited Create(CreateSuspended);
  // Configure the thread to not automatically free itself upon termination.
  FreeOnTerminate := False;
  // Initialize fields.
  FState := CRTS_IDLE;
  FCommand := '';
  FCommandRunner := CommandRunner;
  FUpdateString := '';
  FErrorString := '';
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
  BUF_SIZE = 128;
var
  cmdProcess: TProcess;
  cmdProcessExitCode: Integer;
  cmdSplitter: TStringList;
  cmdSplitterIdx: Integer;
  outputBuffer: array[0..(BUF_SIZE - 1)] of Byte;
  bytesRead: Longword;
  stringStream: TStringStream;
  idx: Integer;
  conversionStr: String;
  lineStr: String;
  lfPos: Integer;
  errorDetected: Boolean;
begin
  // Reset error flag.
  errorDetected := False;
  // Initialize the output buffer with all zeroes.
  for idx := 0 to  (SizeOf(outputBuffer) - 1) do
  begin
    outputBuffer[idx] := 0;
  end;
  // Create string stream instance.
  stringStream := TStringStream.Create('');
  // Start with an empty stream.
  stringStream.Size := 0;
  // Create string list instance.
  cmdSplitter := TStringList.Create;
  // Enter thread's execution loop.
  while not Terminated do
  begin
    if FState = CRTS_BUSY then
    begin
      // Reset error flag.
      errorDetected := False;
      // Clear the string list.
      cmdSplitter.Clear;
      // Break the command apart into the executable and its parameters.
      CommandToList(FCommand, cmdSplitter);
      // It must have at least one entry, which is the executable.
      Assert(cmdSplitter.Count >= 1);
      // Create process instance.
      cmdProcess := TProcess.Create(nil);
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
      // Configure the process to use a pipe so the output of the command can be read.
      cmdProcess.Options := [poUsePipes];
      // Attempt to start command execution. This might cause an exception if the OS
      // cannot create a fork for the process.
      try
        // Run the command.
        cmdProcess.Execute;
      except
        // Set error flag and store related info. The error event handler will be
        // triggered at a later point.
        errorDetected := True;
        FErrorString := RsErrorExecute + ': ' + FCommand + '.';
      end;
      // Only continue with reading from the pip if the command is running.
      if not errorDetected then
      begin
        // Read all output from the pipe into the buffer.
        repeat
          // Read a chunk of data from the pipe. Note that this blocks until the data is
          // available. Therefore BUF_SIZE should not be set too big.
          bytesRead := cmdProcess.Output.Read(outputBuffer, BUF_SIZE);
          // Only process the data from the pipe if there was data available.
          if bytesRead > 0 then
          begin
            // Convert the raw data to a string stream.
            stringStream.Write(outputBuffer, bytesRead);
            // Copy the read-only datastring into a temporary string for conversion purposes.
            conversionStr := stringStream.DataString;
            // Windows line endings are \r\n and Linux line endings are \n. Remove all
            // occurrences of \r to have a common base.
            conversionStr := StringReplace(conversionStr, #13, '', [rfReplaceAll]);
            // Extract all complete lines from the conversion string.
            repeat
              // Determine position of the next line feed.
              lfPos := Pos(#10, conversionStr);
              // Was a linefeed found?
              if lfPos > 0 then
              begin
                // Copy the line including the linefeed.
                lineStr := Copy(conversionStr, 1, lfPos);
                // Replace the linefeed with a string termination.
                lineStr := StringReplace(lineStr, #10, #0, [rfReplaceAll]);
                // Remove the line from the conversion string now that it is copied.
                Delete(conversionStr, 1, lfPos);
                // Add the line to the command output.
                FCommandRunner.FOutput.Add(lineStr);
                // Store the line such that it can be used in the synchronized update event.
                FUpdateString := lineStr;
                // Trigger the update event (if set) to pass on the newly read line from the
                // pipe.
                if Assigned(FCommandRunner.FUpdateEvent) then
                begin
                  Synchronize(@SynchronizeUpdateEvent);
                end;
              end;
            until lfPos = 0;
            // Empty string stream now that its data has been processed.
            stringStream.Size := 0;
            // Check if there is a partial line left in the conversion string.
            if Length(conversionStr) > 0 then
            begin
              // Place the remainder back in the string stream so it will be processed during
              // the next data read from the pipe.
              stringStream.WriteString(conversionStr);
            end;
          end;
          // Check for termination and cancellation event.
          if (Terminated) or (FState = CRTS_IDLE) then
          begin
            Break;
          end;
        until bytesRead = 0;
      end;
      // Properly reap the child process. Otherwise it will remain as a zombie and we
      // might run out of process handles. Note that WaitOnExit cannot be used here,
      // because the loop that reads from the pipe might have been exited upon
      // termination or cancellation request, while the command is still running. Calling
      // WaitOnExit here would deadlock the thread, since the program won't exit if we
      // don't empty out the pipe.
      if cmdProcess.Running then
      begin
        cmdProcessExitCode := 0;
        cmdProcess.Terminate(cmdProcessExitCode);
      end;
      // Release the process instance.
      cmdProcess.Free;
      // All done so its time to transition to the idle state.
      FState := CRTS_IDLE;
      // Trigger the OnDone event in case no error was detected.
      if not errorDetected then
      begin
        Synchronize(@SynchronizeDoneEvent);
      end
      // Trigger the OnError event otherwise.
      else
      begin
        Synchronize(@SynchronizeErrorEvent);
      end;
    end
    else if FState = CRTS_IDLE then
    begin
      // Check for cancellation event.
      if Terminated then
      begin
        // Stop the thread.
        Break;
      end;
      // Don't starve the CPU while idling.
      Sleep(2);
    end;
  end;
  // Release instances.
  cmdSplitter.Free;
  stringStream.Free;
end; //*** end of Execute ***


//***************************************************************************************
// NAME:           SynchronizeUpdateEvent
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Synchronizes to the main thread to execute the code inside this
//                 procedure. This function should only be called from thread level,
//                 so from Execute-method in the following manner: Synchronize(@<name>).
//
//***************************************************************************************
procedure TCommandRunnerThread.SynchronizeUpdateEvent;
begin
  // Only trigger the event if set.
  if Assigned(FCommandRunner.FUpdateEvent) then
  begin
    // Trigger the event.
    FCommandRunner.FUpdateEvent(FCommandRunner, FUpdateString);
  end;
end; //*** end of SynchronizeUpdateEvent ***


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
  // Only trigger the event if set.
  if Assigned(FCommandRunner.FDoneEvent) then
  begin
    // Trigger the event.
    FCommandRunner.FDoneEvent(FCommandRunner);
  end;
end; //*** end of SynchronizeDoneEvent ***


//***************************************************************************************
// NAME:           SynchronizeErrorEvent
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Synchronizes to the main thread to execute the code inside this
//                 procedure. This function should only be called from thread level,
//                 so from Execute-method in the following manner: Synchronize(@<name>).
//
//***************************************************************************************
procedure TCommandRunnerThread.SynchronizeErrorEvent;
begin
  // Only trigger the event if set.
  if Assigned(FCommandRunner.FErrorEvent) then
  begin
    // Trigger the event.
    FCommandRunner.FErrorEvent(FCommandRunner, FErrorString);
  end;
end; //*** end of SynchronizeUpdateEvent ***

end.
//******************************** end of commandrunner.pas *****************************

