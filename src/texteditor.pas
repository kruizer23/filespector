unit TextEditor;
//***************************************************************************************
//  Description: Implements a class for opening files with a GUI text editor under Linux.
//    File Name: texteditor.pas
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
  Classes, SysUtils, LazFileUtils, Process;


//***************************************************************************************
// Type Definitions
//***************************************************************************************
type
  //------------------------------ TTextEditor ------------------------------------------
  TTextEditor = class (TObject)
  private
    FEditor: String;
    FLineNumberOptPrefix: String;
    procedure Locate;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Open(TextFile: String =  ''; LineNumber: LongWord = 0);
    property Editor: String read FEditor;
  end;

implementation
//---------------------------------------------------------------------------------------
//-------------------------------- TTextEditor ------------------------------------------
//---------------------------------------------------------------------------------------
//***************************************************************************************
// NAME:           Create
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Class constructor.
//
//***************************************************************************************
constructor TTextEditor.Create;
begin
  // Call inherited constructor.
  inherited Create;
  // Initialize fields to their default values.
  FEditor := '';
  FLineNumberOptPrefix := '';
  // Attempt to locate the text editor on the system.
  Locate;
end; //*** end of Create ***


//***************************************************************************************
// NAME:           Destroy
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Class destructor.
//
//***************************************************************************************
destructor TTextEditor.Destroy;
begin
  // Call inherited destructor.
  inherited Destroy;
end; //*** end of Destroy ***


//***************************************************************************************
// NAME:           Locate
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Attempts to locate the default text editor on the system.
//
//***************************************************************************************
procedure TTextEditor.Locate;
const
  NUM_EDITORS = 7;
  knownEditors: array[1..2, 1..NUM_EDITORS] of String = (
    // first column contains the executable name.
    ('gedit', 'kate',  'kwrite', 'xed', 'pluma', 'leafpad', 'mousepad'),
    // second column contain the command line option prefix for jumping to a linenumber.
    ('+'    , '-l ' ,  '-l '   , '+'  , '+'    , '--jump=', '')
  );
var
  idx: Integer;
  cmdOutput: String;
begin
  // Loop through all known editors to see if this once is installed on the system.
  for idx := 1 to NUM_EDITORS do
  begin
    if RunCommand('which', [knownEditors[1][idx]], cmdOutput) then
    begin
      // The output might have \n and/or \r characters. Remove them first.
      cmdOutput := StringReplace(cmdOutput, #13, '', [rfReplaceAll]);
      cmdOutput := StringReplace(cmdOutput, #10, '', [rfReplaceAll]);
      // The expected result is a pathname without spaces.
      if (Pos(DirectorySeparator, cmdOutput) = 1) and (Pos(' ', cmdOutput) = 0) then
      begin
        // It should also be an existing file.
        if FileExists(cmdOutput) then
        begin
          if FileIsExecutable(cmdOutput) and FileIsReadable(cmdOutput) then
          begin
            // Store the executable with path info.
            FEditor := cmdOutput;
            // Store the command line option prefix for opening at a specific line number
            FLineNumberOptPrefix := knownEditors[2][idx];
            // No point in continuing the loop.
            Break;
          end;
        end;
      end;
    end;
  end;
end; //*** end of Locate ***


//***************************************************************************************
// NAME:           Open
// PARAMETER:      TextFile The file to open.
//                 LineNumber The linenumber to jump to.
// RETURN VALUE:   none
// DESCRIPTION:    Opens the text editor. A file to open and a line number to jump to can
//                 optionally be specified.
//
//***************************************************************************************
procedure TTextEditor.Open(TextFile: String; LineNumber: LongWord);
var
  runProgram: TProcess;
  fileToOpen: String;
  lineToJumpTo: LongWord;
begin
  // Initialize the line to jump to.
  lineToJumpTo := LineNumber;
  // Initialize file to open to an invalid value.
  fileToOpen := '';
  // Validate the specified file. It should exists and be readable.
  if TextFile <> '' then
  begin
    if (FileExists(TextFile)) and (FileIsReadable(TextFile)) then
    begin
      fileToOpen := TextFile;
    end;
  end;
  // Create process instance.
  runProgram := TProcess.Create(nil);
  // Set the text editor executable. If none was found, fallback to xdg-open.
  runProgram.Executable := FEditor;
  if FEditor = '' then
  begin
    // The xdg-open method requires a filename parameter.
    if fileToOpen <> '' then
    begin
      // xdg-open opens a file or URL in the user's preferred application. Since the
      // actual program is unknown, the command line option for jumping to a specific
      // line is not known and cannot be used. Disable this feature by setting the
      // linenumber to 0. Thi is also the reason that xdg-open is only used as a fall-
      // back method.
      runProgram.Executable := 'xdg-open';
      lineToJumpTo := 0;
    end;
  end;
  // Add parameters for linenumber to jump to and the file to open, if a valid file was
  // specified.
  if fileToOpen <> '' then
  begin
    if (lineToJumpTo > 0) and (FLineNumberOptPrefix <> '') then
    begin
      runProgram.Parameters.Add(FLineNumberOptPrefix + IntToStr(lineToJumpTo));
    end;
    // Add the text file.
    runProgram.Parameters.Add(fileToOpen);
  end;
  // Open the text editor, if a valid executable was set.
  if runProgram.Executable <> '' then
  begin
    runProgram.Execute;
  end;
  // Releas the process instance.
  runProgram.Free;
end; //*** end of Open ***/

end.
//******************************** end of texteditor.pas ********************************

