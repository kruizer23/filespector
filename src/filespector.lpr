program filespector;
//***************************************************************************************
//  Description: Contains the main program entry.
//    File Name: filespector.lpr
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

//***************************************************************************************
// Includes
//***************************************************************************************
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    cmem, // the c memory manager is on some systems much faster for multi-threading
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainUnit, SearchSettings, CommandRunner, FileContentSearcher,
  TextEditor
  { you can add units after this }, SysUtils, VersionInfo;

{$R *.res}

//***************************************************************************************
// NAME:           WriteHelp
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Displays the application's help information on the standard output.
//
//***************************************************************************************
procedure WriteHelp;
begin
  WriteLn('Usage: filespector [OPTIONS] [DIRECTORY]');
  WriteLn('');
  WriteLn('Options:');
  WriteLn('  -?             --help                    Show help options');
  WriteLn('  -i [off|on]    --ignore-case[=off|on]    Ignore case during search');
  WriteLn('  -r [off|on]    --recursive[=off|on]      Recurse into directories');
  WriteLn('  -l [code]      --lang[=code]             Set user interface language');
  WriteLn('');
  WriteLn('FileSpector is a GUI tool for quickly finding all text occurrences in multiple');
  WriteLn('files in a directory, matching a specific file extension pattern.');
  WriteLn('');
  WriteLn('Examples:');
  WriteLn('  filespector -i on -r off /home/user');
  WriteLn('  filespector --recursive=on --lang=de /home/user');
  WriteLn('');
end; //*** end of WriteHelp ***


//***************************************************************************************
// NAME:           ProcessCommandLine
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Checks and extracts all relevant command line options.
//
//***************************************************************************************
procedure ProcessCommandLine;
var
  checkResult: String;
  optionValue: String;
begin
  // Display help information if requested.
  if Application.HasOption('?', 'help') then
  begin
    // Display application's help information.
    WriteHelp;
    // Stop the program.
    halt;
  end;
  // Initialize command line option related global vars.
  CmdLineIgnoreCaseOptionFound := False;
  CmdLineIgnoreCaseOption := False;
  CmdLineRecursiveOptionFound := False;
  CmdLineRecursiveOption := False;
  CmdLineDirectoryOption := '';
  // Check the supported command line options.
  checkResult := Application.CheckOptions('?i:r:l:', 'help ignore-case: recursive: lang:');
  // Only extract the command line options if valid ones were specified.
  if checkResult = '' then
  begin
    // Extract ignore-case option.
    if Application.HasOption('i', 'ignore-case') then
    begin
      // Extract the option value.
      optionValue := Application.GetOptionValue('i', 'ignore-case');
      if optionValue = 'off' then
      begin
        CmdLineIgnoreCaseOptionFound := True;
        CmdLineIgnoreCaseOption := False;
      end
      else if optionValue = 'on' then
      begin
        CmdLineIgnoreCaseOptionFound := True;
        CmdLineIgnoreCaseOption := True;
      end;
    end;
    // Extract recursive search option.
    if Application.HasOption('r', 'recursive') then
    begin
      // Extract the option value.
      optionValue := Application.GetOptionValue('r', 'recursive');
      if optionValue = 'off' then
      begin
        CmdLineRecursiveOptionFound := True;
        CmdLineRecursiveOption := False;
      end
      else if optionValue = 'on' then
      begin
        CmdLineRecursiveOptionFound := True;
        CmdLineRecursiveOption := True;
      end;
    end;
    // Extract initial search directory.
    if ParamCount > 0 then
    begin
      // Check if the last specified option is an existing directory.
      if DirectoryExists(ParamStr(ParamCount)) then
      begin
        CmdLineDirectoryOption := ParamStr(ParamCount);
      end;
    end;
  end;
end; //*** end of ProcessCommandLine ***


//***************************************************************************************
// NAME:           Main
// PARAMETER:      none
// RETURN VALUE:   none
// DESCRIPTION:    Program entry point.
//
//***************************************************************************************
begin
  // Set flag to specify that every form must have a resource.
  RequireDerivedFormResource:=True;
  // Initialize the application.
  Application.Initialize;
  // Process the command line.
  ProcessCommandLine;
  // Create the main form of the application.
  Application.CreateForm(TMainForm, MainForm);
  // Start running the application.
  Application.Run;
end.
//******************************** end of filespector.lpr *******************************

