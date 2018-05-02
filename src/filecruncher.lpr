program filecruncher;
//***************************************************************************************
//  Description: Contains the main program entry.
//    File Name: filecruncher.lpr
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

//***************************************************************************************
// Includes
//***************************************************************************************
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    cmem, // the c memory manager is on some systems much faster for multi-threading
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, MainUnit, SearchSettings, CommandRunner, FileContentSearcher
  { you can add units after this }, SysUtils;

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
  WriteLn('Usage:');
  WriteLn('  filecruncher [OPTION]... [DIRECTORY]');
  WriteLn('');
  WriteLn('Help Options:');
  WriteLn('  -?, --help                    Show help options');
  WriteLn('');
  WriteLn('Application Options:');
  WriteLn('  -i, --ignore-case             Perform case-insensitive search');
  WriteLn('  -n, --not-recursive           Do not recurse into subdirectories');
  WriteLn('  -l, --lang=CODE               Set user interface language');
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
  CheckResult: String;
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
  CmdLineIgnoreCaseOption := False;
  CmdLineNotRecursiveOption := False;
  CmdLineDirectoryOption := '';
  // Check the supported command line options.
  CheckResult := Application.CheckOptions('?inl:', 'help ignore-case not-recursive lang:');
  // Only extract the command line options if valid ones were specified.
  if CheckResult = '' then
  begin
    // Extract ignore case option.
    if Application.HasOption('i', 'ignore-case') then
    begin
      CmdLineIgnoreCaseOption := True;
    end;
    // Extract not-recursive search option.
    if Application.HasOption('n', 'not-recursive') then
    begin
      CmdLineNotRecursiveOption := True;
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
//******************************** end of filecruncher.lpr ******************************

